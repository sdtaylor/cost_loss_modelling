library(tidyverse)
library(doParallel)
library(gbm)

####################################################################################
#Adjustments based on where this script is being run.
#################################################################################
#If running this on hipergator, use "Rscript <thisScript> hipergator" 
#if running locally use 'Rscript <thisScript> local'
#if running in rstudio nothing special is needed. 

args=commandArgs(trailingOnly = TRUE)

#If the 1st argument is na (ie, no argument), then this script is being run inside rstudio
if(is.na(args[1])){
  print('Running locally (probably rstudio)')
  numProcs=2
} else if(args[1]=='local') {
  print('Running locally (probably cli)')
  dataFolder='~/data/bbs/'
  numProcs=2
} else if(args[1]=='hipergator') {
  print('Running on hipergator')
  numProcs=16
}

####################################################################
#Configuration
####################################################################
source('model_scripts/bbs/config.R')
source('model_scripts/bbs/utils.R')

#################################################################
#Data loading and pre-processing
################################################################

counts=read_csv(paste(dataFolder, 'BBS_counts.csv', sep=''))
routes=read_csv(paste(dataFolder, 'BBS_routes.csv', sep='')) %>%
  dplyr::mutate(siteID=paste(countrynum, statenum, route,sep='-')) %>%
  dplyr::select(siteID, long=loni, lat=lati)
species=read_csv(paste(dataFolder, 'BBS_species.csv', sep=''))
weather=read_csv(paste(dataFolder, 'BBS_weather.csv', sep='')) %>%
  mutate(siteID=paste(countrynum, statenum, route,sep='-')) %>%
  dplyr::select(siteID,Year=year, RPID=rpid,runtype)
  
#Spatial route object to use where needed
routes_spatial = SpatialPointsDataFrame(cbind(routes$long, routes$lat), data=routes, 
                                proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

#Some records are of genus only and "unidentified". Get rid of those.
unidSpp=species %>%
  dplyr::select(Aou=AOU, name=english_common_name) %>%
  mutate(unID=ifelse(str_sub(name, 1,5)=='unid.', 1,0)  ) %>%
  filter(unID==1) %>%
  pull('Aou')

#Filter weather to years of study so it can be used to calculate occData next.
weather=weather %>%
  filter(Year %in% c(testing_years, training_years))

occData=counts %>%
  filter(Year %in% c(testing_years, training_years)) %>%
  mutate(siteID=paste(countrynum, statenum, Route,sep='-')) %>%
  dplyr::select(Aou, siteID,year=Year, RPID)

#Remove anything not identified to species
occData=occData %>%
  filter(!Aou %in% unidSpp)

#Remove waterbirds and nocturnal birds.
occData=occData %>%
  filter(!Aou <=2880, !(Aou >=3650 & Aou<=3810), !(Aou>=3900 & Aou <=3910), !(Aou>=4160 & Aou <=4210), Aou!=7010)

#Remove any data where weather was not suitable or did not follow standard
#bbs protocal.
occData=occData %>%
  left_join(weather, by=c('siteID','year'='Year','RPID')) %>%
  filter(runtype==1, RPID==101) %>%
  dplyr::select(-runtype, -RPID)

#A list of sites and the years they were sampled. To be used in making absences for 
#species occurances. Keep only sites with the minium specified number of years in the
#10 year period.
all_routes_surveyed = occData %>%
  dplyr::select(year, siteID) %>%
  distinct() %>%
  mutate(timeframe = ifelse(year %in% training_years, 'training','testing')) %>%
  group_by(timeframe, siteID) %>%
  summarize(num_years_sampled = n()) %>%
  ungroup() %>%
  filter(num_years_sampled>=minimum_years) %>%
  dplyr::select(-num_years_sampled)

#Aggregate the  occurances down to presence in training/testing periods only
occData = occData %>%
  mutate(timeframe = ifelse(year %in% training_years, 'training','testing')) %>%
  dplyr::select(Aou, siteID, timeframe) %>%
  distinct()
  
#bioclim values for all bbs routes from PRISM data.
bioclim_data=get_bioclim_data()

#Some sites have na bioclim values. Mostly canada sites and the occasional one on water. 
bioclim_data = bioclim_data %>%
  filter(!is.na(bio1))

#Only keep data for sites that were actually surveyed
bioclim_data = bioclim_data %>%
  filter(siteID %in% all_routes_surveyed$siteID)

#Split into train and test and aggregate
bioclim_data_training = bioclim_data %>%
  dplyr::filter(year %in% training_years) %>%
  dplyr::select(-year) %>%
  group_by(siteID) %>%
  summarise_all(mean) %>%
  ungroup()

bioclim_data_testing = bioclim_data %>%
  dplyr::filter(year %in% testing_years) %>%
  dplyr::select(-year) %>%
  group_by(siteID) %>%
  summarise_all(mean) %>%
  ungroup()


###################################################################
#Setup parallel processing
####################################################################
cl=makeCluster(numProcs)
registerDoParallel(cl)

#####################################################################
#Iterate thru spp, building SDM's for each windowsize, offset, and model.
#Parallel processing happens over the ~250 species
####################################################################
focal_spp=c(7360, #Carolina chickadee
           6010, #painted bunting
            #3100, #wild turky
            4100 #Golden-fronted Woodpecker
)

#finalDF=foreach(thisSpp=unique(occData$Aou), .combine=rbind, .packages=c('dplyr','tidyr','magrittr','gbm')) %dopar% {
finalDF=foreach(thisSpp=focal_spp, .combine=rbind, .packages=c('dplyr','tidyr','magrittr','gbm')) %dopar% {
  this_spp_results=data.frame()
  
  thisSpp_occurances = occData %>%
    dplyr::filter(Aou==thisSpp) %>%
    dplyr::mutate(presence=1)
  
  thisSpp_data_training = thisSpp_occurances %>%
    filter(timeframe == 'training') %>%
    dplyr::right_join(bioclim_data_training, by = 'siteID') %>%
    dplyr::mutate(presence = ifelse(is.na(presence), 0, presence))
  
  #Skip rare species that end up with a low sample size after all the  filtering
  if(sum(thisSpp_data_training$presence)<100){
    return(data.frame())
  }
  
  #Leave out 20% for model threshold selection 
  calibration_samples = caret::createDataPartition(thisSpp_data_training$presence, p = 0.2, list = FALSE, times = 1)
  
  model=gbm(modelFormula, n.trees=5000, distribution = 'bernoulli', interaction.depth = 4, shrinkage=0.001, 
            data= thisSpp_data_training[-calibration_samples,])
  perf=gbm.perf(model, plot.it=FALSE)
  #model = glm(modelFormula, family='binomial', data=thisSpp_data_training)
  #model=step(model, direction='both')
  
  # Get the  threshold for presence based on the held 20% held out training data
  thisSpp_data_training$prediction = predict(model, n.trees = perf, newdata = thisSpp_data_training, type='response')
  threshold = with(thisSpp_data_training[calibration_samples,], get_threshold(presence, prediction))
  
  # Make predictions on the testing data
  thisSpp_data_testing = thisSpp_occurances %>%
    filter(timeframe == 'testing') %>%
    dplyr::right_join(bioclim_data_testing, by = 'siteID') %>%
    dplyr::mutate(presence = ifelse(is.na(presence), 0, presence))

  this_spp_predictions = thisSpp_data_testing %>%
    dplyr::select(siteID, presence) 
  
  this_spp_predictions$prediction = predict(model, n.trees = perf, newdata=thisSpp_data_testing, type='response')
  this_spp_predictions$prediction = (predictions$prediction > threshold) * 1

  this_spp_predictions$Aou=thisSpp
  return(this_spp_predictions)
}

write_csv(finalDF, initial_results_file)


