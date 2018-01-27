#Uncomment these two lines to install the required packages
#install.packages(c('tidyverse','devtools'))
#devtools::install_github('weecology/portalr')
library(portalr)
library(tidyverse)
library(tscount)


model_output_file = 'intermediate_results/portal_model_output.csv'

years_to_use  = c(1995:2016)
testing_years = c(2000:2016)
testing_months = 276:467

species_of_interest = c('DM','DO','PP','PE','PB')
########################################################################
########################################################################
#The portalr package is made to deal specifically with this dataset.
#This function will download the most up to date data into the 
#current project working directory
#portalr::download_observations(base_folder = 'raw_data')

########################################################################
########################################################################

rodent_counts = portalr::abundance(path='raw_data', level='Site', unknowns = TRUE, incomplete = FALSE,
                                 shape='flat') %>%
  group_by(period, species) %>%
  summarize(num_rodents = sum(abundance)) %>%
  ungroup() %>%
  filter(species %in% species_of_interest)

#There are ~24 species or rodents seen at the site. In the raw data above
#species are only identified by a 2 letter code. This data.frame will associate
#the 2 letter code with the full names as well as other info
species_info = read.csv('raw_data/PortalData/Rodents/Portal_rodent_species.csv')

# This data is organized by periods. Period 1 is the first trapping session
# in 1977. The period is incrimented by 1 every month trapping is done. 
# Information about all the periods, such as dates and which of 24 plots were
# sampled, is in this data.frame
trapping_info = read.csv('raw_data/PortalData/Rodents/Portal_rodent_trapping.csv')


# Some trapping periods start on the last day of the month, so they'll span
# two months if you summarize naively. This assigns each period to a single 
# month,year
period_months = trapping_info %>%
  group_by(period) %>%
  summarize(month = min(month), year=min(year))

# Add in the non-naive year,month 
rodent_counts = rodent_counts %>%
  left_join(period_months, by='period')

#
# 2 trappings in a single calendar month? Average the two into one
#
rodent_counts = rodent_counts %>%
  group_by(year, month, species) %>%
  summarise(num_rodents = as.integer(mean(num_rodents))) %>%
  ungroup()
  
#
# Interpolate missing sampling periods with a spline smoother
#
all_months = expand.grid(year=years_to_use, month=1:12)

interpolate_missing_values = function(df){
  #Use this data.frame of all months/years to make na values for missing ones. 
  df = df %>%
    right_join(all_months, by=c('year','month'))
  
  #Ensure there are no NA species values
  df$species = unique(df$species)[1]
  
  #Use a continuous project_month variable, starting with 1 = the first sampling
  #to do spline fitting
  df$project_month = with(df, (year-1977)*12 + month-1)
  
  #Order matters for fitting the spline
  df = arrange(df, project_month)
  
  smoother = with(filter(df, !is.na(num_rodents)), smooth.spline(x=project_month, y=num_rodents))
  
  df$interpolated_num_rodents = predict(smoother, min(df$project_month):max(df$project_month))$y
  
  df = df %>%
    mutate(num_rodents = ifelse(is.na(num_rodents), interpolated_num_rodents, num_rodents)) %>%
    mutate(num_rodents = as.integer(num_rodents)) %>%
    select(-interpolated_num_rodents)
  
}

rodent_counts = rodent_counts %>%
  group_by(species) %>%
  do(interpolate_missing_values(.))




rm(all_months, smoother)

########################################################################
# A naive model of monthly rodents using poisson monthly averages,

make_seasonal_average_model = function(df){

  model_predictions = df %>%
    group_by(month) %>%
    summarise(prediction = mean(num_rodents)) %>%
    ungroup()
    
  model_predictions$upper_estimate = with(model_predictions, prediction + (1.96*sqrt(prediction)))
  model_predictions$lower_estimate = with(model_predictions, prediction - (1.96*sqrt(prediction)))
  
  return(model_predictions)
}


########################################################################
########################################################################

predictions = data.frame()

for(this_testing_month in testing_months){
  for(this_species in species_of_interest){
    
    this_subset_testing_data = rodent_counts %>%
      filter(project_month %in% this_testing_month:(this_testing_month+11), species==this_species) 
    
    this_subset_training_data = rodent_counts %>%
      filter(project_month < this_testing_month, species==this_species)
    
    # Make a forecast using the seasonal average model
    # the right_join works for this only when exactly 12 months are forecasted
    seasonal_avg_predictions = this_subset_training_data %>%
      make_seasonal_average_model() %>%
      right_join(this_subset_testing_data, by='month') %>%
      mutate(model = 'season_avg')
    
    #this_subset_testing_data$prediction = this_subset_testing_data$num_rodents + round(rnorm(12, mean=0, sd=50),0)
    model = tsglm(this_subset_training_data$num_rodents, model=list(past_obs=1, past_mean=6), distr = 'nbinom')
    
    prediction = predict(model, 12, level=0.95)
    
    tsglm_model_prediction = this_subset_testing_data 
    tsglm_model_prediction$prediction = prediction$pred
    tsglm_model_prediction$lower_estimate = prediction$interval[,1]
    tsglm_model_prediction$upper_estimate = prediction$interval[,2]
    tsglm_model_prediction$model='tsglm'
    
    seasonal_avg_predictions$initial_month = this_testing_month -1
    tsglm_model_prediction$initial_month = this_testing_month -1
    
    predictions = predictions %>%
      bind_rows(seasonal_avg_predictions, tsglm_model_prediction)
    
  }
}

predictions %>%
  group_by(species, model) %>%
  summarise(sqrt(mean((num_rodents - prediction)^2)))

write_csv(predictions, model_output_file)

