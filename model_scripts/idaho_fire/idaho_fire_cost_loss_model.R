library(raster)
library(tidyverse)

#The per year per cell costs
treatment_cost = 10
possible_loss_costs = 10 / seq(0.01, 0.99, 0.02)

testing_years = 2008:2013

traditional_metrics_file = 'results/idaho_fire_metrics.csv'
cost_loss_value_file = 'results/idaho_fire_cost_loss_results.csv'
#############################################################

forecast_rasters = list.files('./initial_model_output/idaho_fire/', pattern='*.tif', full.names = TRUE)
observation_rasters = list.files('./raw_data/idaho_fire/testing', pattern='*.tif', full.names = TRUE)
forecast_rasters = forecast_rasters[!grepl('xml', forecast_rasters)]
observation_rasters = observation_rasters[!grepl('xml', observation_rasters)]


forecasts = raster::stack(forecast_rasters)
observations = raster::stack(observation_rasters)

# changes layer names from 2008_prediction -> 2008 and get a list of them
for(i in 1:length(forecasts@layers)){
  forecasts@layers[[i]]@data@names =  stringr::word(forecasts@layers[[i]]@data@names,1,1,sep='_')
  observations@layers[[i]]@data@names =  stringr::word(observations@layers[[i]]@data@names,1,1,sep='_')
}

#Total 500x500m cells. Rows * columns * years
total_cells = prod(dim(forecasts))

#Convert to yes or no for class 10
is_class_10 = function(x){(x==10)*1}
#forecasts = raster::calc(forecasts, is_class_10)
observations = raster::calc(observations, is_class_10)


########################################################################
#Calculate the fn cost of two raster data sets
false_negative_costs = function(forecast, observation, L){
  fn = (as.vector(observation)==1 & as.vector(forecast)==0)
  total_loss_area = sum(fn)
  total_loss = total_loss_area * L
  return(total_loss)
}

#############################################################################
#Get csv of predictions/observations at all scales
# 
# scaled_data = data.frame()
# for(this_spatial_scale in spatial_scales){
#   for(this_temporal_scale in temporal_scales){
#     if(this_spatial_scale > 1){
#       forecasts_upscaled = raster::aggregate(forecasts, fact=this_spatial_scale, fun=max)
#       #Resample so it retains the original dimensions and cell numbers
#       forecasts_upscaled = raster::resample(forecasts_upscaled, forecasts, method='ngb')
#     } else {
#       forecasts_upscaled = forecasts
#     }
#     
#     if(this_temporal_scale > 1){
#       forecasts_upscaled    = aggregate_temporally(forecasts_upscaled, fact = this_temporal_scale, keep_original_layer_count = FALSE)
#     } 
#     
#     this_scale_data = data.frame(observed = as.vector(observations),
#                                  predicted = as.vector(forecasts_upscaled))
#     
#     this_scale_data$spatial_scale = this_spatial_scale
#     this_scale_data$temporal_scale = this_temporal_scale
#     this_scale_data$cell_id = 1:nrow(this_scale_data)
#     
#     scaled_data = scaled_data %>%
#       bind_rows(this_scale_data)
#     
#   }
# }
# 
# write_csv(scaled_data, scaled_data_file)

###############################################################################
#Calculate cost/loss model curve
cost_loss_values=data.frame()
for(loss_cost in possible_loss_costs){
  for(year in testing_years){
    year_layer = paste0('X',year)
    # Get a binary prediction map based on this cost/loss ratio
    a = treatment_cost/loss_cost
    forecasts_binary = raster::calc(forecasts[[year_layer]], function(x, threshold){(x>=a)*1})
    
    expense_perfect = (sum(as.vector(observations[[year_layer]])) * treatment_cost) / total_cells
    expense_never = (sum(as.vector(observations[[year_layer]])) * loss_cost) / total_cells
    expense_maximimum = min(treatment_cost, expense_never)
    
    forecast_treatment_cost = (sum(as.vector(forecasts_binary)) * treatment_cost)
    forecast_fn_cost = false_negative_costs(forecasts_binary, observations[[year_layer]], loss_cost)
    forecast_expense = (forecast_treatment_cost + forecast_fn_cost) / total_cells
    
    cost_loss_values = cost_loss_values %>%
      bind_rows(data.frame('a' = treatment_cost / loss_cost,
                           'year' = year,
                           'expense_max' = expense_maximimum,
                           'expense_perfect' = expense_perfect, 
                           'expense_forecast' = forecast_expense))
  }
}

cost_loss_values$value = with(cost_loss_values, (expense_max - expense_forecast) / (expense_max - expense_perfect))

ggplot(cost_loss_values, aes(x=year, y=value, color=a, group=a)) + 
  ylim(0,1) +
  geom_line(size=1.5)+
  scale_linetype_manual(values = c('solid','dashed','dotted')) +
  theme(plot.title = element_text(size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20),
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.key.width = unit(35, units = 'mm')) +
  labs(title = "Idaho Fire Cost Loss Analysis") 

ggplot(cost_loss_values, aes(x=a, y=value)) + 
  ylim(-10,1) +
  xlim(0,1) +
  geom_line(size=1.5)+
  scale_linetype_manual(values = c('solid','dashed','dotted')) +
  theme(plot.title = element_text(size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20),
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.key.width = unit(35, units = 'mm')) +
  facet_grid(~year)+
  labs(title = "Idaho Fire Cost Loss Analysis") 

write_csv(cost_loss_values, cost_loss_value_file)
