library(tidyverse)
source('model_scripts/bbs/config.R')
source('model_scripts/bbs/utils.R')

site_level_predictions = read_csv(initial_results_file)

#forecast outcomes given binary predictions
outcomes = data.frame(presence=c(0,1,1,0),
                      prediction=c(1,1,0,0),
                      type=c('fp','tp','fn','tn'))

calculate_cost = function(df, treatment_cost, loss_cost, threshold=0, expense_type){
  if(expense_type=='perfect'){
    df$prediction = df$presence
  } else if(expense_type=='always') {
    df$prediction = 1
  } else if(expense_type=='never') {
    df$prediction = 0
  } else if(expense_type=='forecast'){
    #Nothing to do
  } else {
    stop('No forecast type')
  }
  
  df = df %>%
    left_join(outcomes, by=c('presence','prediction'))
  
  total_tp_fp_routes = sum(df$type %in% c('fp','tp')) 
  total_fn_routes      = sum(df$type == 'fn') 
  total_cost = (treatment_cost * total_tp_fp_routes) + (loss_cost * total_fn_routes)
  
  total_routes = length(unique(df$siteID))

  return(total_cost/total_routes)  
}

#######################################################################################

treatment_cost = 10
possible_loss_costs = treatment_cost / seq(0.01, 1, 0.01)

value_scores = data.frame()
model_stats = data.frame()

for(this_aou in unique(site_level_predictions$Aou)){
#for(this_aou in c(3870, 4090, 4650, 4740, 3200)){
  this_spp_predictions = site_level_predictions %>%
    filter(Aou==this_aou)
  
  #Save some metrics about the model
  # prevalence = round(mean(this_spp_predictions$presence),2)
  # spec = with(this_spp_predictions, specificity(presence, binary_prediction))
  # sens = with(this_spp_predictions, sensitivity(presence, binary_prediction))
  # model_stats = model_stats %>%
  #   bind_rows(data.frame('Aou' = this_aou, prevalence = prevalence, mss = spec+sens))
  
  #loss_cost and threshold here do not matter when expense_type is perfect or always
  expense_perfect = calculate_cost(this_spp_predictions, treatment_cost = treatment_cost,
                                                  loss_cost=0, threshold=0, expense_type='perfect')
  expense_always = calculate_cost(this_spp_predictions, treatment_cost = treatment_cost,
                                                  loss_cost=0, threshold=0, expense_type='always')

  for(this_loss_cost in possible_loss_costs){
    for(threshold_type in c('max_specificity_sensitivity_base_probability','max_specificity_sensitivity_calibrated_probability',
                            'cl_ratio_base_probability','cl_ratio_calibrated_probability')){
      a = treatment_cost/this_loss_cost
      
      # Set the threshold for presencse as either the maxmimization of specificity/sensitivity (calculated in the modeling building script)
      # or as the cost/loss ration (a).
      # Thre prediction column is references in the caluclate_cost() function above. 
      if(threshold_type == 'max_specificity_sensitivity_base_probability') {
        this_spp_predictions$prediction = this_spp_predictions$base_binary_prediction
      } else if(threshold_type == 'max_specificity_sensitivity_calibrated_probability') {
        this_spp_predictions$prediction = this_spp_predictions$calibrated_binary_prediction
      } else if(threshold_type == 'cl_ratio_base_probability') {
        this_spp_predictions$prediction = 1 * (this_spp_predictions$base_probability >= a)
      } else if(threshold_type == 'cl_ratio_calibrated_probability') {
        this_spp_predictions$prediction = 1 * (this_spp_predictions$calibrated_probability >= a)
      }
      
    
      expense_never = calculate_cost(this_spp_predictions, treatment_cost = treatment_cost,
                                     loss_cost=this_loss_cost, threshold=0, expense_type='never')
      expense_forecast = calculate_cost(this_spp_predictions, treatment_cost = treatment_cost, loss_cost = this_loss_cost,
                                        threshold=a, expense_type = 'forecast')
      
      value_scores = value_scores %>%
        bind_rows(data.frame('Aou'=this_aou, 'a'=a, threshold = a, threshold_type = threshold_type,
                             'expense_forecast' = expense_forecast, 'expense_perfect'=expense_perfect, 
                             'expense_always'= expense_always, 'expense_never'=expense_never))
    }
  }
}


# model_stats$prevalence = round(model_stats$prevalence, 2)
# model_stats$mss = round(model_stats$mss,2)

value_scores$expense_max = with(value_scores, pmin(expense_always, expense_never))
value_scores$value = with(value_scores, (expense_max - expense_forecast)/(expense_max - expense_perfect))


write_csv(value_scores, 'results/bbs_cost_loss_results.csv')
