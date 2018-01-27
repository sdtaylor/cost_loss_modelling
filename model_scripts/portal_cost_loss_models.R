library(tidyverse)

population_thresholds = c(5,10,20,40)

predictions = read_csv('initial_model_output/portal_model_output.csv') %>%
  filter(model %in% c('tsglm'))

# #Use replicate_month to order the timeseries within each forecast replicate
# predictions = predictions %>%
#   group_by(initial_month, model, species) %>%
#   arrange(project_month) %>%
#   mutate(replicate_month = 1:12) %>%
#   ungroup() %>%
#   select(-month, -year)

###########################################
# Calculate predictions for each grain size for later comparison

###########################################

#The per month 
treatment_cost = 10
possible_loss_costs = 10 / seq(0.11, 0.99, 0.01)

#forecast outcomes given binary predictions
outcomes = data.frame(observed_binary=c(0,1,1,0),
                      predicted_binary=c(1,1,0,0),
                      type=c('fp','tp','fn','tn'))


#Gives cost per yr in terms of prescribed treatment vs actual losses
calculate_expense = function(df, treatment_cost, loss_cost, expense_type){
  if(expense_type=='perfect'){
    df$predicted_binary = df$observed_binary
  } else if(expense_type=='always') {
    df$predicted_binary = 1
  } else if(expense_type=='never') {
    df$predicted_binary = 0
  } else if(expense_type=='forecast'){
    #No corrections done here
  } else {
    stop('No forecast type')
  }
  
  df = df %>%
    left_join(outcomes, by=c('observed_binary','predicted_binary')) %>%
    ungroup()
  
  total_tp_fp_months = sum(df$type %in% c('fp','tp'))
  total_fn_months     = sum(df$type == 'fn')
  total_expense_from_treatment_costs = treatment_cost * total_tp_fp_months
  total_expense_from_loss_costs = loss_cost * total_fn_months
  
  total_expense = total_expense_from_loss_costs + total_expense_from_treatment_costs
  
  total_replicates = df %>%
    select(initial_month, project_month) %>%
    distinct() %>%
    nrow()
  
  average_per_month_expense = total_expense / total_replicates
  return(average_per_month_expense)  
}


###############################################################################
#Calculate cost/loss model curves
cost_loss_values=data.frame()

for(this_species in unique(predictions$species)){
  for(this_threshold in population_thresholds){
    
    predictions_binary = predictions %>%
      filter(species==this_species) %>%
      mutate(observed_binary = (num_rodents < this_threshold)*1,
             predicted_binary= (prediction < this_threshold)*1) %>%
      select(project_month, initial_month, observed_binary, predicted_binary, model)
    
    for(this_loss_cost in possible_loss_costs){
      expense_perfect = calculate_expense(predictions_binary, treatment_cost = treatment_cost, 
                                              loss_cost = this_loss_cost, expense_type='perfect')
      expense_never = calculate_expense(predictions_binary, treatment_cost = treatment_cost, 
                                                 loss_cost = this_loss_cost, expense_type='never')
      expense_max = min(treatment_cost, expense_never)

      forecast_expense = calculate_expense(predictions_binary, treatment_cost = treatment_cost, 
                                            loss_cost = this_loss_cost, expense_type='forecast')
        
        cost_loss_values = cost_loss_values %>%
          bind_rows(data.frame('a' = treatment_cost / this_loss_cost,
                               'expense_max' = expense_max,
                               'expense_perfect' = expense_perfect, 
                               'expense_forecast' = forecast_expense,
                               'species' = this_species,
                               'threshold' = this_threshold))
      
    }
  }
}
cost_loss_values$value = with(cost_loss_values, (expense_max - expense_forecast)/(expense_max - expense_perfect))


write_csv(cost_loss_values, 'results/portal_cost_loss_values.csv')
