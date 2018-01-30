library(tidyverse)
library(cowplot)

#Hand made toy data for figure 2
example_data = read.csv('results/example_figure_data.csv') 

#Loss cost model for toy example in figure 2
treatment_cost = 10
possible_loss_costs = 10 / seq(0.11, 1, 0.01)
total_area = 16

values = data.frame()

for(loss_cost in possible_loss_costs){
  expense_perfect = ( sum(example_data$presence)  * treatment_cost) / total_area
  expense_never = ( sum(example_data$presence)  * loss_cost) / total_area
  expense_maximum = min(expense_never, treatment_cost)
  
  tp_fp_expense = sum(example_data$forecast) * treatment_cost
  fn_expense = sum(example_data$presence==1 & example_data$forecast==0) * loss_cost
  forecast_expense = (fn_expense + tp_fp_expense) / total_area
  
  a = treatment_cost/loss_cost
  
  values = values %>%
    bind_rows(data.frame(expense_forecast = forecast_expense, 
                         expense_max = expense_maximum,
                         expense_perfect = expense_perfect, 
                         a=a))
}

values$value = with(values, (expense_max - expense_forecast) / (expense_max - expense_perfect))

########################################################################################
# Plot the spatial forecast grids at 2 grain sizes

example_data = example_data %>%
  mutate(forecast_text = ifelse(forecast==1, 'Forecast to be present','Forecast to be absent'),
         presence_text = ifelse(presence==1, 'Actually present','Actually absent'))

grid_plot= ggplot(example_data, aes(x,y)) +
                geom_tile(aes(fill=forecast_text), color='black', size=0.8, alpha=0.3) + 
                geom_point(aes(color=presence_text), size=4) +
                scale_fill_manual(values = c('#D55E00','#009E73')) + 
                scale_color_manual(values = c('#D55E00','#009E73')) +
                theme(axis.line = element_blank(), 
                      axis.ticks = element_line(colour = NA), 
                      panel.grid.major = element_line(colour = NA), 
                      panel.grid.minor = element_line(colour = NA), 
                      axis.title = element_text(colour = NA), 
                      axis.text = element_text(colour = NA), 
                      #plot.title = element_text(colour = NA),
                      panel.background = element_rect(fill = NA), 
                      plot.background = element_rect(colour = NA),
                      legend.title = element_text(color=NA),
                      legend.text = element_text(size=10),
                      legend.key.size = unit(5, units = 'mm'),
                      legend.position = c(0.1,0.001),
                      legend.direction = 'horizontal',
                      legend.box = 'horizontal',
                      legend.spacing = unit(10, units = 'mm')) +
                guides(color = guide_legend(label.position = 'left', direction = 'vertical'),
                       fill = guide_legend(label.position = 'left', direction = 'vertical'))
print(grid_plot)


value_plot = ggplot(values, aes(x=a, y=value)) +
              geom_line(size=3.0, alpha=0.8) +
              ylim(0,1) +
              scale_x_continuous(breaks = seq(0,1,0.1)) + 
              theme_bw()+
              labs(linetype='Grain',
                   x='a = C/L',
                   y='Value') +
              theme(#axis.line = element_blank(), 
                    #axis.ticks = element_line(colour = NA), 
                    panel.grid.major = element_line(colour = NA), 
                    panel.grid.minor = element_line(colour = NA), 
                    axis.title = element_text(size=25), 
                    axis.text = element_text(size=16), 
                    #plot.title = element_text(colour = NA),
                    panel.background = element_rect(fill = NA), 
                    plot.background = element_rect(colour = NA),
                    legend.position = 'none',
                    legend.direction = 'horizontal',
                    legend.title = element_text(size=20),
                    legend.key.width = unit(30, units='mm'),
                    legend.text = element_text(size=20), 
                    plot.margin = unit(c(0,0,0,0), units = 'cm'))

################################################
# Put them together

plot_labels = c('A: Spatial Forecast',
                'B: Value Curve')

main_plots = plot_grid(grid_plot , 
                       value_plot + theme(legend.position = 'none'),
                       nrow=1, align = 'v', scale=c(1,0.85),
                       labels=plot_labels, label_x = c(0.05,0.05), label_y = c(1, 1))

ggsave('manuscript/value_example.png', main_plots, width=29, height = 13, units = 'cm')
