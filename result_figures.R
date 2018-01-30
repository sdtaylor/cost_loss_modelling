library(tidyverse)
library(viridis)

##################################################
# Portal
portal_cost_loss_values = read_csv('results/portal_cost_loss_values.csv')

############
thresholds = sort(unique(portal_cost_loss_values$threshold))
threshold_labels = paste0('< ', thresholds)
portal_cost_loss_values$threshold = factor(portal_cost_loss_values$threshold, levels = thresholds, labels = threshold_labels)

portal_fig = ggplot(portal_cost_loss_values, aes(x=a, y=value, color=model)) + 
  ylim(0.01,1) +
  geom_line(size=1.5) + 
  facet_grid(species~threshold) +
  theme_bw() +
  theme(axis.text = element_text(size=8),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle='Abundance Threshold')

ggsave(portal_fig, filename = 'manuscript/portal_cost_loss_values.png', height=5, width=8)

##################################################
# Idaho fire
idaho_cost_loss_values = read_csv('results/idaho_fire_cost_loss_results.csv')

idaho_value = ggplot(idaho_cost_loss_values, aes(x=a, y=value, color=as.factor(year), group=year)) + 
  ylim(0.001,1) +
  xlim(0,1) +
  geom_line(size=2)+
  #scale_color_brewer(palette = 'Greys') +
  scale_color_viridis(discrete = TRUE, direction = -1, option = 'inferno', end=0.94) +
  theme_bw() +
  theme(plot.title = element_text(size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20),
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        legend.position = c(0.8, 0.8), 
        legend.direction = "vertical",
        legend.key.width = unit(15, units = 'mm'),
        legend.key.height = unit(9, units='mm')) +
  labs(title = "Idaho Fire Cost Loss Analysis",
       x='a = C/L',y='Value', color='Year') 

ggsave(idaho_value, filename = 'manuscript/idaho_cost_loss_value.png',height=8, width=10)

# Value over time
a_ratios_of_interest = c(0.01, 0.25, 0.51, 0.63)
idaho_value_over_time = ggplot(filter(idaho_cost_loss_values, a %in% a_ratios_of_interest), aes(x=year, y=value, color=as.factor(a), group=a)) + 
  ylim(-0.5,1) +
  geom_line(size=2.5)+
  geom_hline(yintercept = 0, size=2) +
  scale_color_brewer(palette='Dark2') +
  theme_bw() +
  theme(plot.title = element_text(size = 30),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 23),
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        legend.position =  c(0.5, 0.17), 
        legend.direction = "vertical",
        legend.background = element_rect(fill='grey95'),
        legend.key.width = unit(35, units = 'mm'),
        legend.key.height = unit(10, units='mm')) +
  labs(title = "Idaho Fire cost loss values over time",
       x='Year', y = 'Value', color='a = C/L') 

ggsave(idaho_value_over_time, filename = 'manuscript/idaho_value_over_time.png',height=8, width=10)

both_idaho_plots = cowplot::plot_grid(idaho_value, idaho_value_over_time,
                                      labels=c('A.','B.'), ncol=1, label_size = 30)
ggsave(both_idaho_plots, filename='manuscript/idaho_value.png',height=16, width=10)
