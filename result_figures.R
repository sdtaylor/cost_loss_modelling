library(tidyverse)
library(viridis)

##################################################
##################################################
# Portal
portal_cost_loss_values = read_csv('results/portal_cost_loss_values.csv')

############
thresholds = sort(unique(portal_cost_loss_values$threshold))
threshold_labels = paste0('< ', thresholds)
portal_cost_loss_values$threshold = factor(portal_cost_loss_values$threshold, levels = thresholds, labels = threshold_labels)

portal_cost_loss_values$model = factor(portal_cost_loss_values$model, levels = c('season_avg','tsglm'), 
                                                        labels = c('Seasonal\nAverage', 'TS-GLM'))

portal_fig = ggplot(portal_cost_loss_values, aes(x=a, y=value, color=model)) + 
  ylim(0.01,1) +
  geom_line(size=1.5) + 
  scale_color_manual(values = c('#E69F00','#0072B2')) + 
  facet_grid(species~threshold) +
  theme_bw() +
  theme(axis.text = element_text(size=8),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle='Abundance Threshold',
       x='a', y = 'Value', color='Model')

ggsave(portal_fig, filename = 'manuscript/portal_cost_loss_values.png', height=5, width=8)

##################################################
##################################################
# BBS

bbs_cost_loss_results = read_csv('results/bbs_cost_loss_results.csv') %>%
  filter(threshold_type %in% c('max_specificity_sensitivity_base_probability', 'cl_ratio_base_probability'))
bbs_species_names = read_csv('raw_data/bbs/BBS50_species.csv') %>%
  select(Aou = AOU, species = english_common_name)

bbs_cost_loss_results = bbs_cost_loss_results %>%
  left_join(bbs_species_names, by='Aou')

selected_species = c(
  6450, #  Nashville Warbler
  6882, # House Sparrow
  5930, # Northern Cardinal
  5010  # Eastern Meadowlark
)

bbs_select_spp_value = ggplot(filter(bbs_cost_loss_results, Aou %in% selected_species), aes(x=a, y=value, color=threshold_type)) +
  geom_line(size=2) + 
  scale_color_manual(values=c('#0072B2','#D55E00')) + 
  ylim(0.01,1) + 
  facet_wrap(~species) + 
  labs(y='Value', x = 'Cost/Loss Ratio (a)', color='Threshold Type') + 
  theme_bw() + 
  theme(strip.text = element_text(size=15),
        axis.text =  element_text(size=13),
        axis.title = element_text(size=15),
        legend.position = c(0.75, y=0.88),
        legend.background = element_rect(color='black',fill='grey99'),
        legend.key.width = unit(15,'mm'),
        legend.title = element_text(size=13),
        legend.text = element_text(size=10))

ggsave(bbs_select_spp_value, filename='manuscript/bbs_select_spp_value.png',height=20, width=23, units='cm', dpi=100)

##############
# Plot of quantile values
# Three quantiles, and the maximum, value for all xxx BBS species over all possible C/L values.
# The minimum value for any given C/L is below 0, signifying a forecast provides no value over taking naive actions.
bbs_median_values = bbs_cost_loss_results %>%
  group_by(a) %>%
  summarize('50%' = quantile(value, 0.50),
            'Max Value' = max(value),
            '25%' = quantile(value, 0.25),
            '75%' = quantile(value, 0.75)) %>%
  ungroup() %>%
  gather(metric, metric_value, '50%','Max Value','25%','75%')

bbs_quantile_values = ggplot(bbs_median_values, aes(x=a, y=metric_value, color=metric)) +
  geom_line(size=3) + 
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  ylim(0,1) +
  xlim(0,1) +
  labs(y='Value', x = 'a = Cost/Loss', color='Quantile') + 
  theme_bw()  +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.position = c(0.8,0.8),
        legend.background = element_rect(fill = 'white',color='black')) + 
  guides(color = guide_legend(reverse = TRUE))

ggsave(bbs_quantile_values, filename = 'manuscript/bbs_quantile_values.png', height=5, width=6)

###############
# Supplement plot of value for every species
bbs_all_spp_values = ggplot(bbs_cost_loss_results, aes(x=a, y=value, color=threshold_type)) +
  geom_line() + 
  #scale_color_manual(values=c('black','#56B4E9')) + 
  ylim(0,1) + 
  facet_wrap(~species) + 
  labs(y='Value', x = 'a = cost/loss') + 
  theme_bw() + 
  theme(strip.text = element_text(size=5),
        axis.text =  element_text(size=5))

ggsave(bbs_all_spp_values, filename='manuscript/bbs_all_spp_values.png',height=16, width=14)

