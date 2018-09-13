library(tidyverse)

################################
###############################
# BBS
bbs_cost_loss_results = read_csv('results/bbs_cost_loss_results.csv')
bbs_species_names = read_csv('raw_data/bbs/BBS50_species.csv') %>%
  select(Aou = AOU, species = english_common_name)

bbs_cost_loss_results = bbs_cost_loss_results %>%
  left_join(bbs_species_names, by='Aou')
#########
# The number of species which made it to the final analysis

num_species = length(unique(bbs_cost_loss_results$species))
species_with_or_without_value = bbs_cost_loss_results %>%
  group_by(species) %>%
  summarise(has_some_value = max(value) >0,
            has_no_value = max(value)<=0) %>%
  ungroup()
sum(species_with_or_without_value$has_no_value)
