###
# Temporal PCA settings analysis
###
library(tidyverse)
library(moments)

# Load data
temporal_pca_results <- read_csv("temporal_pca_results.csv")
## Cleaning data
temporal_pca_results %>% 
  select(-experiment_description) %>% 
  drop_na() -> temporal_pca_results
temporal_pca_results
## Linking error data
error_files <- paste(temporal_pca_results$results_dir, '/errors.csv', sep = '')
error_tables <- lapply(error_files, read_csv)
## Merging with model_results
error_tables <- Map(function(df, val) {
  df$results_dir <- val
  df
}, error_tables, temporal_pca_results$results_dir)
error_tables <- error_tables %>% bind_rows() # correct when all results found 
error_tables <- merge(temporal_pca_results,error_tables, by='results_dir')
error_tables %>% 
  select(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame, substructure, loss, error_type) -> error_tables

# n_temp_feats vs n_geom_feats
error_tables %>%
  filter(error_type == 'valid',
         geom_model == 'PCA') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>%
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  mutate(loss = paste(round(mean_loss, 2), ' (X', round(sd_loss, 2), ')', sep="")) %>% 
  select(- mean_loss, -sd_loss) %>% 
  pivot_wider(names_from = c('n_geom_feats'), values_from = 'loss') %>% 
  ungroup() %>% 
  select(-geom_model, -temp_model) -> n_feats_results_pca
n_feats_results_pca %>% 
  kable(format = "latex")

# n_temp_feats vs n_geom_feats
error_tables %>%
  filter(error_type == 'valid',
         geom_model == 'GDL') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>%
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  mutate(loss = paste(round(mean_loss, 2), ' (X', round(sd_loss, 2), ')', sep="")) %>% 
  select(- mean_loss, -sd_loss) %>% 
  pivot_wider(names_from = c('n_geom_feats'), values_from = 'loss') %>% 
  ungroup() %>% 
  select(-geom_model, -temp_model) -> n_feats_results_gdl
n_feats_results_gdl %>% 
  kable(format = "latex")
