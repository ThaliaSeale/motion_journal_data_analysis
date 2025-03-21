###
# Temporal reconstruction error
###

# Packages
library(tidyverse)
library(moments)

# Results
model_results <- read_csv("model_results.csv")

# Removing missing results
model_results <- model_results %>% 
  select(-experiment_description) %>% drop_na()

# Loading and cleaning error data
## Loading errors
error_files <- paste(model_results$results_dir, '/errors.csv', sep = '')
error_tables <- lapply(error_files, read_csv)
## Merging with model_results
error_tables <- Map(function(df, val) {
  df$results_dir <- val
  df
}, error_tables, model_results$results_dir)
error_tables <- error_tables %>% bind_rows() # correct when all results found 
error_tables <- merge(model_results,error_tables, by='results_dir')
error_tables %>% 
  select(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame, substructure, loss, error_type) -> error_tables
error_tables

# Overall reconstruction results
# Table for average reconstruction loss for each sample
error_tables %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame, error_type) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, error_type) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, error_type) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  mutate(loss = paste(round(mean_loss, 2), '(X', round(sd_loss, 2), ')')) %>% 
  select(-mean_loss, -sd_loss)

error_tables %>% 
  mutate(model = paste(geom_model, '+', temp_model)) %>% 
  group_by(model, sample, error_type) %>% 
  summarise(loss = mean(loss)) %>%
  ggplot(aes(x = model, fill = error_type, y = loss)) +
  geom_boxplot() +
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(. ~ model, scales = 'free_x') +
  theme_bw() +
  ggtitle('Avg reconstruction loss for each sample over the full cardiac cycle reconstruction') +
  labs(fill = 'Split') +
  xlab('Model') +
  ylab('Mean Chamfer distance (mm)')

# Substructure results
error_tables %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, substructure, error_type) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  mutate(loss = paste(round(mean_loss, 2), '(X', round(sd_loss, 2), ')')) %>% 
  select(-mean_loss, -sd_loss) %>% 
  pivot_wider(names_from = substructure, values_from = c(loss))

error_tables %>% 
  mutate(model = paste(geom_model, '+', temp_model)) %>%  
  filter(error_type == 'valid') %>% 
  mutate(across(where(is.character), ~ str_replace_all(., "_", " "))) %>% 
  ggplot(aes(x = model, fill = substructure, y = loss)) +
  geom_boxplot() + 
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(. ~ model, scales = 'free_x') +
  theme_bw() +
  ggtitle('Avg reconstruction loss of each substructure over the full cardiac cycle reconstruction') +
  labs(fill = 'Substructure') +
  xlab('Model') + 
  ylab('Mean Chamfer distance (mm)')

# Frame results
error_tables %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, frame) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  mutate(loss = paste(round(mean_loss,2), '(X', round(sd_loss,2), ')')) %>% 
  select(-mean_loss, -sd_loss) %>% 
  pivot_wider(values_from = loss, names_from = frame)

error_tables %>% 
  mutate(model = paste(geom_model, '+', temp_model),
         frame = as.factor(frame)) %>%  
  mutate(model = str_replace_all(model, '_', ' ')) %>% 
  filter(error_type == 'valid') %>% 
  group_by(model, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  ggplot(aes(x = frame, y = loss, fill = model)) +
  geom_boxplot() + 
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(model ~ .) +
  theme_bw() +
  ggtitle('Avg reconstruction loss for each frame over the full cardiac cycle reconstruction') +
  xlab('Frame') + 
  ylab('Mean Chamfer distance (mm)')
