###
# PCA x TimeVAE hyperparameter tuning results
###

library(tidyverse)

# Load results
hyperparameter_tuning_results <- read_csv("hyperparameter_tuning_results.csv")

# Loss type results
hyperparameter_tuning_results %>% 
  filter(beta_value == 0.001,
         layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(geom_model, loss_type) %>% 
  summarise(val_loss = mean(val_loss)) %>% 
  pivot_wider(values_from = val_loss, names_from = geom_model) %>% 
  select(loss_type, pca, gdl) %>% 
  kable(booktabs = T, format = 'latex', digits = 2)

# beta value
hyperparameter_tuning_results %>% 
  filter(layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         )  %>%
  filter(!(geom_model == 'pca' & loss_type != 'corresponding_points')) %>%
  group_by(geom_model, beta_value) %>%
  summarise(val_loss = mean(val_loss)) %>%
  pivot_wider(values_from = val_loss, names_from = geom_model) %>% 
  select(beta_value, pca, gdl) %>% 
  mutate(across(c(pca, gdl), ~ round(.x, 2))) %>% 
  kable(booktabs = T, format = 'latex')

# Hidden layers
hyperparameter_tuning_results %>% 
  filter(
         beta_value == 0.001,
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  filter(!(geom_model == 'pca' & loss_type != 'corresponding_points')) %>%
  group_by(geom_model, layers) %>%
  summarise(val_loss = mean(val_loss)) %>%
  pivot_wider(values_from = val_loss, names_from = geom_model) %>%
  select(layers, pca, gdl)  %>%
  arrange(nchar(layers)) %>% 
  kable(booktabs = T, format = 'latex', digits = 2)

# Model architeture
hyperparameter_tuning_results %>% 
  filter(
         beta_value == 0.001,
         layers == '[50, 100, 200]',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(geom_model, model_architecture) %>% 
  summarise(val_loss = mean(val_loss)) %>%
  pivot_wider(values_from = val_loss, names_from = geom_model) %>%
  select(model_architecture, pca, gdl)  %>%
  kable(booktabs = T, format = 'latex', digits = 2)

# emb dims
pca_hyperparam_results %>% 
  filter(loss_type == 'corresponding_points',
         beta_value == 0.001,
         layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_geom_feats == 12
         ) 

# Load results
hyperparameter_tuning_results %>% 
  filter(geom_model == 'gdl') %>% 
  select(-geom_model) -> gdl_hyperparam_results

# Loss type results
gdl_hyperparam_results %>% 
  filter(beta_value == 0.001,
         layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(loss_type) %>% 
  summarise(val_loss = mean(val_loss)) %>% 
  kable(booktabs = T, format = 'latex')

# beta value
pca_hyperparam_results %>% 
  filter(loss_type == 'corresponding_points',
         layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(beta_value) %>% 
  summarise(val_loss = mean(val_loss)) %>% 
  kable(booktabs = T, format = 'latex')

# Hidden layers
pca_hyperparam_results %>% 
  filter(loss_type == 'corresponding_points',
         beta_value == 0.001,
         model_architecture == 'vae_conv',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(layers) %>% 
  summarise(val_loss = mean(val_loss)) %>% 
  arrange(nchar(layers)) %>% 
  kable(booktabs = T, format = 'latex')

# Model architeture
pca_hyperparam_results %>% 
  filter(loss_type == 'corresponding_points',
         beta_value == 0.001,
         layers == '[50, 100, 200]',
         n_temp_feats == 16,
         n_geom_feats == 12
         ) %>% 
  group_by(model_architecture) %>% 
  summarise(val_loss = mean(val_loss)) %>% 
  kable(booktabs = T, format = 'latex')

# emb dims
pca_hyperparam_results %>% 
  filter(loss_type == 'corresponding_points',
         beta_value == 0.001,
         layers == '[50, 100, 200]',
         model_architecture == 'vae_conv',
         n_geom_feats == 12
         ) 