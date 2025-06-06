###
# PCA x TimeVAE hyperparameter tuning results
###

library(tidyverse)
library(viridisLite)
library(knitr)
colors <- viridis(3)

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
hyperparameter_tuning_results %>% 
  filter(geom_model == 'pca') %>% 
  select(-geom_model) -> pca_hyperparam_results

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

# Number of features
hyperparameter_tuning_results %>% 
  # drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'pca') %>%
  select(n_temp_feats, n_geom_feats, val_loss) %>% 
  mutate(val_loss = round(val_loss, 2)) %>% 
  pivot_wider(values_from = val_loss, names_from = n_geom_feats) %>% 
  arrange(n_temp_feats) %>% 
  kable('latex', booktabs=T)

hyperparameter_tuning_results %>% 
  drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'pca') %>%
  select(n_temp_feats, n_geom_feats, kld_loss) %>% 
  mutate(kld_loss = round(kld_loss, 2)) %>% 
  pivot_wider(values_from = kld_loss, names_from = n_geom_feats) %>% 
  arrange(n_temp_feats) %>% 
  kable('latex', booktabs=T)

hyperparameter_tuning_results %>% 
  drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'pca') %>%
  select(n_temp_feats, n_geom_feats, val_loss, kld_loss) %>% 
  ggplot(aes(x = val_loss, y = kld_loss, color = as.factor(n_geom_feats), shape = as.factor(n_temp_feats))) +
  geom_point() +
  theme_bw() +
  xlab('Reconstruction Loss') +
  ylab('KLD Loss') +
  guides(shape=guide_legend(title='k_temp'), color=guide_legend(title='k_geom')) +
  scale_color_manual(values =colors) +
  ggtitle('Recon. Loss vs KLD loss \n for different values of k_geom and k_temp')

ggsave('PCAxTimeVAE_n_temp.png', height=4, width=6)

hyperparameter_tuning_results %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'gdl') %>%
  select(n_temp_feats, n_geom_feats, val_loss) %>% 
  mutate(val_loss = round(val_loss, 2)) %>% 
  pivot_wider(values_from = val_loss, names_from = n_geom_feats) %>% 
  arrange(n_temp_feats) %>% 
  kable('latex', booktabs=T)


hyperparameter_tuning_results %>% 
  drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'gdl') %>%
  select(n_temp_feats, n_geom_feats, kld_loss) %>% 
  mutate(kld_loss = round(kld_loss, 2)) %>% 
  pivot_wider(values_from = kld_loss, names_from = n_geom_feats) %>% 
  arrange(n_temp_feats) %>% 
  kable('latex', booktabs=T)
  
hyperparameter_tuning_results %>% 
  drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE') %>% 
  select(geom_model, n_temp_feats, n_geom_feats, val_loss, kld_loss) %>% 
  ggplot(aes(x = val_loss, y = kld_loss, color = as.factor(n_geom_feats), shape = as.factor(n_temp_feats))) +
  geom_point() +
  theme_bw() +
  xlab('Reconstruction Loss') +
  ylab('KLD Loss') +
  guides(shape=guide_legend(title='k_temp'), color=guide_legend(title='k_geom')) +
  scale_color_manual(values =colors) +
  facet_grid(geom_model ~ .) +
  ggtitle('Recon. Loss vs KLD loss \n for different values of k_geom and k_temp')

hyperparameter_tuning_results %>% 
  drop_na() %>% 
  filter(beta_value == 0.0001,
         layers == '[50]',
         model_architecture == 'timeVAE',
         geom_model == 'gdl') %>%
  select(n_temp_feats, n_geom_feats, kld_loss) %>% 
  mutate(kld_loss = round(kld_loss, 2)) %>% 
  pivot_wider(values_from = kld_loss, names_from = n_geom_feats) %>% 
  arrange(n_temp_feats) %>% 
  kable('latex', booktabs=T)

ggsave('TimeVAE_n_temp.png', height=4, width=6)

