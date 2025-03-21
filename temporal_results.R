###
# Presenting temporal reconstruction results
###
library(tidyverse)
library(moments)

# Data
timeVAE_results_path <- '/home/wolf6273/4D_geom/checkpoints/temporal/'
timeVAE_experiments <- c('pca_12_components_16_embeddings',
                         'final_pca_12')

ssa_pca_results_path <- '/home/wolf6273/singular_spectrum_analysis/results/'
ssa_pca_experiments <- c('ssa_pca_6_components_on_pca_12_components',
                         'ssa_pca_12_components_on_pca_12_components',
                         'ssa_pca_16_components_on_pca_12_components',
                         'ssa_pca_24_components_on_pca_12_components',
                         'ssa_pca_50_components_on_pca_12_components',
                         'ssa_pca_12_components_on_pca_16_components',
                         'ssa_pca_16_components_on_pca_16_components',
                         'ssa_pca_24_components_on_pca_16_components',
                         'ssa_pca_50_components_on_pca_16_components',
                         'ssa_pca_12_components_on_pca_24_components',
                         'ssa_pca_16_components_on_pca_24_components',
                         'ssa_pca_24_components_on_pca_24_components',
                         'ssa_pca_50_components_on_pca_24_components',
                         'ssa_pca_12_components_on_pca_50_components',
                         'ssa_pca_16_components_on_pca_50_components',
                         'ssa_pca_24_components_on_pca_50_components',
                         'ssa_pca_50_components_on_pca_50_components',
                         'ssa_pca_50_components_on_gdl_12_components')

ssa_pca_separated_features_experiments <- c('ssa_pca_50_components_on_pca_12_components_separated_features',
                                            'ssa_pca_50_components_on_gdl_12_components_separated_features')

# Error analysis
errors_file_name <- 'errors.csv'
timeVAE_errors <- read.csv(paste(timeVAE_results_path, timeVAE_experiments, '/' ,  errors_file_name, sep = '')) %>% bind_rows()
timeVAE_errors$temporal_model_type <- 'DL'
timeVAE_errors %>% 
  mutate(geom_model_type = str_extract(model, 'pca|gdl')) %>% 
  mutate(k_temp = str_extract(model, '\\d+(?=_embeddings)')) %>% 
  mutate(k_geom = str_extract(model, paste('(?<=', geom_model_type, '_)\\d+', sep=''))) -> timeVAE_errors
ssa_pca_error_files <- paste(ssa_pca_results_path, ssa_pca_experiments, '/' ,  errors_file_name, sep = '')
ssa_pca_errors <- read_csv(ssa_pca_error_files) %>% bind_rows()
ssa_pca_errors$temporal_model_type <- 'SSA'
ssa_pca_errors %>% 
  mutate(geom_model_type = str_extract(model, '(?<=on_)(pca|gdl)')) %>% 
  mutate(k_temp = str_extract(model, '(?<=ssa_pca_)\\d+')) %>% 
  mutate(k_geom = str_extract(model, paste('(?<=on_', geom_model_type, '_)\\d+', sep = ''))) -> ssa_pca_errors

ssa_pca_separated_features_error_files <- paste(ssa_pca_results_path, ssa_pca_separated_features_experiments, '/' ,  errors_file_name, sep = '')
ssa_pca_separated_features_errors <- read_csv(ssa_pca_separated_features_error_files) %>% bind_rows()
ssa_pca_separated_features_errors$temporal_model_type <- 'SSA_separated_features'
ssa_pca_separated_features_errors %>% 
  mutate(geom_model_type = str_extract(model, '(?<=on_)(pca|gdl)')) %>% 
  mutate(k_temp = str_extract(model, '(?<=ssa_pca_)\\d+')) %>% 
  mutate(k_geom = str_extract(model, paste('(?<=on_', geom_model_type, '_)\\d+', sep = ''))) -> ssa_pca_separated_features_errors

errors <- rbind(timeVAE_errors,
                ssa_pca_errors,
                ssa_pca_separated_features_errors)
errors %>% 
  mutate(k_geom = as.integer(k_geom),
         k_temp = as.integer(k_temp)) -> errors

# embed dims results
errors %>% 
  filter(k_geom == 12) %>% 
  group_by(sample, frame, error_type, temporal_model_type, geom_model_type, k_temp, k_geom) %>% 
  summarise(mean_loss = mean(loss)) %>% 
  group_by(error_type, temporal_model_type, geom_model_type, k_temp, k_geom) %>% 
  summarise(mean_loss = mean(mean_loss))
            
rrors %>% 
  filter(k_temp > 6, k_geom == 12, geom_model_type == 'pca') %>% 
  group_by(sample, frame, error_type, temporal_model_type, k_temp) %>% 
  summarise(mean_loss = mean(loss)) %>% 
  ggplot(aes(y = mean_loss, fill = error_type, x = error_type)) + 
  geom_hline(yintercept = 1.8) +
  facet_grid(temporal_model_type ~ k_temp) +
  geom_boxplot() + 
  theme_bw()

# ssa embed dims results
errors %>% 
  # filter(k_temp > 6 & k_temp < 50, temporal_model_type == 'SSA', k_geom > 6 & k_geom < 50) %>% 
  filter(k_temp > 6 & k_temp != 16, temporal_model_type == 'SSA', k_geom %in% c(12, 50), geom_model_type == 'pca') %>% 
  mutate(k_temp = paste('k_temp =', k_temp),
         k_geom = paste('k_geom =', k_geom)) %>% 
  group_by(sample, frame, error_type, k_temp, k_geom) %>% 
  summarise(mean_loss = mean(loss)) %>% 
  ggplot(aes(y = mean_loss, fill = error_type, x = error_type)) +
  geom_hline(yintercept = 1.8) +
  facet_grid(k_temp ~ k_geom) +
  geom_boxplot() + 
  theme_bw() + 
  ggtitle(expression('Reconstruction quality while varying latent variables')) +
  ylab('Mean Chamfer Loss') +
  xlab('Train/Test split')+
  scale_fill_discrete(guide='none')

ggsave('ssa_emb_dims_results.png', width = 7, height = 12)


errors %>% 
  group_by(error_type) %>% 
  summarise(sd(loss))
errors %>% 
  filter(error_type == 'valid', k_temp > 6) %>% 
  group_by(sample, frame, k_geom, k_temp) %>% 
  summarise(mean(loss)) %>% 
  group_by(k_geom, k_temp) %>% 
  summarise(mean_loss = mean(`mean(loss)`), sd = sd(`mean(loss)`)) %>% 
  mutate(loss = paste(round(mean_loss, 2), '(X', round(sd, 2), ')')) %>% 
  select(-mean_loss, -sd) %>% 
  pivot_wider(names_from = k_geom, values_from = loss)

# Latent space distribution
latent_space_dist_file_name <- 'temporal_representations.csv'
ssa_pca_latent_space_dist <- read.csv(paste(ssa_pca_results_path, ssa_pca_experiments[[3]], '/' ,  latent_space_dist_file_name, sep = ''))
ssa_pca_latent_space_dist %>% 
  select(-X) %>% 
  pivot_longer(starts_with('X'), names_to = 'SSA_PC', values_to = 'PC_value') %>% 
  mutate(SSA_PC = as.integer(str_extract(SSA_PC, '\\d+'))) -> ssa_pca_latent_space_dist 

ssa_pca_latent_space_dist <- read.csv(paste(ssa_pca_results_path, ssa_pca_experiments[[18]], '/' ,  latent_space_dist_file_name, sep = ''))
ssa_pca_latent_space_dist %>% 
  select(-X) %>% 
  pivot_longer(starts_with('X'), names_to = 'SSA_PC', values_to = 'PC_value') %>% 
  mutate(SSA_PC = as.integer(str_extract(SSA_PC, '\\d+'))) -> ssa_pca_latent_space_dist 

# Test for normality
ssa_pca_latent_space_dist %>% 
  filter(split == 'train') %>% 
  group_by(SSA_PC) %>%  
  summarise(shapiro = shapiro.test(PC_value)[['p.value']],
            agostino = agostino.test(PC_value)[['p.value']]) %>%  
  pivot_longer(names_to = 'test', cols = c('shapiro', 'agostino')) %>% 
  mutate(significance = ifelse(value < 0.05/16, ifelse(value < 0.01/16, ifelse(value < 0.001/16, '***', '**'), '*'), '')) -> ssa_stat_tests
ssa_stat_tests

# Latent space distribution plot
ssa_pca_latent_space_dist %>% 
  merge(y = ssa_stat_tests%>% 
          filter(test == 'shapiro'), by = "SSA_PC") %>% 
  filter(split == 'train') %>% 
  mutate(SSA_PC = str_replace(SSA_PC, 'X', 'PC')) %>% 
  mutate(SSA_PC = paste(SSA_PC, ', p =', signif(value, 3), significance)) %>% 
  # ggplot(aes(x = PC_value, fill = SSA_PC)) + 
  ggplot(aes(x = PC_value)) + 
  geom_histogram(aes(y = after_stat(density))) + 
  facet_wrap(. ~ SSA_PC, scales = 'free') + 
  theme_bw() +
  ggtitle('Distribution of each SSA latent variable') +
  xlab('PC Value') +
  ylab('Density')

ggsave('SSA_on_PC_latent_densities.png')


  
# Latent space traversal
latent_traversal_file_name <- 'time_series_traversal.csv'
timeVAE_latent_traversal <- read.csv(paste(timeVAE_results_path, timeVAE_experiments, '/' ,  latent_traversal_file_name, sep = '')) 
ssa_pca_latent_traversal <- lapply(paste(ssa_pca_results_path, ssa_pca_experiments, '/' ,  latent_traversal_file_name, sep = ''), read.csv)
# Plot traversal
latent_traversal_plot <- function(latent_traversal_df, title){
  latent_traversal_df %>% 
    filter(geom_dim < 16 & temp_dim < 16) %>% 
    mutate(geom_dim = paste('k_geom =', geom_dim),
           temp_dim = paste('k_temp =', temp_dim)) %>% 
    ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
    geom_line() +
    facet_grid(geom_dim ~ temp_dim, scales = 'free_y') +
    scale_color_viridis_b() +
    ggtitle(title) +
    theme_bw()
}

latent_traversal_plot(ssa_pca_latent_traversal[[9]], 'SSA (k_temp = 50) on PCA (k_geom = 16)')
latent_traversal_plot(ssa_pca_latent_traversal[[18]], 'SSA (k_temp = 50) on GDL (k_geom = 12)')
# The diagonals dominated by each principal component. But you can see that within that, there is variation over time, and some functional differences can be observed
ggsave('SSA_on_PCA_traversal.png')

latent_traversal_plot(timeVAE_latent_traversal, 'ConvVAE (k_temp = 16) on PCA (k_geom = 12)')
