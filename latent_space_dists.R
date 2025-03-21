###
# Latent space dist
###

# Packages
library(tidyverse)
library(moments)

# Results
model_results <- read_csv("model_results.csv")

# # Remove sep results
# model_results %>% 
#   filter(!str_detect(temp_model, 'sep')) -> model_results

# Removing missing results
model_results <- model_results %>% 
  select(-experiment_description) %>% drop_na()

# Loading and cleaning error data
## Loading errors
model_results %>% 
  mutate(latent_dist_file = ifelse(str_detect(temp_model, 'PCA'), 'temporal_representations.csv', 'mus.csv'),
         latent_dist_file = paste(results_dir, '/', latent_dist_file, sep = '')) -> model_results
latent_dists <- lapply(model_results$latent_dist_file, read_csv)

## Merging with model_results
latent_dists <- Map(function(df, val) {
  df$results_dir <- val
  if('split' %in% colnames(df)){
    df <- df %>% 
      filter(split == 'train') %>% 
      select(-split) -> df
  }
  df %>% 
    select(-1) -> df
  df %>% 
    pivot_longer(cols = !results_dir, names_to = 'latent_var') -> df
  df
}, latent_dists, model_results$results_dir)
latent_dists <- latent_dists %>% bind_rows() # correct when all results found 
latent_dists  <- merge(model_results,latent_dists, by='results_dir')
latent_dists %>% 
  select(-results_dir, -latent_dist_file) -> latent_dists

# Test for normality
latent_dists %>% 
  group_by(temp_model, geom_model, latent_var) %>% 
  summarise(shapiro = shapiro.test(value)[['p.value']],
            agostino = agostino.test(value)[['p.value']]) %>% 
  pivot_longer(names_to = 'test', cols = c('shapiro', 'agostino')) %>% 
  # mutate(significance = ifelse(value < 0.05/16, ifelse(value < 0.01/16, ifelse(value < 0.001/16, '***', '**'), '*'), '')) -> normality_stat_tests # check this!!!
  mutate(significance = ifelse(value < 0.05, ifelse(value < 0.01, ifelse(value < 0.001, '***', '**'), '*'), '')) %>% 
  mutate(bonferoni_significance = ifelse(value < 0.05/16, ifelse(value < 0.01/16, ifelse(value < 0.001/16, '***', '**'), '*'), '')) -> normality_stat_tests # check this!!!
normality_stat_tests %>% 
  group_by(temp_model, geom_model, test) %>% 
  summarise(n_latents = n(), n_normal = sum(significance == ''), prop_normal = n_normal/n_latents) %>% 
  mutate(value = pbinom(n_normal, n_latents, 0.95),) %>% 
  mutate(significance = ifelse(value < 0.05, ifelse(value < 0.01, ifelse(value < 0.001, '***', '**'), '*'), '')) %>% 
  select(-c(value, n_latents, n_normal)) %>% 
  pivot_wider(names_from = test, values_from = c(prop_normal, significance)) -> normality_stats_table

normality_stats_table %>% 
  mutate(prop_normal_agostino = prop_normal_agostino * 100,
         prop_normal_shapiro = prop_normal_shapiro * 100) %>% 
  select(temp_model, geom_model, prop_normal_shapiro, significance_shapiro, prop_normal_agostino, significance_agostino) %>% 
  kable(digits = 1, format = 'latex', booktabs = T)

# Latent space distribution plot
latent_dists %>% 
  group_by(geom_model, temp_model, latent_var) %>% 
  mutate(latent_sds = sd(value)) -> latent_sds
latent_dists %>% 
  merge(y = latent_sds) -> latent_dists
latent_dists %>% 
  merge(y = normality_stat_tests %>% 
          mutate(p_value = value) %>% 
          filter(test == 'shapiro') %>% 
          select(-c(test, value)), by = c('temp_model', 'geom_model','latent_var')
          ) %>% 
  filter(0 < as.integer(latent_var), as.integer(latent_var) <= 6) %>% 
  mutate(model = paste(geom_model, 'x', temp_model, sep=''),
         value = value/latent_sds) %>% 
  # filter(model == 'PCAxPCA') %>% 
  ggplot(aes(x = value, fill = significance)) +
  geom_histogram(aes(y = after_stat(density)), color = 'black') +
  facet_grid(model ~ latent_var, scales = 'free') +
  theme_bw() +
  ggtitle('Distribution of the first 6 latent representations for each model') +
  xlab('Normalised Latent Value') +
  ylab('Density') +
  scale_fill_discrete(name = 'Shapiro Significance')

ggsave('latent_dists.pdf')
