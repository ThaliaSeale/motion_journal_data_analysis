###
# Presenting geometric reconstruction results V2
###
# Packages
library(tidyverse)
library(knitr)
library(plotrix)

# Loading data
pca_experiment_names <- c('pca_16_components',
                      'pca_50_components',
                      'pca_6_components',
                      'pca_12_components',
                      'pca_24_components',
                      'kpca_poly_deg_1_6_pcs',
                      'kpca_poly_deg_1_12_pcs',
                      'kpca_poly_deg_1_16_pcs',
                      'kpca_poly_deg_1_24_pcs',
                      'kpca_poly_deg_1_50_pcs',
                      'kpca_poly_final_deg_2',
                      'kpca_poly_final_deg_3',
                      'kpca_poly_final_deg_4'
                      )


pca_errors <- lapply(pca_experiment_names, function(x) read.csv(paste('/home/wolf6273/PC_PCA/results/', x, '/errors.csv', sep='')))

gdl_experiment_names <- c('gdl_6_latents',
                          'gdl_16_latents_low_beta',
                          'gdl_12_latents_low_beta',
                          'gdl_24_latents_low_beta',
                          'gdl_50_latents_low_beta'
                          )

gdl_errors <- lapply(gdl_experiment_names, function(x) read.csv(paste('/home/wolf6273/4D_geom/checkpoints/geometric/', x, '/errors.csv', sep='')))

errors <- c(pca_errors, gdl_errors)
errors <- bind_rows(errors)

# Formatting data
errors %>%
  mutate(error_type = factor(error_type, levels = c('valid', 'train'))) %>% 
  mutate(method = str_extract(model, 'pca|kpca|gdl')) %>%
  mutate(method = ifelse(method == 'pca', 'PCA', method)) %>%
  mutate(method = ifelse(method == 'kpca', 'kPCA', method)) %>%
  mutate(method = ifelse(method == 'gdl', 'GDL', method)) %>%
  mutate(n_components = str_extract(model, '\\d+(?=_components|_pcs|_latents)')) %>% 
  mutate(n_components = ifelse(is.na(n_components), 50, n_components)) %>% 
  mutate(n_components = as.integer(n_components)) %>% 
  mutate(poly_degree = str_extract(model, '(?<=deg_)\\d+')) -> errors

# Table
errors %>%
  mutate(kernel = str_extract(model, '(RBF|poly|Sigmoid)')) %>% 
  mutate(model_type = ifelse(str_detect(method, 'kPCA'), paste(method, ' (', kernel, ')', sep=''), method)) %>% 
  mutate(model_type = ifelse(str_detect(method, 'kPCA'), paste(method, ' (', kernel, ')', sep=''), method)) %>% 
  mutate(model_type = ifelse(str_detect(model, 'poly'), paste(model_type, '($d =', poly_degree, '$)'), model_type)) -> errors
errors %>% 
  group_by(model_type, n_components, error_type, poly_degree) %>% 
  summarise(mean_error = mean(error), sd_error = sd(error), n = n()) %>% 
  mutate(CI_upper = mean_error + qnorm(0.975) * sqrt(sd_error/n),
         CI_lower = mean_error - qnorm(0.975) * sqrt(sd_error/n)) -> summary_table
summary_table %>% 
  mutate(mean_error = signif(mean_error, digits = 3),
         sd_error = signif(sd_error, digits = 3)) %>% 
  mutate(error = paste(mean_error, ' (X', sd_error, ')', sep='')) %>% 
  select(model_type, n_components, error_type, error) %>% 
  pivot_wider(names_from = error_type, values_from = error) %>% 
  select(model_type, n_components, train, valid)  %>% 
  kable(format = 'latex', booktabs = TRUE)


# Plots
# Components and performance
errors %>% 
  filter(error_type == 'valid',
         is.na(poly_degree) | poly_degree == 1) %>% 
  mutate(model_type = ifelse(str_detect(method, 'kPCA'), 'kPCA', model_type)) %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(n_components), y = error, fill = model_type), varwidth = TRUE) +
  geom_hline(yintercept = 1.8, linetype = 2) +
  theme_bw() +
  ggtitle(expression('Change in Reconstruction Quality with Increase in k'['geom'])) +
  xlab(expression('k'['geom'])) +
  ylab('Validation Chamfer Distance') +
  labs(color = 'Model', fill = 'Model', linetype = 'Model') +
  facet_grid(. ~ n_components, scales = "free_x")+ 
  theme(
    strip.text.x = element_blank()
  )
ggsave('n_components_geom_performance.png')
  # Generalisability %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 
errors %>% 
  # filter(n_components == 50) %>%  
  mutate(model_name = paste(model_type, 'k_geom =' , n_components)) %>% 
  ggplot(aes(x = error, y = model_name, fill = error_type)) +
  geom_boxplot() +
  geom_vline(xintercept = 1.8, linetype = 2) +
  theme_bw() +
  scale_x_continuous(limits = c(0,15))
