library(tidyverse)
library(knitr)

# Loading files
MI_detection <- read.csv('MI_detection.csv')
MI_detection %>% 
  pivot_longer(cols = c(contains('latent'), contains('errors')), values_to = 'file_path') -> MI_detection
MI_detection %>% 
  mutate(status = str_extract(name, 'iMI|pMI|healthy'), data_series = str_extract(name, 'latents|errors')) %>% 
  dplyr::select(-name) -> MI_detection

MI_detection %>%
  filter(data_series == 'errors') %>% 
  dplyr::select(-data_series) -> MI_errors
MI_errors %>% 
  drop_na() -> MI_errors

MI_detection %>%
  filter(data_series == 'latents') %>% 
  dplyr::select(-data_series) -> MI_latents

error_data <- lapply(MI_errors$file_path, read.csv)
error_data <- Map(function(df, file_path) {
  df$file_path <- file_path 
  df
}, error_data, MI_errors$file_path)
error_data <- error_data %>% bind_rows() # correct when all results found 
error_data %>% 
  merge(MI_errors) %>% 
  dplyr::select(-model, -file_path) -> error_data

detection_data <- lapply(MI_latents$file_path, read.csv)
detection_data <- Map(function(df, file_path) {
  df$file_path <- file_path 
  if('X' %in% colnames(df)){
    df %>% 
      dplyr::select(-X) -> df
  }
  df %>% 
    rownames_to_column() %>% 
    rename(subject = rowname) %>% 
    pivot_longer(cols = contains('X'), names_to = 'latent_var') -> df
  df
}, detection_data, MI_latents$file_path)
detection_data <- detection_data %>% bind_rows() # correct when all results found 
detection_data %>% 
  merge(MI_latents) %>% 
  dplyr::select(-file_path) -> detection_data 
detection_data %>% 
  filter(geom_method %in% c('PCA_ED', 'PCA_ES')) %>% 
  mutate(latent_var = paste(latent_var, str_extract(geom_method, '(ED|ES)'), sep='_'),
         geom_method = 'PCA_COMBINED') -> PCA_COMBINED_LATENTS
rbind(detection_data,
      PCA_COMBINED_LATENTS) -> detection_data

# Error analysis
# Error table
error_data %>% 
  filter(error_type == 'test') %>% 
  group_by(sample, frame, geom_method, geom_dims, temp_method, temp_dims, status) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(sample, geom_method, geom_dims, temp_method, temp_dims, status) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(geom_method, geom_dims, temp_method, temp_dims, status) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss))  %>% 
  mutate(across(c(mean_loss, sd_loss), round, digits = 3)) %>% 
  mutate(error = paste(mean_loss, '(X', sd_loss, ')')) %>% 
  dplyr::select(-contains('loss')) %>% 
  pivot_wider(names_from = status, values_from = error) %>% 
  mutate(temp_method = factor(temp_method, levels = c('PCA', 'PCAS', 'TimeVAE', 'TimeVAES')),
         geom_method = factor(geom_method, levels = c('PCA', 'GDL'))) %>% 
  arrange(temp_method, geom_method) %>% 
  kable(format = 'latex', digits = 3, booktabs = T)

# Save latent csv
detection_data %>% 
  write.csv('detection_data.csv')

# Load results
results_iMI <- read.csv('results_iMI.csv')
results_pMI <- read.csv('results_pMI.csv')

results_iMI %>% 
  kable(format = 'latex', digits = 3, col.names = c('Model', 'Acc.', 'Prec.', 'Rec.', 'F1', 'AUC'), booktabs = T)

results_pMI %>% 
  kable(format = 'latex', digits = 3, col.names = c('Model', 'Acc.', 'Prec.', 'Rec.', 'F1', 'AUC'), booktabs = T)
  

# AIC
## PCAxPCA
### iMI
library(MASS)