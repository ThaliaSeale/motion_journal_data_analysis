
###
# Clinical Application (sep features timeVAE)
###

library(tidyverse)
library(knitr)

# Load MI data
# MI_errors <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_gdl_5_incident/errors.csv')
# MI_latents <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_gdl_5_incident/mus.csv')
MI_errors <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_pca_5_incident/errors.csv')
MI_latents <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_pca_5_incident/mus.csv')

# Load train and test data
# train_test_errors <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_gdl_5/errors.csv')
# train_test_latents <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_gdl_5/mus.csv')
train_test_errors <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_pca_5/errors.csv')
train_test_latents <- read.csv('/home/wolf6273/4D_geom/checkpoints/temporal/sparse_pca_5/mus.csv')

# Overall error comparison
MI_errors %>% 
  mutate(data = 'incident_MI') %>% 
  dplyr::select(-model) -> MI_errors

train_test_errors %>% 
  filter(error_type == 'valid') %>% 
  mutate(data = 'healthy') %>% 
  dplyr::select(-model) -> healthy_errors

errors <- rbind(MI_errors,
                healthy_errors)

errors %>% 
  group_by(data) %>% 
  distinct(sample) %>% 
  summarise(n_samples = n())

errors %>% 
  group_by(data, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(data, sample) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(data) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  kable(format = 'latex', booktabs = T, digits = 3)

# COMMENTS: There is a slight reduction in reconstruction quality, the variability is similar. However it is not huge.

# Latent space analysis 
MI_latents %>% 
  mutate(sample = row_number()) %>% 
  pivot_longer(1:(last_col()-1), names_to = 'latent_var') %>% 
  mutate(latent_var = as.numeric(str_extract(latent_var, '\\d+')),
         data = 'MI') -> MI_latents
  
train_test_latents %>% 
  mutate(sample = row_number()) %>% 
  pivot_longer(1:(last_col()-1), names_to = 'latent_var') %>% 
  mutate(latent_var = as.numeric(str_extract(latent_var, '\\d+')),
         data = 'Healthy') -> train_test_latents 

latents <- rbind(MI_latents,
                 train_test_latents)

## Plot histograms for first 16 variables
latents %>% 
  filter(latent_var <= 15) %>% 
  ggplot(aes(x = value, fill = data)) +
  geom_histogram() +
  facet_wrap(~ latent_var, scales='free_x')

## Boxplots
latents %>% 
  filter(latent_var <= 15) %>% 
  ggplot(aes(y = value, fill = data)) +
  geom_boxplot() +
  facet_wrap(~ latent_var, scales='free')

## COMMENTS: Not much difference in distribution.

## Summary statistics
### Means
latents %>%
  group_by(latent_var, data) %>% 
  summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = data, values_from = mean) -> latent_means 
latent_means

### Checking for normality
latents %>%
  group_by(latent_var, data) %>% 
  summarise(shapiro_p_value = shapiro.test(value)$p.value) %>% 
  mutate(sig = shapiro_p_value < 0.05)

### COMMENTS: Doesn't follow normality assumptions. So we should maybe use Wilcoxon to check means?

### Wilcoxon signed rank
n_latents <- length(unique(latents$latent_var))
latents %>% 
  group_by(latent_var) %>% 
  summarise(wilcox_p_value = wilcox.test(value ~ data)$p.value) %>% 
  mutate(sig = (wilcox_p_value > 0.1/n_latents) * 0 + (wilcox_p_value < 0.1/n_latents & wilcox_p_value > 0.05/n_latents) * 1 + (wilcox_p_value < 0.05/n_latents & wilcox_p_value > 0.001/n_latents) * 2 + (wilcox_p_value < 0.001/n_latents & wilcox_p_value > 0.0001/n_latents) * 3 + (wilcox_p_value < 0.0001/n_latents) * 4) %>% 
  filter(sig > 0) -> wilcoxon_latent_results

latent_means %>% 
  filter(latent_var %in% wilcoxon_latent_results$latent_var) 

# Logistic regression
library(MASS)

latents  %>%
  pivot_wider(names_from = latent_var, values_from = value, names_prefix = "latent_") %>% 
  mutate(healthy = as.factor(data)) %>% 
  dplyr::select(-sample, -data) %>% 
  relocate(healthy, .before = 1) -> lr_data

incident_lr <- glm(healthy ~ ., data = lr_data, family=binomial())
summary(incident_lr)

incident_lr_step <- incident_lr %>% 
  stepAIC()

summary_incident <- summary(incident_lr_step)
summary_incident
# COMMENTS: Add interpretation here.

## Goodness-of-fit
### Deviance
incident_full_model <- incident_lr_step
incident_null_model <- glm(healthy ~ 1, data = lr_data, family=binomial())

anova(incident_null_model, incident_full_model)

### Prediction accuracy
# install.packages('caret')
library(caret)
# install.packages('MLeval')
library(MLeval)

# ctrl <- trainControl(method = 'cv', number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     savePredictions = T,
                     classProbs = T,
                     verboseIter = T)

model <- train(healthy ~ ., data = lr_data, method = 'glm', family='binomial', trControl = ctrl)

lr_data %>% 
  summarise(n_healthy = sum(healthy == 'Healthy'), n_incident = sum(healthy == 'MI'))

evalm(model)
