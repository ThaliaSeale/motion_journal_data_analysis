###
# Clinical Application
###

library(tidyverse)

# Load MI data
MI_errors <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_24_components_MI/errors.csv')
MI_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_24_components_MI/temporal_representations.csv')

# Load train and test data
train_test_errors <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_24_components/errors.csv')
train_test_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_24_components/temporal_representations.csv')

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

# COMMENTS: There is a slight reduction in reconstruction quality and variability in the result, however it is not huge, not statistically significant.

# Latent space analysis 
MI_latents %>% 
  pivot_longer(2:(last_col()-1), names_to = 'latent_var') %>% 
  mutate(latent_var = as.numeric(str_extract(latent_var, '\\d+')),
         data = 'MI') %>% 
  filter(latent_var <= 16) -> MI_latents # only look at first 16 latent variables
  
train_test_latents %>% 
  pivot_longer(2:(last_col()-1), names_to = 'latent_var') %>% 
  mutate(latent_var = as.numeric(str_extract(latent_var, '\\d+')),
         data = 'Healthy') %>% 
  filter(latent_var <= 16) -> train_test_latents # only look at first 16 latent variables

latents <- rbind(MI_latents,
                 train_test_latents)

## Plot histograms
latents %>% 
  ggplot(aes(x = value, fill = data)) +
  geom_histogram() +
  facet_wrap(~ latent_var, scales='free_x')
  
?geom_hline

## Summary statistics
latents %>%
  group_by(latent_var, data) %>% 
  summarise(mean = mean(value), sd = sd(value)) %>% 
  pivot_wider(names_from= data, values_from = c(mean, sd))


# Logistic regression
library(MASS)

latents  %>%
  pivot_wider(names_from = latent_var, values_from = value, names_prefix = "latent_") %>% 
  mutate(healthy = as.factor(data)) %>% 
  dplyr::select(-X, -split, -data) %>% 
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
