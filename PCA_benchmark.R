###
# PCA comparative analysis
###

# Load latents
prevalent_MI_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/prevalent_MI_ED_latents.csv')
prevalent_MI_ED %>% mutate(data = 'prevalent_MI_ED') -> prevalent_MI_ED
prevalent_MI_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/prevalent_MI_ES_latents.csv')
prevalent_MI_ES %>% mutate(data = 'prevalent_MI_ES') -> prevalent_MI_ES
incident_MI_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/incident_MI_ED_latents.csv')
incident_MI_ED %>% mutate(data = 'incident_MI_ED') -> incident_MI_ED
incident_MI_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/incident_MI_ES_latents.csv')
incident_MI_ES %>% mutate(data = 'incident_MI_ES') -> incident_MI_ES
healthy_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/healthy_ED_latents.csv')
healthy_ED %>% mutate(data = 'healthy_ED') -> healthy_ED
healthy_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/healthy_ES_latents.csv')
healthy_ES %>% mutate(data = 'healthy_ES') -> healthy_ES

geom_latents <- rbind(prevalent_MI_ED,
                      prevalent_MI_ES,
                      incident_MI_ED,
                      incident_MI_ES,
                      healthy_ED,
                      healthy_ES)

geom_latents

# Histograms
geom_latents %>% 
  pivot_longer(cols = 1:12, names_to = 'latent_var') %>% 
  filter(str_detect(data, 'ED')) %>% 
  ggplot(aes(x = value, fill = data)) +
  geom_histogram() +
  facet_wrap(~ latent_var)

geom_latents %>% 
  pivot_longer(cols = 1:12, names_to = 'latent_var') %>% 
  filter(str_detect(data, 'ES')) %>% 
  ggplot(aes(x = value, fill = data)) +
  geom_histogram() +
  facet_wrap(~ latent_var)

# Logistic regression
library(MASS)

# ED regression for incident
geom_latents %>% 
  filter(str_detect(data, 'ED'),
         str_detect(data, 'incident|healthy')) %>% 
  mutate(healthy = as.factor(data)) %>% 
  dplyr::select(-data) -> lr_data

lr_data %>% 
  summarise(is_healthy = sum(str_detect(healthy, 'healthy')), is_MI = sum(str_detect(healthy, 'MI')))
  
incident_lr <- glm(healthy ~ ., data = lr_data, family=binomial())
summary(incident_lr)

incident_lr_step <- incident_lr %>% 
  stepAIC()

summary_incident <- summary(incident_lr_step)

# ctrl <- trainControl(method = 'cv', number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     savePredictions = T,
                     classProbs = T,
                     verboseIter = T)

model <- train(healthy ~ ., data = lr_data, method = 'glm', family='binomial', trControl = ctrl)

evalm(model)

# ES
geom_latents %>% 
  filter(str_detect(data, 'ES'),
         str_detect(data, 'incident|healthy')) %>% 
  mutate(healthy = as.factor(data)) %>% 
  dplyr::select(-data) -> lr_data

lr_data %>% 
  summarise(is_healthy = sum(str_detect(healthy, 'healthy')), is_MI = sum(str_detect(healthy, 'MI')))
  
incident_lr <- glm(healthy ~ ., data = lr_data, family=binomial())
summary(incident_lr)

incident_lr_step <- incident_lr %>% 
  stepAIC()

summary_incident <- summary(incident_lr_step)

# ctrl <- trainControl(method = 'cv', number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     savePredictions = T,
                     classProbs = T,
                     verboseIter = T)

model <- train(healthy ~ ., data = lr_data, method = 'glm', family='binomial', trControl = ctrl)

evalm(model)
