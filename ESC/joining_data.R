library(tidyverse)
library(data.table)

# Loading latents
healthy_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features/temporal_representations.csv')
iMI_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_iMI/temporal_representations.csv')
pMI_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_pMI/temporal_representations.csv')

# Loading split lists
healthy_pids <- c(
  read_lines('~/data/healthy_reconstructed_frames_combined/train.list'),
  read_lines('~/data/healthy_reconstructed_frames_combined/valid.list'),
  read_lines('~/data/healthy_reconstructed_frames_combined/test.list')
)

iMI_pids <- read_lines('/home/wolf6273/data/Incident-MI_reconstructed_frames_combined/test.list')
pMI_pids <- read_lines('/home/wolf6273/data/Prevalent-MI_reconstructed_frames_combined/test.list')

# Adding pid to dataframe
cbind(healthy_latents, healthy_pids) %>% 
  dplyr::select(-X) %>%
  rename(eid = healthy_pids) %>% 
  relocate(eid, split) -> healthy_latents
healthy_latents %>% 
  mutate(status = 'healthy') -> healthy_latents

cbind(iMI_latents, iMI_pids) %>% 
  dplyr::select(-X) %>% 
  rename(eid = iMI_pids) %>% 
  relocate(eid, split) -> iMI_latents
iMI_latents %>% 
  mutate(status = 'iMI') -> iMI_latents
  
cbind(pMI_latents, pMI_pids) %>% 
  dplyr::select(-X) %>% 
  rename(eid = pMI_pids) %>% 
  relocate(eid, split) -> pMI_latents
pMI_latents %>% 
  mutate(status = 'pMI') -> pMI_latents

# Loading metadata
# meta_data <- fread('/home/wolf6273/data/UKB/ukb45078.csv')
# gender <- meta_data %>% 
#   select(eid, `31-0.0`) %>%  # https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=31
#   rename(sex = `31-0.0`)
# 
# write.csv(gender, 'gender.csv')
gender <- read.csv('gender.csv')
gender %>% 
  dplyr::select(-X) -> gender

# Join data
dataset <- rbind(healthy_latents,
                 iMI_latents,
                 pMI_latents)

dataset %>% 
  merge(gender) -> dataset

# Check for missing data
dataset %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))
  
# Loading clinical metrics
healthy_metrics <- read.csv('/home/wolf6273/4D_geom/checkpoints/clinical_metrics/clinical_metrics_train_metrics.csv')
iMI_metrics <- read.csv('/home/wolf6273/4D_geom/checkpoints/clinical_metrics/iMI_metrics.csv')
pMI_metrics <- read.csv('/home/wolf6273/4D_geom/checkpoints/clinical_metrics/pMI_metrics.csv')

healthy_metrics %>% 
  drop_na() %>% 
  mutate(split = factor(split, c('train', 'valid', 'test'))) %>% 
  group_by(split) %>% 
  arrange(i, .by_group = T) -> healthy_metrics
healthy_metrics$i <- healthy_pids

iMI_metrics %>% 
  drop_na() %>% 
  mutate(split = factor(split, c('train', 'valid', 'test'))) %>% 
  group_by(split) %>% 
  arrange(i, .by_group = T) -> iMI_metrics
iMI_metrics$i <- iMI_pids 

pMI_metrics %>% 
  drop_na() %>% 
  mutate(split = factor(split, c('train', 'valid', 'test'))) %>% 
  group_by(split) %>% 
  arrange(i, .by_group = T) -> pMI_metrics
pMI_metrics$i <- pMI_pids

clinical_metrics <- rbind(healthy_metrics,
                      iMI_metrics,
                      pMI_metrics)
clinical_metrics %>% 
  rename(eid = i) -> clinical_metrics

# Knitting datasets
dataset %>% 
  merge(clinical_metrics) -> dataset 

# Adding metrics
dataset %>% 
  mutate(LVEF = (LVEDV - LVESV)/LVEDV,
         RVEF = (RVEDV - RVESV)/RVEDV) -> dataset

# Saving cleaned dataset
write.csv(dataset, 'combined_dataset.csv')
dataset %>% 
  filter(sex == 0, status == 'iMI')
