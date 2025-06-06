###
# Clinical Metrics
###

library(tidyverse)
library(knitr)

# Load paths
clinical_metric_files <- read_csv("clinical_metric_files.csv")

# Loading metrics
clinical_metrics <- lapply(clinical_metric_files$metrics_path, read.csv)
## Merging with model_results
clinical_metrics <- Map(function(df, population) {
  df$population <- population 
  df
  }, clinical_metrics, clinical_metric_files$population)
clinical_metrics <- clinical_metrics %>% bind_rows() # correct when all results found 

# Checking for NAs and that we have volumes for each
clinical_metrics %>% 
  drop_na() -> clinical_metrics
# Healthy
which(!(0:421 %in% filter(clinical_metrics, split == 'train', population == 'healthy')$i))
which(!(0:52 %in% filter(clinical_metrics, split == 'valid', population == 'healthy')$i))
which(!(0:52 %in% filter(clinical_metrics, split == 'test', population == 'healthy')$i))
# PCAxPCA
which(!(0:499 %in% filter(clinical_metrics, split == 'test', population == 'PCAxPCA')$i))

# Calculating derived metrics
clinical_metrics %>% 
  mutate(across(LVEDV:LVepiEDV, ~ .x/1000)) %>% 
  mutate(LVEF = (LVEDV - LVESV)/LVEDV,
         RVEF = (RVEDV - RVESV)/RVEDV,
         LV.mass = (LVepiEDV - LVEDV) * 1.05) -> clinical_metrics

# Histogram
clinical_metrics %>% 
  pivot_longer(cols = starts_with(c('LV', 'RV')), names_to = 'metric') %>% 
  mutate(metric = factor(metric, levels = c('LVEDV', 'LVESV', 'LV.mass', 'LVEF', 'RVEDV', 'RVESV', 'RVEF'))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~ metric, scales = 'free') 

# Check bad values
clinical_metrics %>% 
  filter(RVESV > 150)

clinical_metrics %>% 
  filter(!(i %in% c(29, 345))) -> clinical_metrics # bad reconstructions

# Filter bad EFs (EF < 0)
clinical_metrics %>% 
  filter(LVEF < 0 | RVEF < 0) # 1 bad 

clinical_metrics %>% 
  filter(LVEF > 0,
         RVEF > 0) -> clinical_metrics


# Summary statistics
clinical_metrics %>% 
  mutate(subject = row_number()) %>% 
  pivot_longer(cols = starts_with(c('LV', 'RV')), names_to = 'metric') -> clinical_metrics

clinical_metrics %>% 
  group_by(population, metric) %>%
  summarise(mean = mean(value), sd = sd(value)) -> clinical_metric_summary

clinical_metric_summary %>% 
  filter(metric !=  'LVepiEDV') %>% 
  mutate(metric = factor(metric, levels = c('LVEDV', 'LVESV', 'LV.mass', 'LVEF', 'RVEDV', 'RVESV', 'RVEF'))) %>% 
  arrange(metric) %>% 
  mutate(across(c(mean, sd), ~ round(.x , digits = 2)),
         mean = paste(mean, ' (X ', sd, ')', sep = '')) %>%
  select(-sd) %>%
  pivot_wider(names_from = population, values_from = mean) -> clinical_metric_summary
clinical_metric_summary

# Distributional differences
clinical_metrics %>% 
  # filter(split == 'test') %>% 
  ggplot(aes(x = population, y = value, fill = population)) + 
  geom_violin() + 
  facet_wrap(~ metric, scales = 'free') 
 
ks_test <- function(Population, Metric){
  x <- (clinical_metrics %>% 
    filter(population == 'healthy', metric == Metric, split == 'test'))$value
  y <- (clinical_metrics %>% 
    filter(population == Population, metric == Metric))$value
  test_result <- ks.test(x,y)
  test_result$p.value
}

clinical_metrics %>% 
  select(population, metric) %>% 
  filter(population != 'healthy') %>% 
  distinct() -> ks_results
ks_results$ks_result <- Map(ks_test, ks_results$population, ks_results$metric)
ks_results %>% 
  mutate(ks_result = as.numeric(ks_result)) -> ks_results
ks_results %>% 
  filter(population == 'PCAxPCA_truncnorm')
ks_results %>% 
  filter(population == 'PCAxPCA_tophat')


# MMD COV 1-NNA

gen_metrics <- read.csv('/home/wolf6273/4D_geom/checkpoints/generative_metrics/metrics.csv')

models <- c('PCAxPCA',
            'PCAxTimeVAE',
            'GDLxPCA',
            'PCAxPCA',
            'GDLxPCA',
            'GDLxPCA',
            'GDLxTimeVAE',
            'Baseline',
            'PCAxPCA',
            'GDLxPCA',
            'PCAxPCA')

sampling_method <- c('KDE (Tophat)',
                     'GMM',
                     'Trunc. Norm.',
                     'Trunc. Norm.',
                     'Norm.',
                     'KDE (Gaussian)',
                     'GMM',
                     '',
                     'KDE (Gaussian)',
                     'KDE (Tophat)',
                     'Norm.')

gen_metrics <- cbind(models, sampling_method, gen_metrics)

gen_metrics %>% 
  select(-exp_name) %>% 
  arrange(models) %>% 
  kable(format = 'latex', booktabs = T, digits = 3)
