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

clinical_metrics %>% 
  group_by(population) %>% 
  summarise(n())
clinical_metrics %>% 
  filter(population == 'PCAxTimeVAE')


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
  select(-sd) -> clinical_metric_summary

# Distributional differences
models <- c("Baseline",           
            "PCAxPCA normal",
            "PCAxPCA truncnorm",
            "PCAxPCA KDE gaussian",
            "PCAxPCA KDE tophat",
            "PCAxTimeVAE ",
            "GDLxPCA normal",
            "GDLxPCA truncnorm",
            "GDLxPCA KDE gaussian",
            "GDLxPCA KDE tophat",
            "GDLxTimeVAE "
            )
models <- rev(models)

clinical_metrics %>% 
  filter(split == 'test') %>% 
  group_by(population) %>% 
  summarise(n())

clinical_metrics %>% 
  filter(split == 'test') %>%
  mutate(geom_method = str_extract(population, '^(PCA|GDL)')) %>% 
  mutate(temp_method = str_extract(population, '(?<=x)(PCA|TimeVAE)')) %>% 
  mutate(sampling_method = str_extract(population, 'truncnorm|normal|kde_tophat|kde_gaussian'),
         sampling_method = str_replace_all(sampling_method, '_', ' '),
         sampling_method = str_replace_all(sampling_method, 'kde', 'KDE'),
         sampling_method = ifelse(is.na(sampling_method), '', sampling_method)) %>% 
  mutate(population = ifelse(population == 'healthy', 'Baseline', paste(geom_method, 'x', temp_method, ' ', sampling_method, sep = ''))) %>% 
  mutate(population = factor(population, levels = models)) %>% 
  filter(metric %in% c('LV.mass', 'LVEF', 'RVEF')) %>%
  ggplot(aes(y = population, x = value, fill = population)) +
  geom_violin() +
  facet_grid(. ~ metric, scales = 'free') +
  theme_bw()+
  theme(legend.position = 'none') +
  xlab('Metric Value') +
  ylab('Sampling Method') +
  ggtitle('Distributions of Sampled Clinical Metrics')

ggsave('clinical_metric_dists.pdf')
 
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
  mutate(old_population = population) %>% 
  filter(metric %in% c('LV.mass', 'LVEF', 'RVEF')) %>% 
  mutate(geom_method = str_extract(population, '^(PCA|GDL)')) %>% 
  mutate(temp_method = str_extract(population, '(?<=x)(PCA|TimeVAE)')) %>% 
  mutate(sampling_method = str_extract(population, 'truncnorm|normal|kde_tophat|kde_gaussian'),
         sampling_method = str_replace_all(sampling_method, '_', ' '),
         sampling_method = str_replace_all(sampling_method, 'kde', 'KDE'),
         sampling_method = ifelse(is.na(sampling_method), '', sampling_method)) %>% 
  mutate(population = ifelse(population == 'healthy', 'Baseline', paste(geom_method, 'x', temp_method, ' ', sampling_method, sep = ''))) %>% 
  mutate(ks_result = round(ks_result, digits = 3)) %>% 
  pivot_wider(names_from = metric, values_from = ks_result, names_prefix = 'KS_') -> ks_results

clinical_metric_summary %>% 
  filter(metric %in% c('LV.mass', 'LVEF', 'RVEF')) %>%
  merge(ks_results, by.x = 'population', by.y = 'old_population', all.x = T) %>% 
  pivot_wider(names_from = metric, values_from = mean) %>% 
  mutate(population.y = as.character(population.y),
         population.y = ifelse(is.na(population.y), 'Baseline', population.y)) %>% 
  mutate(population.y = factor(population.y, levels = rev(models))) %>% 
  arrange(population.y) %>% 
  select(geom_method, temp_method, sampling_method, LV.mass, LVEF, RVEF, KS_LV.mass, KS_LVEF, KS_RVEF) %>% 
    kable(format = 'latex', booktabs = T)

# MMD COV 1-NNA

gen_metrics <- read.csv('/home/wolf6273/4D_geom/checkpoints/generative_metrics/metrics.csv')
gen_metrics

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
gen_metrics

gen_metrics %>% 
  select(-exp_name) %>% 
  mutate(models = factor(models, levels = c('Baseline', 'PCAxPCA', 'PCAxTimeVAE', 'GDLxPCA', 'GDLxTimeVAE'))) %>% 
  arrange(models) %>% 
  kable(format = 'latex', booktabs = T, digits = 3)
