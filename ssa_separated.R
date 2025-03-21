# Separated features results

library(tidyverse)

# Load data
time_series_traversal_separated_features <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_separated_features/time_series_traversal.csv')

time_series_traversal_separated_features

time_series_traversal_separated_features %>%
  filter(temp_dim < 14) %>% 
  ggplot(aes(y = value, x = frame, color = sd, group = sd)) +
  geom_line() +
  facet_grid(geom_dim ~ temp_dim, scales = 'free_y') +
  theme_bw() +
  scale_color_viridis_b() +
  xlab('Frame') +
  ylab('Value') +
  ggtitle('SSA (separated) on PCA (k_geom = 16)')

ggsave('PCA_separated_features.png')