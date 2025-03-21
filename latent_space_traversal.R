###
# Time series latent traversal
###

library(tidyverse)
library(ggh4x)

# Load data
model_results <- read_csv("model_results.csv")

# Removing missing results
model_results <- model_results %>% 
  select(-experiment_description) %>% drop_na()

# Loading and cleaning error data
## Loading errors
model_results %>% 
  mutate(latent_traversal_file = 'time_series_traversal.csv',
         latent_traversal_file = paste(results_dir, '/', latent_traversal_file, sep = '')) -> model_results
latent_traversals <- lapply(model_results$latent_traversal_file, read_csv)

## Merging with model_results
latent_traversals <- Map(function(df, val) {
  df$results_dir <- val
  df
}, latent_traversals, model_results$results_dir)
latent_traversals <- latent_traversals %>% bind_rows() %>% select(-1) # correct when all results found 
latent_traversals <- merge(model_results,latent_traversals, by='results_dir')
latent_traversals %>% 
  select(-c(results_dir, latent_traversal_file, model)) -> latent_traversals
latent_traversals %>%
  filter(frame == 0) %>%
  mutate(frame = ifelse(str_detect(temp_model, 'PCA'), 10, 50),
         frame = as.integer(frame)) %>% 
  bind_rows(latent_traversals) -> latent_traversals

# Plots
latent_traversals %>% 
  filter(temp_model == 'PCA', geom_model == 'PCA',
         temp_dim %in% c(0, 1, 7, 9),
         geom_dim <= 3) %>%
         # temp_dim <= 15,
         # geom_dim <= 15) %>%
  ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
  geom_line() +
  facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
  theme_bw() +
  xlab('Frame') +
  ylab('Value') +
  scale_x_continuous(breaks = c(0,5,10)) +
  scale_color_viridis_b('SD', show.limits = T) +
  ggtitle('Latent Space Traversal, PCAxPCA')

ggsave('latent_traversal_PCAxPCA.pdf', width = 12, height = 5)

latent_traversals %>% 
  filter(temp_model == 'PCA', geom_model == 'GDL',
         temp_dim <= 16,
         temp_dim %in% c(0, 4, 6, 10),
         geom_dim <= 3) %>% 
  ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
  geom_line() +
  facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
  theme_bw() +
  xlab('Frame') +
  ylab('Value') +
  scale_x_continuous(breaks = c(0,5,10)) +
  scale_color_viridis_b('SD', show.limits = T) +
  ggtitle('Latent Space Traversal, GDLxPCA')

ggsave('latent_traversal_GDLxPCA.pdf', width = 12, height = 5)

latent_traversals %>% 
  filter(temp_model == 'TimeVAE', geom_model == 'PCA',
         temp_dim %in% c(3,4,11,15),
         geom_dim <= 3) %>%
  ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
  geom_line() +
  facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
  theme_bw() +
  xlab('Frame') +
  ylab('Value') +
  scale_x_continuous(breaks = c(0,25,50)) +
  scale_color_viridis_b('SD', show.limits = T) +
  ggtitle('Latent Space Traversal, PCAxTimeVAE')

ggsave('latent_traversal_PCAxTimeVAE.pdf', width = 12, height = 5)

latent_traversals %>% 
  filter(temp_model == 'TimeVAE', geom_model == 'GDL',
         temp_dim %in% c(1, 14, 9, 13),
         geom_dim <= 3) %>%
  # filter(temp_model == 'TimeVAE', geom_model == 'GDL') %>% 
  ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
  geom_line() +
  facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
  theme_bw() +
  xlab('Frame') +
  ylab('Value') +
  scale_x_continuous(breaks = c(0,25,50)) +
  scale_color_viridis_b('SD', show.limits = T) +
  ggtitle('Latent Space Traversal, GDLxTimeVAE')

ggsave('latent_traversal_GDLxTimeVAE.pdf')

# latent_traversals %>% 
#   filter(temp_model == 'PCA_sep', geom_model == 'PCA',
#          temp_dim <= 10,
#          geom_dim <= 15) %>%
#   ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
#   geom_line() +
#   facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
#   theme_bw() +
#   xlab('Frame') +
#   ylab('Value') +
#   scale_x_continuous(breaks = c(0,5,10)) +
#   scale_color_viridis_b('SD', show.limits = T) +
#   ggtitle('Latent Space Traversal, PCAxPCAS')
# 
# ggsave('latent_traversal_PCAxPCAS.pdf')

latent_traversals %>% 
  filter(temp_model == 'PCA_sep', geom_model == 'PCA',
         temp_dim <= 10,
         geom_dim <= 15) %>%
  ggplot(aes(x = frame, y = value, color = sd, group = sd)) +
  geom_line() +
  facet_nested('Geom. Dim.' + geom_dim ~ 'Temp. Dim.' + temp_dim, scales = 'free_y') +
  theme_bw() +
  xlab('Frame') +
  ylab('Value') +
  scale_x_continuous(breaks = c(0,5,10)) +
  scale_color_viridis_b('SD', show.limits = T) +
  ggtitle('Latent Space Traversal, PCAxPCAS')

ggsave('latent_traversal_PCAxPCAS.pdf', width = 10, height = 4)
