'''
Presenting geometric reconstruction results
'''
# Packages
library(tidyverse)
library(knitr)

# Loading and preparing data
Geometric_Model_Chamfer_Distances <- read_csv("Geometric_Model_Chamfer_Distances.csv")
colnames(Geometric_Model_Chamfer_Distances) <- c('geometric_model', 'train_chamfer_distance', 'valid_chamfer_distance')

Geometric_Model_Chamfer_Distances %>%
  mutate(geometric_model = str_replace(geometric_model, 'Geometric Deep Learning', 'GDL')) %>% 
  mutate(method= str_extract(geometric_model, '\\w+')) %>% 
  mutate(n_components = str_extract(geometric_model, '\\d+(?=( components| latent variables))')) %>% 
  mutate(poly_degree = str_extract(geometric_model, '(?<=degree )\\d+')) %>% 
  mutate(kernel = str_extract(geometric_model, '(RBF|poly|Sigmoid)')) %>% 
  mutate(model_type = ifelse(str_detect(geometric_model, 'kPCA'), paste(method, ' (', kernel, ')', sep=''), method)) %>% 
  mutate(model_type = ifelse(str_detect(geometric_model, 'poly'), paste(model_type, '($d =', poly_degree, '$)'), model_type)) -> Geometric_Model_Chamfer_Distances 

# As latex table
Geometric_Model_Chamfer_Distances %>% 
  select(model_type, n_components, train_chamfer_distance, valid_chamfer_distance) -> geometric_reconstruction_results_table

colnames(geometric_reconstruction_results_table) <- c('Model', 'Number of PCs/latents', 'Train', 'Valid')

geometric_reconstruction_results_table %>% 
  kable(format = 'latex', booktabs = TRUE, digits = 4) 

# Plot
# n_components on performance
Geometric_Model_Chamfer_Distances %>%
  filter(method == 'PCA' | method == 'GDL' | (kernel == 'poly' & poly_degree == 1)) %>% 
  mutate(n_components = as.integer(n_components)) %>% 
  ggplot(aes(group = method, x = n_components, y = valid_chamfer_distance, colour = method)) +
  geom_line(aes(linetype = method)) +
  geom_point(aes(shape = method), size = 3) +
  theme_bw() +
  ggtitle('Change in reconstruction quality with increase in components/latent variables') +
  xlab('Validation Chamfer Distance') +
  ylab('Number of components/latent variables') +
  labs(color = 'Model', shape = 'Model', linetype = 'Model')

ggsave('n_components_geom_performance.png')
# Generalisability
Geometric_Model_Chamfer_Distances %>% 
  pivot_longer(cols = c(train_chamfer_distance, valid_chamfer_distance), names_to = 'Split', values_to = 'chamfer_distance') %>% 
  mutate(Split = ifelse(str_detect(Split, 'train'), 'Train', 'Validation')) %>% 
  ggplot(aes(fill = Split, group = Split, y = geometric_model, x = chamfer_distance)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  xlab('Avg. Chamfer Distance') +
  ylab('Model') +
  ggtitle('Reconstruction quality on train and validation sets') +
  theme_bw()

ggsave('train_vs_valid_geometric.png')
