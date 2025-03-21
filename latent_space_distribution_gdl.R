# Histogram of the GDL latent space
mus <- read.csv('/home/wolf6273/4D_geom/checkpoints/geometric/gdl_50_latents/mus.csv')
mus %>% 
  pivot_longer(cols = everything(), names_to = 'latent_variable') %>% 
  mutate(latent_variable = str_extract(latent_variable, '\\d+')) -> mus 

mus %>%
  mutate(latent_var_int = as.integer(latent_variable)) %>% 
  filter(latent_var_int <= 16) %>% 
  ggplot(aes(x = value, fill = latent_variable)) + 
  geom_histogram() +
  facet_wrap(. ~ latent_variable)
