###
# Temporal reconstruction error
###

# Packages
library(tidyverse)
library(moments)

# Results
model_results <- read_csv("model_results.csv")

# Removing missing results
model_results <- model_results %>% 
  select(-experiment_description) %>% drop_na()

# Loading and cleaning error data
## Loading errors
error_files <- paste(model_results$results_dir, '/errors.csv', sep = '')
error_tables <- lapply(error_files, read_csv)
## Merging with model_results
error_tables <- Map(function(df, val) {
  df$results_dir <- val
  df
}, error_tables, model_results$results_dir)
error_tables <- error_tables %>% bind_rows() # correct when all results found 
error_tables <- merge(model_results,error_tables, by='results_dir')
error_tables %>% 
  select(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame, substructure, loss, error_type) -> error_tables
error_tables

# Overall reconstruction results
# Table for average reconstruction loss for each sample
error_tables %>% 
  select(-n_temp_feats, -n_geom_feats) %>%
  group_by(temp_model, geom_model, sample, frame, error_type) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, geom_model, sample, error_type) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, geom_model, error_type) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  mutate(loss = paste(round(mean_loss, 2), '(X', round(sd_loss, 2), ')')) %>% 
  select(-mean_loss, -sd_loss) %>% 
  kable(format = 'latex', booktabs = T)

error_tables %>% 
  mutate(model = paste(geom_model, 'x', temp_model, sep = "")) %>% 
  group_by(model, sample, error_type) %>% 
  summarise(loss = mean(loss)) %>%
  ggplot(aes(x = model, fill = error_type, y = loss)) +
  geom_boxplot() +
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(. ~ model, scales = 'free_x') +
  theme_bw() +
  ggtitle('Avg reconstruction loss for each sample over the full cardiac cycle reconstruction') +
  labs(fill = 'Split') +
  xlab('Model') +
  ylab('Mean Chamfer distance (mm)')

# Substructure results
error_tables %>% 
  select(-n_temp_feats, -n_geom_feats) %>%
  group_by(temp_model, geom_model, substructure, error_type) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) -> substructure_results


substructure_results %>% 
  ungroup() %>% 
  mutate(model = paste(temp_model, 'x', geom_model, sep = '')) %>% 
  mutate(loss = paste(format(round(mean_loss, 2), nsmall = 2), '(X', format(round(sd_loss, 2), nsmall = 2), ')')) %>% 
  select(-temp_model, -geom_model, -mean_loss, -sd_loss) %>% 
  pivot_wider(names_from = substructure, values_from = c(loss)) %>% 
  kable(format = 'latex', booktabs = T)

error_tables %>% 
  mutate(model = paste(geom_model, '+', temp_model)) %>%  
  filter(error_type == 'valid') %>% 
  mutate(across(where(is.character), ~ str_replace_all(., "_", " "))) %>% 
  ggplot(aes(x = model, fill = substructure, y = loss)) +
  geom_boxplot() + 
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(. ~ model, scales = 'free_x') +
  theme_bw() +
  ggtitle('Avg reconstruction loss of each substructure over the full cardiac cycle reconstruction') +
  labs(fill = 'Substructure') +
  xlab('Model') + 
  ylab('Mean Chamfer distance (mm)')

# Frame results
{
temp_model <- list()
geom_model <- list()
F_values <- list()
p_values <- list()
for(t_model in unique(error_tables$temp_model)){
  for(g_model in unique(error_tables$geom_model)){
    temp_model <- append(temp_model, t_model)
    geom_model <- append(geom_model, g_model)
    # print(t_model)
    # print(g_model)
    temp_geom_table <- error_tables %>% 
      filter(temp_model == t_model, geom_model == g_model, 
             error_type == 'valid') %>% 
      group_by(sample, frame) %>% 
      summarise(loss = mean(loss))
    frame.aov <- aov(loss ~ frame, data = temp_geom_table)
    summary.frame.aov <- summary(frame.aov)
    # print(summary.frame.aov[[1]]$`F value`[[1]])
    # print(summary.frame.aov[[1]]$`Pr(>F)`[[1]])
    F_values <- append(F_values, summary.frame.aov[[1]]$`F value`[[1]])
    p_values <- append(p_values, summary.frame.aov[[1]]$`Pr(>F)`[[1]])
  }
}
frame_anova <- as.data.frame(cbind(temp_model, geom_model, F_values, p_values))
  }


frame_anova  %>% 
  mutate(signif = ifelse(p_values > 0.1, "", ifelse(p_values > 0.05, ".", ifelse(p_values > 0.01, "*", ifelse(p_values > 0.001, "**", "***"))))) -> frame_anova

error_tables %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, frame) %>% 
  summarise(mean_loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats) %>% 
  summarise(sd_loss_across_frames = sd(mean_loss)) %>% 
  select(temp_model, geom_model, sd_loss_across_frames) -> frame_sds

frame_anova %>%
  merge(frame_sds) %>% 
  select(-n_temp_feats) %>% 
  relocate(sd_loss_across_frames, .after = geom_model)  %>% 
  mutate(across(c(sd_loss_across_frames, F_values), ~ round(as.numeric(.x), digits = 2))) %>% 
  mutate(p_values = signif(as.numeric(p_values), digits = 3)) %>%  
  mutate(model = paste(temp_model, 'x', geom_model, sep = '')) %>% 
  relocate(model, .before = 0)  %>% 
  select(-temp_model, -geom_model) %>% 
  kable(format = 'latex', booktabs = T)

error_tables %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, frame) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  mutate(loss = paste(round(mean_loss,2), '(X', round(sd_loss,2), ')')) %>% 
  select(-mean_loss, -sd_loss) %>% 
  pivot_wider(values_from = loss, names_from = frame)

error_tables %>% 
  mutate(model = paste(geom_model, 'x', temp_model, sep = ""),
         frame = as.factor(frame)) %>%  
  mutate(model = str_replace_all(model, '_', ' ')) %>% 
  filter(error_type == 'valid') %>% 
  group_by(model, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  ggplot(aes(x = frame, y = loss, fill = model)) +
  geom_boxplot() + 
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(model ~ .) +
  theme_bw() +
  ggtitle('Avg reconstruction loss for each frame over the full cardiac cycle reconstruction') +
  xlab('Frame') + 
  ylab('Mean Chamfer distance (mm)')

# Substructure results
{
temp_model <- list()
geom_model <- list()
F_values <- list()
p_values <- list()
for(t_model in unique(error_tables$temp_model)){
  for(g_model in unique(error_tables$geom_model)){
    temp_model <- append(temp_model, t_model)
    geom_model <- append(geom_model, g_model)
    # print(t_model)
    # print(g_model)
    temp_geom_table <- error_tables %>% 
      filter(temp_model == t_model, geom_model == g_model, 
             error_type == 'valid') 
    frame.aov <- aov(loss ~ substructure, data = temp_geom_table)
    summary.frame.aov <- summary(frame.aov)
    # print(summary.frame.aov[[1]]$`F value`[[1]])
    # print(summary.frame.aov[[1]]$`Pr(>F)`[[1]])
    F_values <- append(F_values, summary.frame.aov[[1]]$`F value`[[1]])
    p_values <- append(p_values, summary.frame.aov[[1]]$`Pr(>F)`[[1]])
  }
}
substructure_anova <- as.data.frame(cbind(temp_model, geom_model, F_values, p_values))
  }


substructure_anova  %>% 
  mutate(signif = ifelse(p_values > 0.1, "", ifelse(p_values > 0.05, ".", ifelse(p_values > 0.01, "*", ifelse(p_values > 0.001, "**", "***"))))) -> substructure_anova

substructure_anova %>%
  merge(substrucuture_results) %>% 
  # relocate(sd_loss_across_substructures, .after = geom_model)  %>% 
  mutate(across(c(3,6:7), ~ round(as.numeric(.x), digits = 2))) %>% 
  # mutate(p_values = signif(as.numeric(p_values), digits = 2)) %>% 
  select(-p_values )
  kable(format = 'latex', booktabs = T)

error_tables %>% 
  filter(error_type == 'valid') %>% 
  select(-error_type) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  group_by(temp_model, n_temp_feats, geom_model, n_geom_feats, frame) %>% 
  summarise(mean_loss = mean(loss), sd_loss = sd(loss)) %>% 
  mutate(loss = paste(round(mean_loss,2), '(X', round(sd_loss,2), ')')) %>% 
  select(-mean_loss, -sd_loss) %>% 
  pivot_wider(values_from = loss, names_from = frame)

error_tables %>% 
  mutate(model = paste(geom_model, 'x', temp_model, sep = ""),
         frame = as.factor(frame)) %>%  
  mutate(model = str_replace_all(model, '_', ' ')) %>% 
  filter(error_type == 'valid') %>% 
  group_by(model, sample, frame) %>% 
  summarise(loss = mean(loss)) %>% 
  ggplot(aes(x = frame, y = loss, fill = model)) +
  geom_boxplot() + 
  geom_hline(yintercept = 1.8, linetype = 'dashed') +
  facet_grid(model ~ .) +
  theme_bw() +
  ggtitle('Avg reconstruction loss for each frame over the full cardiac cycle reconstruction') +
  xlab('Frame') + 
  ylab('Mean Chamfer distance (mm)')
