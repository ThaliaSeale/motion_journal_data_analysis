library(tidyverse)
library(ggnewscale)

# Load cleaned dataset
dataset <- read.csv('combined_dataset.csv')

# Descriptive statistics and visualisation

## EF differences Men vs Women, healthy vs not
dodge <- position_dodge(width = 0.9)

dataset %>%
  filter(split %in% c('valid', 'test')) %>% 
  mutate(sex = ifelse(sex == 0, 'Female', 'Male')) %>%
  ggplot(aes(x = status, y = LVEF)) +  # status is now on x-axis
  
  # Background shading for Male LVEF zones
  geom_rect(data = data.frame(sex = c("Male", "Male", "Male", "Male"),
                              ymin = c(min(dataset$LVEF)- 0.05, 0.3, 0.4, 0.52),
                              ymax = c(30, 40, 52, 75)/100,
                              fill_cat = c("Severely Abnormal", "Moderately Abnormal", 
                                           "Mildly Abnormal", "Normal")),
            aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, fill = fill_cat),
            alpha = 0.15, inherit.aes = FALSE) +
  
  # Background shading for Female LVEF zones
  geom_rect(data = data.frame(sex = c("Female", "Female", "Female", "Female"),
                              ymin = c(min(dataset$LVEF) - 0.05, 0.30, 0.40, 0.54),
                              ymax = c(30, 40, 54, 75)/100,
                              fill_cat = c("Severely Abnormal", "Moderately Abnormal", 
                                           "Mildly Abnormal", "Normal")),
            aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, fill = fill_cat),
            alpha = 0.15, inherit.aes = FALSE) +
  
  # First fill scale for background
  scale_fill_manual(name = "LVEF Zone",
                    values = c("Normal" = "green",
                               "Mildly Abnormal" = "yellow",
                               "Moderately Abnormal" = "orange",
                               "Severely Abnormal" = "red")) +
  
  ggnewscale::new_scale_fill() +  # allow a second fill scale
  
  # Violin and boxplot, all white
  geom_violin(position = dodge, fill = "white", color = "black", alpha = 1) +
  geom_boxplot(width = 0.1, position = dodge, fill = "white", color = "black", outlier.shape = NA) +
  
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "right") +
  facet_wrap(~ sex, scales = "free_x") +
  ggtitle('Sex-specific Classification of Left Ventricular Ejection Fraction (LVEF)')

# 1. Filter for healthy and iMI
df_filtered <- subset(dataset, status %in% c("healthy", "iMI"))

# 2. Convert status to binary (1 = iMI, 0 = healthy)
df_filtered$status_bin <- ifelse(df_filtered$status == "iMI", 1, 0)

# 3. Select predictor columns starting with "X"
X_cols <- grep("^X", names(df_filtered), value = TRUE)

# 4. Split by sex
df_female <- subset(df_filtered, sex == 0)
df_male   <- subset(df_filtered, sex == 1)

# 5. Create formulas
full_formula <- as.formula(paste("status_bin ~", paste(X_cols, collapse = " + ")))
null_formula <- status_bin ~ 1

# 6. Fit null models
null_model_female <- glm(null_formula, data = df_female, family = binomial())
null_model_male   <- glm(null_formula, data = df_male, family = binomial())

# 7. Forward AIC selection
selected_model_female <- step(null_model_female,
                              scope = list(lower = null_formula, upper = full_formula),
                              direction = "forward", trace = TRUE)

selected_model_male <- step(null_model_male,
                            scope = list(lower = null_formula, upper = full_formula),
                            direction = "forward", trace = TRUE)

# 8. Summarise results
summary(selected_model_female)
summary(selected_model_male)

