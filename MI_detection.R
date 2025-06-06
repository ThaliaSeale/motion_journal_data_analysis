###
# MI detection
###

library(tidyverse)

# Detection from clinical metrics
clinical_metric_files <- read_csv("clinical_metric_files.csv")

# Detection from PCA
iMI_errors <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_MI/errors.csv')
iMI_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_MI/temporal_representations.csv')
## Prevalent MI 
pMI_errors <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_prevalent/errors.csv')
pMI_latents <- read.csv('/home/wolf6273/singular_spectrum_analysis/results/ssa_pca_50_components_on_pca_12_components_separated_features_prevalent/temporal_representations.csv')

# Loading PCA benchmark results
healthy_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/healthy_ED_latents.csv')
healthy_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/healthy_ES_latents.csv')
iMI_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/incident_MI_ED_latents.csv')
iMI_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/incident_MI_ES_latents.csv')
pMI_ED <- read.csv('/home/wolf6273/singular_spectrum_analysis/prevalent_MI_ED_latents.csv')
pMI_ES <- read.csv('/home/wolf6273/singular_spectrum_analysis/prevalent_MI_ES_latents.csv')