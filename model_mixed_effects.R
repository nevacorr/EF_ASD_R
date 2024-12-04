# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("lme4")

library(ggplot2)
library(tidyr)
library(lme4)
library(dplyr)

rm(list = ls())

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Convert empty strings in Group column to NA to NA
ibis_behav$Group[ibis_behav$Group == ""] <- NA  # Convert empty strings to NA

# Remove rows with no Group
ibis_behav_filtered_unscaled <- ibis_behav[!is.na(ibis_behav$Group), ]

source("scale_columns_to_zscore.R")

# Scale score columns
ibis_behav_filtered <- scale_columns_to_zscore(ibis_behav_filtered_unscaled, 
          c("AB_12_Percent", "AB_24_Percent", "Flanker_Standard_Age_Corrected", "DCCS_Standard_Age_Corrected"))

# Convert 'Group' to a factor
ibis_behav_filtered$Group <- factor(ibis_behav_filtered$Group)

# Set 'GroupLR-' as the reference level
ibis_behav_filtered$Group <- relevel(ibis_behav_filtered$Group, ref = "LR-")

source("fit_linear_mixed_effects_model.R")

result_flanker <- fit_linear_mixed_effects_model("Flanker_Standard_Age_Corrected", ibis_behav_filtered)
result_dccs <- fit_linear_mixed_effects_model("DCCS_Standard_Age_Corrected", ibis_behav_filtered)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker, "Flanker")
plot_model_with_age_by_group(result_dccs, "DCCS")
