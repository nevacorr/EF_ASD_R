# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("lme4")

library(ggplot2)
library(tidyr)
library(lme4)
library(dplyr)

rm(list = ls())

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Clean the Group column: convert empty strings to NA
# ibis_behav$Group <- trimws(ibis_behav$Group)  # Remove spaces
ibis_behav$Group[ibis_behav$Group == ""] <- NA  # Convert empty strings to NA

# Remove rows with no Group
ibis_behav_filtered <- ibis_behav[!is.na(ibis_behav$Group), ]

# Convert 'Group' to a factor
ibis_behav_filtered$Group <- factor(ibis_behav_filtered$Group)

# Set 'GroupLR-' as the reference level
ibis_behav_filtered$Group <- relevel(ibis_behav_filtered$Group, ref = "LR-")

source("fit_linear_mixed_effects_model.R")

result_flanker <- fit_linear_mixed_effects_model("Flanker_Standard_Age_Corrected", ibis_behav_filtered)
result_dccs <- fit_linear_mixed_effects_model("DCCS_Standard_Age_Corrected", ibis_behav_filtered)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker, "Flanker")
