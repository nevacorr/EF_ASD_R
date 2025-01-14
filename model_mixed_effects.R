# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("lme4")
# install.packages("lmerTest")

library(ggplot2)
library(tidyr)
library(lme4)
library(dplyr)
library(lmerTest)

rm(list = ls())

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Convert empty strings in Group column to NA 
ibis_behav$Group[ibis_behav$Group == ""] <- NA  
ibis_behav$Flanker_Standard_Age_Corrected[ibis_behav$Flanker_Standard_Age_Corrected == ""] <- NA
ibis_behav$DCCS_Standard_Age_Corrected[ibis_behav$DCCS_Standard_Age_Corrected == ""] <- NA

# Remove rows with no Group
ibis_behav_filtered <- ibis_behav[!is.na(ibis_behav$Group), ]

# Remove LR+ group
ibis_behav_filtered <- ibis_behav_filtered %>% filter(Group != "LR+")

# Convert Group and Sex to factors
ibis_behav_filtered$Sex <- factor(ibis_behav_filtered$Sex)
ibis_behav_filtered$Group <- factor(ibis_behav_filtered$Group)
ibis_behav_filtered$Identifiers <- factor(ibis_behav_filtered$Identifiers)

# Set 'GroupLR-' as the reference level
ibis_behav_filtered$Group <- relevel(ibis_behav_filtered$Group, ref = "LR-")

source("fit_linear_mixed_effects_model.R")

result_flanker1 <- fit_linear_mixed_effects_model("Flanker_Standard_Age_Corrected", ibis_behav_filtered)
result_dccs1 <- fit_linear_mixed_effects_model("DCCS_Standard_Age_Corrected", ibis_behav_filtered)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker1, "Flanker_Standard_Age_Corrected")
plot_model_with_age_by_group(result_dccs1, "DCCS_Standard_Age_Corrected")

source("fit_linear_mixed_effects_model_predictschoolage.R")

result_flanker <- fit_linear_mixed_effects_model_predictschoolage("Flanker_Standard_Age_Corrected", ibis_behav_filtered)
result_dccs <- fit_linear_mixed_effects_model_predictschoolage("DCCS_Standard_Age_Corrected", ibis_behav_filtered)



