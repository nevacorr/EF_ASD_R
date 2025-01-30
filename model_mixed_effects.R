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

standardize <- 1 # Indicate whether to convert EF scores to Z scores

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

# Remove duplicate rows
ibis_behav <- ibis_behav[!duplicated(ibis_behav$Identifiers), ]

# Convert empty strings in Group column to NA 
ibis_behav$Group[ibis_behav$Group == ""] <- NA  

if (standardize == 1) {
  # Manually calculate z-scores for 12mo, 24mo and school age columns
  ibis_behav$AB_12_Percent <- (ibis_behav$AB_12_Percent -
                               mean(ibis_behav$AB_12_Percent, na.rm = TRUE)) / sd(ibis_behav$AB_12_Percent, na.rm = TRUE)
  ibis_behav$AB_24_Percent <- (ibis_behav$AB_24_Percent -
                               mean(ibis_behav$AB_24_Percent, na.rm = TRUE)) / sd(ibis_behav$AB_24_Percent, na.rm = TRUE)
  ibis_behav$Flanker_Standard_Age_Corrected <- (ibis_behav$Flanker_Standard_Age_Corrected -
                               mean(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)
  ibis_behav$DCCS_Standard_Age_Corrected <- (ibis_behav$DCCS_Standard_Age_Corrected -
                                                  mean(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)
  ibis_behav$BRIEF2_GEC_T_score <- (ibis_behav$BRIEF2_GEC_T_score -
                                                  mean(ibis_behav$BRIEF2_GEC_T_score, na.rm = TRUE)) / sd(ibis_behav$BRIEF2_GEC_T_score, na.rm = TRUE)
  }

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

# Count occurrences in the Group column
counts <- ibis_behav_filtered %>%
  count(Group)

print(counts)

source("fit_linear_mixed_effects_model.R")

result_flanker1 <- fit_linear_mixed_effects_model("Flanker_Standard_Age_Corrected", ibis_behav_filtered, standardize)
result_dccs1 <- fit_linear_mixed_effects_model("DCCS_Standard_Age_Corrected", ibis_behav_filtered, standardize)
result_brief2 <- fit_linear_mixed_effects_model("BRIEF2_GEC_T_score", ibis_behav_filtered, standardize)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker1, "Flanker_Standard_Age_Corrected", standardize)
plot_model_with_age_by_group(result_dccs1, "DCCS_Standard_Age_Corrected", standardize)
plot_model_with_age_by_group(result_brief2, "BRIEF2_GEC_T_score", standardize)

#source("fit_linear_mixed_effects_model_predictschoolage.R")

#result_flanker <- fit_linear_mixed_effects_model_predictschoolage("Flanker_Standard_Age_Corrected", ibis_behav_filtered, standardize)
#result_dccs <- fit_linear_mixed_effects_model_predictschoolage("DCCS_Standard_Age_Corrected", ibis_behav_filtered, standardize)





