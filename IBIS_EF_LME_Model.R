
# install.packages(emmeans)
# install.packages(forcats)
# install.packages(ggplot2)

library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(forcats)
library(lmerTest)

rm(list = ls())

# Define output file path
subdir <- "processed_datafiles"

# Create subdirectory if it doesn't exist
if (!dir.exists(subdir)) {
  dir.create(subdir)
}

# Load data
ibis_behav_orig <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

unique_duplicates <- names(table(ibis_behav_orig$Identifiers)[table(ibis_behav_orig$Identifiers) > 1])
# Duplicates are UNC0013, UNC0041, UNC0147, UNC0154

ibis_behav <- ibis_behav_orig[!duplicated(ibis_behav_orig$Identifiers), ]

# Write data without duplicates to file
write.csv(ibis_behav, file = file.path(subdir, 'ibis_subj_demographics_and_data_used_for_2025analysis.csv'), row.names = FALSE)

# Rename groups
ibis_behav <- ibis_behav %>%
  mutate(Group = recode(Group, 
                        "HR+" = "HL-ASD", 
                        "HR-" = "HL-noASD", 
                        "LR-" = "LL"))

# Function to clean data and calculate z-scores
clean_and_calculate_zscores <- function(df, column) {

  # Convert empty Group string values to NA 
  df$Group[df$Group == ""] <- NA
  
  # Remove rows with NA values
  df_clean <- na.omit(df)
  
  # Remove LR+ group
  df_clean <- df_clean %>% filter(Group != "LR+")
  
  # Rename groups
  df_clean <- df_clean %>%
    mutate(Group = recode(Group, 
                                "HR+" = "HL-ASD", 
                                "HR-" = "HL-noASD", 
                                "LR-" = "LL"))

  # Replace score with z score  
  df_clean[[column]]<- (df_clean[[column]] - mean(df_clean[[column]], na.rm = TRUE)) / sd(df_clean[[column]], na.rm = TRUE)
  
  return(df_clean)
}

# Make subset dataframes for scores
flanker_df <- ibis_behav %>% select(Identifiers, Group, Flanker_Standard_Age_Corrected)
dccs_df <- ibis_behav %>% select(Identifiers, Group, DCCS_Standard_Age_Corrected)
ab12_df <- ibis_behav %>% select(Identifiers, Group, AB_12_Percent)
ab24_df <- ibis_behav %>% select(Identifiers, Group, AB_24_Percent)
brief2_df <- ibis_behav %>% select(Identifiers, Group, BRIEF2_GEC_T_score)

# Apply the cleaning and z-score calculation function to the dataframes
flanker_df_norm <- clean_and_calculate_zscores(flanker_df, 'Flanker_Standard_Age_Corrected')
dccs_df_norm <- clean_and_calculate_zscores(dccs_df, 'DCCS_Standard_Age_Corrected')
ab12_df_norm <- clean_and_calculate_zscores(ab12_df, 'AB_12_Percent')
ab24_df_norm <- clean_and_calculate_zscores(ab24_df, 'AB_24_Percent')
brief2_df_norm <- clean_and_calculate_zscores(brief2_df, 'BRIEF2_GEC_T_score')

# Write cleaned dataframes to file
write.csv(flanker_df_norm, file=file.path(subdir, 'flanker_used_for_2025analysis.csv'), row.names = FALSE)
write.csv(dccs_df_norm, file=file.path(subdir, 'dccs_used_for_2025analysis.csv'), row.names = FALSE)
write.csv(ab12_df_norm, file=file.path(subdir, 'ab12_used_for_2025analysis.csv'), row.names = FALSE)
write.csv(ab24_df_norm, file=file.path(subdir, 'ab24_used_for_2025analysis.csv'), row.names = FALSE)
write.csv(brief2_df_norm, file=file.path(subdir, 'brief2_used_for_2025analysis.csv'), row.names = FALSE)

# Select the Identifiers and school age score column from each dataframe
flanker_selected <- flanker_df_norm %>% select(Identifiers, ends_with('Flanker_Standard_Age_Corrected'))
dccs_selected <- dccs_df_norm %>% select(Identifiers, ends_with('DCCS_Standard_Age_Corrected'))
ab12_selected <- ab12_df_norm %>% select(Identifiers, ends_with('AB_12_Percent'))
ab24_selected <- ab24_df_norm %>% select(Identifiers, ends_with('AB_24_Percent'))
brief2_selected <- brief2_df_norm %>% select(Identifiers, ends_with('BRIEF2_GEC_T_score'))

# Make a dataframe with just the demographic variables
ibis_demo = ibis_behav %>% select(Identifiers, Group, Sex)

# Merge behavior with the demographics dataframe based on the Identifiers column
z_normative_df <- ibis_demo %>%
  left_join(flanker_selected, by = "Identifiers") %>%
  left_join(dccs_selected, by = "Identifiers") %>%
  left_join(ab12_selected, by = "Identifiers") %>%
  left_join(ab24_selected, by = "Identifiers") %>%
  left_join(brief2_selected, by = "Identifiers")

# For the Brief2 GEC score, higher values indicate more difficulty with EF
# Flip the sign of the Brief2 column
z_normative_df$BRIEF2_GEC_T_score <- -z_normative_df$BRIEF2_GEC_T_score

# Convert empty strings in Group column to NA 
z_normative_df$Group[z_normative_df$Group == ""] <- NA

# Convert Group and Identifiers to factor
z_normative_df$Group <- factor(z_normative_df$Group, levels = c("LL", "HL-noASD", "HL-ASD"))
z_normative_df$Identifiers <- factor(z_normative_df$Identifiers)

# Make LL group the reference group
z_normative_df$Group <- relevel(z_normative_df$Group, ref = "LL")

source("fit_linear_mixed_effects_model.R")

print("Normative Z-score analysis")
print("Results for Flanker")
result_flanker = fit_linear_mixed_effects_model('Flanker_Standard_Age_Corrected', z_normative_df)
print("Results for DCCS")
result_dccs = fit_linear_mixed_effects_model('DCCS_Standard_Age_Corrected', z_normative_df)
print("Results for Brief2")
result_brief2 = fit_linear_mixed_effects_model('BRIEF2_GEC_T_score', z_normative_df)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker, "Flanker")
plot_model_with_age_by_group(result_dccs, "Dimensional Change Card Sort")
plot_model_with_age_by_group(result_brief2, "Brief2")

mystop=1
