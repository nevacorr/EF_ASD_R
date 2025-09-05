
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
ibis_behav_orig <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/Behav_Data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

unique_duplicates <- names(table(ibis_behav_orig$Identifiers)[table(ibis_behav_orig$Identifiers) > 1])
# Duplicates are UNC0013, UNC0041, UNC0147, UNC0154

# Remove rows with duplicate identifiers
ibis_behav_rd <- ibis_behav_orig[!duplicated(ibis_behav_orig$Identifiers), ]

# Load IQ data
iq_df <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/Behav_Data/IQ data_Long_data-2025-07-26T23_51_14.152Z.csv"))
iq_df_subset <- iq_df[,c("Identifiers", "V24.mullen.composite_standard_score")]
# Replace "." with NA
iq_df_subset$V24.mullen.composite_standard_score[iq_df_subset$V24.mullen.composite_standard_score == "."] <- NA
# Convert IQ to numeric type
iq_df_subset$V24.mullen.composite_standard_score <- as.numeric(iq_df_subset$V24.mullen.composite_standard_score)
# Convert IQ to z score
iq_df_subset[["V24.mullen.composite_standard_score"]]<- 
  (iq_df_subset[["V24.mullen.composite_standard_score"]] 
  - mean(iq_df_subset[["V24.mullen.composite_standard_score"]], 
  na.rm = TRUE)) / sd(iq_df_subset[["V24.mullen.composite_standard_score"]], na.rm = TRUE)

# Merge IQ data with rest of dataframe
ibis_behav <- merge(ibis_behav_rd, iq_df_subset, by = 'Identifiers', all.x=TRUE)

# Write dataframe to file
write.csv(ibis_behav, file = file.path(subdir, 'ibis_subj_demographics_and_data_used_for_2025analysis_plusIQ.csv'), row.names = FALSE)

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
  
  # Remove all rows containing NA values except for IQ column
  df <- df %>%
    filter(if_all(-V24.mullen.composite_standard_score, ~ !is.na(.)))
  
  # # Remove rows where V24 mullens is NA
  num_removed <- df %>% filter(is.na(V24.mullen.composite_standard_score)) %>% nrow()
  df_clean <- df %>% filter(!is.na(V24.mullen.composite_standard_score))
  cat(column, "removed", num_removed, "rows missing IQ\n")
  
  # Remove other rows with NA values
  # df_clean <- na.omit(df)
  
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
flanker_df <- ibis_behav %>% select(Identifiers, Group, Flanker_Standard_Age_Corrected, V24.mullen.composite_standard_score)
dccs_df <- ibis_behav %>% select(Identifiers, Group, DCCS_Standard_Age_Corrected, V24.mullen.composite_standard_score)
ab12_df <- ibis_behav %>% select(Identifiers, Group, AB_12_Percent, V24.mullen.composite_standard_score)
ab24_df <- ibis_behav %>% select(Identifiers, Group, AB_24_Percent, V24.mullen.composite_standard_score)
brief2_df <- ibis_behav %>% select(Identifiers, Group, BRIEF2_GEC_T_score, V24.mullen.composite_standard_score)

# Apply the cleaning and z-score calculation function to the dataframes
flanker_df_norm <- clean_and_calculate_zscores(flanker_df, 'Flanker_Standard_Age_Corrected')
dccs_df_norm <- clean_and_calculate_zscores(dccs_df, 'DCCS_Standard_Age_Corrected')
ab12_df_norm <- clean_and_calculate_zscores(ab12_df, 'AB_12_Percent')
ab24_df_norm <- clean_and_calculate_zscores(ab24_df, 'AB_24_Percent')
brief2_df_norm <- clean_and_calculate_zscores(brief2_df, 'BRIEF2_GEC_T_score')

# Write cleaned dataframes to file
write.csv(flanker_df_norm, file=file.path(subdir, 'flanker_used_for_2025analysis_withIQ.csv'), row.names = FALSE)
write.csv(dccs_df_norm, file=file.path(subdir, 'dccs_used_for_2025analysis_withIQ.csv'), row.names = FALSE)
write.csv(ab12_df_norm, file=file.path(subdir, 'ab12_used_for_2025analysis_withIQ.csv'), row.names = FALSE)
write.csv(ab24_df_norm, file=file.path(subdir, 'ab24_used_for_2025analysis_withIQ.csv'), row.names = FALSE)
write.csv(brief2_df_norm, file=file.path(subdir, 'brief2_used_for_2025analysis_withIQ.csv'), row.names = FALSE)

# Select the Identifiers and school age score column from each dataframe
flanker_selected <- flanker_df_norm %>% select(Identifiers, ends_with('Flanker_Standard_Age_Corrected'))
dccs_selected <- dccs_df_norm %>% select(Identifiers, ends_with('DCCS_Standard_Age_Corrected'))
ab12_selected <- ab12_df_norm %>% select(Identifiers, ends_with('AB_12_Percent'))
ab24_selected <- ab24_df_norm %>% select(Identifiers, ends_with('AB_24_Percent'))
brief2_selected <- brief2_df_norm %>% select(Identifiers, ends_with('BRIEF2_GEC_T_score'))

# Make a dataframe with just the demographic variables 
ibis_demo = ibis_behav %>% select(Identifiers, Group, Sex, V24.mullen.composite_standard_score)

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

# Rename V24 mullens column to IQ
z_normative_df <- z_normative_df %>% rename(IQ = V24.mullen.composite_standard_score)

source("fit_linear_mixed_effects_model_IQ.R")

print("Normative Z-score analysis")
print("Results for Flanker")
result_flanker = fit_linear_mixed_effects_model_IQ('Flanker_Standard_Age_Corrected', z_normative_df)
print("Results for DCCS")
result_dccs = fit_linear_mixed_effects_model_IQ('DCCS_Standard_Age_Corrected', z_normative_df)
print("Results for Brief2")
result_brief2 = fit_linear_mixed_effects_model_IQ('BRIEF2_GEC_T_score', z_normative_df)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker, "Flanker")
plot_model_with_age_by_group(result_dccs, "Dimensional Change Card Sort")
plot_model_with_age_by_group(result_brief2, "Brief2")

mystop=1
