
install.packages(emmeans)
install.packages(forcats)
install.packages(ggplot2)

library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(forcats)

dummy_encode = 1

rm(list = ls())

# Load data
ibis_behav_orig <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

unique_duplicates <- names(table(ibis_behav_orig$Identifiers)[table(ibis_behav_orig$Identifiers) > 1])
# Duplicates are UNC0013, UNC0041, UNC0147, UNC0154

ibis_behav <- ibis_behav_orig[!duplicated(ibis_behav_orig$Identifiers), ]

# Function to clean data and calculate z-scores
clean_and_calculate_zscores <- function(df, column, group_name) {

  # Convert empty Group string values to NA 
  df$Group[df$Group == ""] <- NA
  
  # Remove rows with NA values
  df_clean <- na.omit(df)
  
  # Remove LR+ group
  df_clean <- df_clean %>% filter(Group != "LR+")

  # Choose the data for the LR- group
  lr_data <- df_clean[df_clean$Group == group_name, ]
  
  # Calculate mean and standard deviation for the LR- group
  mean_lr <- mean(lr_data[[column]], na.rm = TRUE)
  sd_lr <- sd(lr_data[[column]], na.rm = TRUE)
    
  # Calculate z-scores for the column and add it as a new column
  new_column_name <- paste0(column, "_z_score_norm")
  df_clean[[new_column_name]] <- (df_clean[[column]] - mean_lr) / sd_lr
  
  return(df_clean)
}

# Make subset dataframes for scores
flanker_df <- ibis_behav %>% select(Identifiers, Group, Flanker_Standard_Age_Corrected)
dccs_df <- ibis_behav %>% select(Identifiers, Group, DCCS_Standard_Age_Corrected)
ab12_df <- ibis_behav %>% select(Identifiers, Group, AB_12_Percent)
ab24_df <- ibis_behav %>% select(Identifiers, Group, AB_24_Percent)
brief2_df <- ibis_behav %>% select(Identifiers, Group, BRIEF2_GEC_T_score)

# Apply the cleaning and z-score calculation function to the dataframes
flanker_df_norm <- clean_and_calculate_zscores(flanker_df, 'Flanker_Standard_Age_Corrected', 'LR-')
dccs_df_norm <- clean_and_calculate_zscores(dccs_df, 'DCCS_Standard_Age_Corrected', 'LR-')
ab12_df_norm <- clean_and_calculate_zscores(ab12_df, 'AB_12_Percent', 'LR-')
ab24_df_norm <- clean_and_calculate_zscores(ab24_df, 'AB_24_Percent', 'LR-')
brief2_df_norm <- clean_and_calculate_zscores(brief2_df, 'BRIEF2_GEC_T_score', 'LR-')

# Select the Identifiers and *_z_score_norm columns from each dataframe
flanker_selected <- flanker_df_norm %>% select(Identifiers, ends_with("_z_score_norm"))
dccs_selected <- dccs_df_norm %>% select(Identifiers, ends_with("_z_score_norm"))
ab12_selected <- ab12_df_norm %>% select(Identifiers, ends_with("_z_score_norm"))
ab24_selected <- ab24_df_norm %>% select(Identifiers, ends_with("_z_score_norm"))
brief2_selected <- brief2_df_norm %>% select(Identifiers, ends_with("_z_score_norm"))

# Make a dataframe with just the demographic variables
ibis_demo = ibis_behav %>% select(Identifiers, Group, Sex)

# Merge behavior with the demographics dataframe based on the Identifiers column
z_normative_df <- ibis_demo %>%
  left_join(flanker_selected, by = "Identifiers") %>%
  left_join(dccs_selected, by = "Identifiers") %>%
  left_join(ab12_selected, by = "Identifiers") %>%
  left_join(ab24_selected, by = "Identifiers") %>%
  left_join(brief2_selected, by = "Identifiers")

z_normative_df <- z_normative_df %>% 
  rename(
    Flanker_Standard_Age_Corrected = Flanker_Standard_Age_Corrected_z_score_norm,
    DCCS_Standard_Age_Corrected = DCCS_Standard_Age_Corrected_z_score_norm,
    AB_12_Percent = AB_12_Percent_z_score_norm,
    AB_24_Percent = AB_24_Percent_z_score_norm,
    BRIEF2_GEC_T_score = BRIEF2_GEC_T_score_z_score_norm
  )

# For the Brief2 GEC score, higher values indicate more difficulty with EF
# Flip the sign of the Brief2 column
z_normative_df$BRIEF2_GEC_T_score <- -z_normative_df$BRIEF2_GEC_T_score

# Convert empty strings in Group column to NA 
z_normative_df$Group[z_normative_df$Group == ""] <- NA

# Dummy encode Sex
z_normative_df <-  z_normative_df %>% 
  mutate(Sex = factor(Sex, levels = c("Male", "Female")))
contrasts(z_normative_df$Sex) <- contr.treatment(2, base = 2)

# Print Sex contrast
print(contrasts(z_normative_df$Sex))

# Convert group to a factor
z_normative_df <- z_normative_df %>%
  mutate(Group = factor(Group, levels = c("HR+", "HR-", "LR-")))

if (dummy_encode == 1) {
  # Apply dummy encoding to the group variable
  contrasts(z_normative_df$Group) <- contr.treatment(3, base = 3)
} else {
# Apply effect encoding to the group variable
  contrasts(z_normative_df$Group) <- contr.sum(length(levels(z_normative_df$Group)))
}

# print coding
print(contrasts(z_normative_df$Group))

# Convert Identifiers to factor
z_normative_df$Identifiers <- factor(z_normative_df$Identifiers)

# Set 'GroupLR-' as the reference level
# z_normative_df$Group <- relevel(z_normative_df$Group, ref = "LR-")

source("fit_linear_mixed_effects_model.R")

print("Normative Z-score analysis")
result_flanker = fit_linear_mixed_effects_model('Flanker_Standard_Age_Corrected', z_normative_df, dummy_encode)
result_dccs = fit_linear_mixed_effects_model('DCCS_Standard_Age_Corrected', z_normative_df, dummy_encode)
result_brief2 = fit_linear_mixed_effects_model('BRIEF2_GEC_T_score', z_normative_df, dummy_encode)

source("plot_model_with_age_by_group.R")

plot_model_with_age_by_group(result_flanker, "LR- Normed Flanker_Standard_Age_Corrected")
plot_model_with_age_by_group(result_dccs, "LR- Normed DCCS_Standard_Age_Corrected")
plot_model_with_age_by_group(result_brief2, "LR- Normed Brief2 GEC score")

mystop=1
