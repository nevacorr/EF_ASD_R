library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(forcats)
library(lmerTest)
library(readr)
library(writexl)
library(ggplot2)
library(forcats)

rm(list = ls())

source("fit_linear_mixed_effects_model_Final.R")
source("print_and_save_summary_Final.R")
source("plot_model_with_age_by_group_Final.R")
source("clean_and_calculate_zscores.R")

# Define output file path
subdir <- "processed_datafiles"

# Create subdirectory if it doesn't exist
if (!dir.exists(subdir)) {
  dir.create(subdir)
}

outcome_vars <- c(
  Flanker_Standard_Age_Corrected = 'Flanker_Standard_Age_Corrected',
  DCCS_Standard_Age_Corrected = 'DCCS_Standard_Age_Corrected',
  AB_12_Percent = 'AB_12_Percent',
  AB_24_Percent = 'AB_24_Percent',
  BRIEF2_GEC_T_score = 'BRIEF2_GEC_T_score'
)

# Load data
ibis_behav_orig <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/Behav_Data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2_addedmissing_age_data_mat_ed_15Sep2025a.csv"))

unique_duplicates <- names(table(ibis_behav_orig$Identifiers)[table(ibis_behav_orig$Identifiers) > 1])
# Duplicates are UNC0013, UNC0041, UNC0147, UNC0154

# Remove rows with duplicate identifiers
ibis_behav_rd <- ibis_behav_orig[!duplicated(ibis_behav_orig$Identifiers), ]

# Show values for maternal education and value counts
print(table(ibis_behav_rd$V06.tsi.mother_education))

# Convert maternal education to binary variable (note remove college_degree 
# if want a little more balanced)
# Code missing values as 0
ibis_behav_rd$maternal_education <-
  ifelse(ibis_behav_rd$V06.tsi.mother_education %in% c("college_degree", "some_grad_level",
                                                       "grad_degree"), 1, 0)

print(table(ibis_behav_rd$maternal_education, useNA = "ifany"))

# If BRIEF2 columns have prefix "VSD.All" and Parent substrings, remove these substrings
names(ibis_behav_rd) <- gsub("^VSD\\.All\\.BRIEF2_", "BRIEF2_", names(ibis_behav_rd))
names(ibis_behav_rd) <- gsub("^BRIEF2_Parent.", "BRIEF2_", names(ibis_behav_rd))

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
write.csv(ibis_behav, file = file.path(subdir, 'ibis_subj_demographics_and_data_used_for_2025analysis_plusIQandME.csv'), row.names = FALSE)

# # Rename groups
# ibis_behav <- ibis_behav %>%
#   mutate(Group = recode(Group, 
#                         "HR+" = "HL-ASD", 
#                         "HR-" = "HL-noASD", 
#                         "LR-" = "LL"))

# ---- Loop over covariate options ----
for(use_covariates in c(TRUE, FALSE)) {
  
  cov_label <- ifelse(use_covariates, "_withIQ_ME", "_noIQ_ME")
  
  for(score_col in names(outcome_vars)) {
    
    # Make subset dataframe for score
    df <- ibis_behav %>% select(
      Identifiers, Group, all_of(score_col), 
      V24.mullen.composite_standard_score, V06.tsi.mother_education, 
      maternal_education
    )

    # Apply the cleaning and z-score calculation function to the dataframe
    df_norm <- clean_and_calculate_zscores(df, score_col, use_covariates)

    # Write cleaned dataframes to file
    out_file <-  file.path(subdir, paste0(score_col, cov_label, '.csv'))
    write.csv(df_norm, file=out_file, row.names = FALSE)

    # Select the Identifiers column for the score
    df_selected <- df_norm %>% select(Identifiers, ends_with(score_col))

    # Make a dataframe with just the demographic variables 
    ibis_demo <-  ibis_behav %>% select(Identifiers, Group, Sex, 
                        V24.mullen.composite_standard_score, maternal_education)

    # Merge behavior with the demographics dataframe based on the Identifiers column
    z_normative_df <- ibis_demo %>%
      left_join(df_selected, by = "Identifiers")

    if (score_col == "BRIEF2_GEC_T_score") {
      # Flip the sign of the Brief2 column
      z_normative_df$BRIEF2_GEC_T_score <- -z_normative_df$BRIEF2_GEC_T_score
    }

    # Convert empty strings in Group column to NA 
    z_normative_df$Group[z_normative_df$Group == ""] <- NA

    # Convert Group and Identifiers to factor
    z_normative_df$Group <- factor(z_normative_df$Group, levels = 
                                     c("LL", "HL-noASD", "HL-ASD"))
    z_normative_df$Identifiers <- factor(z_normative_df$Identifiers)

    # Make LL group the reference group
    z_normative_df$Group <- relevel(z_normative_df$Group, ref = "LL")

    # Rename V24 mullens column to IQ
    z_normative_df <- z_normative_df %>% rename(IQ = V24.mullen.composite_standard_score)

    # Rename maternal_education to ME
    z_normative_df <- z_normative_df %>% rename(ME = maternal_education)

    # Fit model
    model_results <-  fit_linear_mixed_effects_model_Final(
                    score_column = score_col, 
                    df = z_normative_df, 
                    use_covariates = use_covariates)

    # Plot model
    plot_model_with_age_by_group_Final(model_results, score_col, 
                                 paste0("IQ and ME covariates: ", use_covariates))
  }
}

