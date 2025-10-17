library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(forcats)
library(lmerTest)
library(readr)
library(writexl)
library(ggplot2)

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

school_age_outcome_vars <- c(
  Flanker_Standard_Age_Corrected = 'Flanker_Standard_Age_Corrected',
  DCCS_Standard_Age_Corrected = 'DCCS_Standard_Age_Corrected',
  BRIEF2_GEC_T_score = 'BRIEF2_GEC_T_score',
  BRIEF2_shift_T_score = 'BRIEF2_shift_T_score',
  BRIEF2_inhibit_T_score = 'BRIEF2_inhibit_T_score',
  BRIEF2_working_memory_T_score = 'BRIEF2_working_memory_T_score'
)

# Load data
ibis_behav_orig <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/Behav_Data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF1+2_Brief2subscales_16Oct2025.csv"))

# Convert maternal education to binary variable 
ibis_behav_orig$maternal_education <-
  ifelse(ibis_behav_orig$V06.tsi.mother_education %in% c("college_degree", "some_grad_level",
                                                       "grad_degree"), 1, 0)

# If BRIEF2 columns have prefix "VSD.All" and Parent substrings, remove these substrings
names(ibis_behav_orig) <- gsub("^VSD\\.All\\.BRIEF2_", "BRIEF2_", names(ibis_behav_orig))
names(ibis_behav_orig) <- gsub("^BRIEF2_Parent.", "BRIEF2_", names(ibis_behav_orig))

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
ibis_behav <- merge(ibis_behav_orig, iq_df_subset, by = 'Identifiers', all.x=TRUE)

# Write dataframe to file
write.csv(ibis_behav, file = file.path(subdir, 'ibis_subj_demographics_and_data_used_for_2025analysis_with_Brief2_subscales_with_brief1.csv'), row.names = FALSE)

# Rename groups
ibis_behav <- ibis_behav %>%
  mutate(Group = recode(Group,
                        "HR+" = "HL-ASD",
                        "HR-" = "HL-noASD",
                        "LR-" = "LL"))

# ---- Loop over covariate options ----
for(use_covariates in c(TRUE, FALSE)) {
  
  cov_label <- ifelse(use_covariates, "_withIQ_ME", "_noIQ_ME")
    
  for(score_col in names(school_age_outcome_vars)) {
        
    # Make subset dataframe for score
    df <- ibis_behav %>% select(
      Identifiers, Group, all_of(score_col), 
      V24.mullen.composite_standard_score, V06.tsi.mother_education, 
      maternal_education
    )
    
    ab12_df <- ibis_behav %>% select(Identifiers, Group, AB_12_Percent, 
              V24.mullen.composite_standard_score, V06.tsi.mother_education, maternal_education)
    ab24_df <- ibis_behav %>% select(Identifiers, Group, AB_24_Percent, 
              V24.mullen.composite_standard_score, V06.tsi.mother_education, maternal_education)
    
    
    # Apply the cleaning and z-score calculation function to the dataframe
    df_norm <- clean_and_calculate_zscores(df, score_col, use_covariates)
    ab12_df_norm <- clean_and_calculate_zscores(ab12_df, 'AB_12_Percent', use_covariates)
    ab24_df_norm <- clean_and_calculate_zscores(ab24_df, 'AB_24_Percent', use_covariates)
    
    # Write cleaned dataframes to file
    write.csv(df_norm, file=file.path(subdir, paste0(score_col, '_used_for_2025analysis_', cov_label, '.csv')), row.names = FALSE)
    write.csv(ab12_df_norm, file=file.path(subdir, paste0('ab12_used_for_2025analysis_', cov_label, '.csv')), row.names = FALSE)
    write.csv(ab24_df_norm, file=file.path(subdir, paste0('ab24_used_for_2025analysis_', cov_label,'.csv')), row.names = FALSE)
    
    # Select the Identifiers column for the score
    df_selected <- df_norm %>% select(Identifiers, all_of(score_col))
    ab12_selected <- ab12_df_norm %>% select(Identifiers, 'AB_12_Percent')
    ab24_selected <- ab24_df_norm %>% select(Identifiers, 'AB_24_Percent')
    
    # Make a dataframe with just the demographic variables 
    ibis_demo <-  ibis_behav %>% select(Identifiers, Group, Sex, 
                        V24.mullen.composite_standard_score, maternal_education)
    
    # Merge behavior with the demographics dataframe based on the Identifiers column
    z_normative_df <- ibis_demo %>%
      left_join(df_selected, by = "Identifiers")%>%
      left_join(ab12_selected, by = "Identifiers") %>%
      left_join(ab24_selected, by = "Identifiers")
    
    if (score_col %in% c("BRIEF2_GEC_T_score",
                         "BRIEF2_shift_T_score",
                         "BRIEF2_working_memory_T_score",
                         "BRIEF2_inhibit_T_score")) {
      # Flip the sign of the corresponding column
      z_normative_df[[score_col]] <- -z_normative_df[[score_col]]
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
                    data = z_normative_df, 
                    use_covariates = use_covariates)
    
    # Plot model
    plot_model_with_age_by_group_Final(model_results, score_col, use_covariates)
    
 }
}

