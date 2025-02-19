
library(dplyr)

rm(list = ls())

demopath <- "/Users/nevao/Documents/IBIS_EF/source data/"
brainpath <- "/Users/nevao/Documents/IBIS_EF/source data/Brain_Data/IBIS1&2_volumes_v3.13/"

ids <- 
  read.csv(paste0(demopath, "IBIS IDs.csv"))

demo_behav <- 
  read.csv(paste0(demopath, "IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

cortex_parcel_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_LobeParcel_2020May5_withFrontDiv_V12V24only.csv"))

icv_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_ICV_2020May4_V12V24only.csv"))

tissue_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_TotTiss_2020May4_V12V24only.csv"))

source("Utility_Functions.R")

# Change ids file so that it has only Identifiers and CandID columns that represents values for all ages
ids <- ids %>%
  mutate(
    V06.demographcs.CandID = ifelse(V06.demographics.CandID == "." | V06.demographics.CandID == "", NA, V06.demographics.CandID),
    VSA.demographics.CandID = ifelse(VSA.demographics.CandID == "." | VSA.demographics.CandID == "", NA, VSA.demographics.CandID),
    V06.demographics.CandID = suppressWarnings(as.integer(V06.demographics.CandID)),
    VSA.demographics.CandID = suppressWarnings(as.integer(VSA.demographics.CandID)),
    CandID = coalesce(V06.demographics.CandID, VSA.demographics.CandID)
  ) %>%
  select(Identifiers, CandID)

demo_behav[demo_behav == ""] <- NA

source("Utility_Functions.R")

cortex_parcel_df <- remove_bad_quality_data_12_24(cortex_parcel_df, c('Par', 'Front', 'Cing'))
icv_df <- remove_bad_quality_data_12_24(icv_df, c('ICV'))
tissue_df <- remove_bad_quality_data_12_24(tissue_df, c('totTiss'))

demo_behav <- subset(demo_behav, select = -X)

merged_df <- merge(ids, demo_behav, by = "Identifiers", all.x = TRUE, all.y = TRUE)
merged_df <- merge(merged_df, cortex_parcel_df, by = "CandID", all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, icv_df, by = "CandID", all = TRUE, all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, tissue_df, by = 'CandID', all = TRUE, all.x = TRUE, all.y = FALSE)

merged_df <- merged_df %>%
  filter(rowSums(!is.na(select(., -Identifiers, -CandID, -Combined_ASD_DX, -Risk, -Group, -Sex))) > 0)

merged_df <- merged_df[, !(grepl("Age", names(merged_df)) & !grepl("Age_Corrected", names(merged_df)))]

merged_df <- merged_df[, !grepl("Reversal", colnames(merged_df))]

for (col in grep("GM|WM.*_V12$|GM|WM.*_V24$", colnames(merged_df), value = TRUE)) {
  merged_df[[col]] <- as.numeric(merged_df[[col]])
  
  if (grepl("_V12$", col)) {
    merged_df[[col]] <- merged_df[[col]] / merged_df$totTiss_V12
  } else if (grepl("_V24$", col)) {
    merged_df[[col]] <- merged_df[[col]] / merged_df$totTiss_V24
  }
}