
library(dplyr)

rm(list = ls())

demopath <- "/Users/nevao/Documents/IBIS_EF/source data/"
brainpath <- "/Users/nevao/Documents/IBIS_EF/source data/Brain_Data/"

ids <- 
  read.csv(paste0(demopath, "IBIS IDs.csv"))

demo_behav <- 
  read.csv(paste0(demopath, "IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

#anotb <- 
#  read.csv(paste0(demopath, "AnotB_clean.csv"))

cortex_parcel_df <- 
  read.csv(paste0(brainpath, "IBISandDS_VSA_Cerebrum_and_LobeParcel_v01.02_20210809/",
                 "IBISandDS_VSA_Cerebrum_and_LobeParcel_Vols_v01.02_20210706.csv"))

vol_df <- 
  read.csv(paste0(brainpath, "IBISandDS_VSA_TissueSeg_v01.02_20210706/",
                  "IBISandDS_VSA_TissueSeg_Vols_v01.02_20210706.csv"))

subcort_df <- 
  read.csv(paste0(brainpath, "IBISandDS_VSA_Subcort_and_LV_v01.02_20210706/",
                  "IBISandDS_VSA_Subcort_and_LV_Vols_v01.02_20210706.csv"))

source("Utility_Functions.R")

ids_new <- ids %>%
  mutate(
    V06.demographcs.CandID = ifelse(V06.demographics.CandID == "." | V06.demographics.CandID == "", NA, V06.demographics.CandID),
    VSA.demographics.CandID = ifelse(VSA.demographics.CandID == "." | VSA.demographics.CandID == "", NA, VSA.demographics.CandID),
    V06.demographics.CandID = suppressWarnings(as.numeric(V06.demographics.CandID)),
    VSA.demographics.CandID = suppressWarnings(as.numeric(VSA.demographics.CandID)),
    CandID = coalesce(V06.demographics.CandID, VSA.demographics.CandID)
  ) %>%
  select(Identifiers, CandID)


#ids <- ids[c("Identifiers", "DCCID")]
#names(ids)[names(ids) == "DCCID"] <- "CandID"
#anotb$CandID<- as.integer(anotb$CandID)

demo_behav[demo_behav == ""] <- NA

cortex_parcel_df <- remove_bad_quality_data(cortex_parcel_df)
vol_df <- remove_bad_quality_data(vol_df)
subcort_df <- remove_bad_quality_data(subcort_df)


demo_behav <- subset(demo_behav, select = -X)

merged_df <- merge(anotb, demo_behav, by = "Identifiers", all.x = TRUE, all.y = TRUE)
merged_df <- merge(merged_df, cortex_parcel_df, by = "CandID", all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, vol_df, by = "CandID", all = TRUE, all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, subcort_df, by = 'CandID', all = TRUE, all.x = TRUE, all.y = FALSE)

merged_df <- merged_df %>%
  filter(rowSums(!is.na(select(., -Identifiers, -CandID, -Combined_ASD_DX, -Risk, -Group, -Sex))) > 0)

merged_df <- merged_df[, !(grepl("Age", names(merged_df)) & !grepl("Age_Corrected", names(merged_df)))]


