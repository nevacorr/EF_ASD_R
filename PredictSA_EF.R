
rm(list = ls())

demopath <- "/Users/nevao/Documents/IBIS_EF/source data/"
brainpath <- "/Users/nevao/Documents/IBIS_EF/source data/Brain_Data/"

demo_behav <- 
  read.csv(paste0(demopath, "IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

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

cortex_parcel_df <- remove_bad_quality_data(cortex_parcel_df)
vol_df <- remove_bad_quality_data(vol_df)
subcort_df <- remove_bad_quality_data(subcort_df)

names(demo_behav)[names(demo_behav) == "Identifiers"] <- "CandID"
demo_behav <- subset(demo_behav, select = -X)

merged_df <- merge(demo_behav, cortex_parcel_df, by = "CandID", all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, vol_df, by = "CandID", all = TRUE, all.x = TRUE, all.y = FALSE)
merged_df <- merge(merged_df, subcort_df, by = 'CandID', all = TRUE, all.x = TRUE, all.y = FALSE)
