
rm(list = ls())

demopath <- "/Users/nevao/Documents/IBIS_EF/source data/"
brainpath <- "/Users/nevao/Documents/IBIS_EF/source data/Brain_Data/IBIS1&2_volumes_v3.13/"

ids <- 
  read.csv(paste0(demopath, "IBIS IDs_added_missing.csv"))

demo_behav <- 
  read.csv(paste0(demopath, "IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

cortex_parcel_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_LobeParcel_2020May5_withFrontDiv_V12V24only.csv"))

icv_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_ICV_2020May4_V12V24only.csv"))

tissue_df <- 
  read.csv(paste0(brainpath, "IBIS_v3.13_TotTiss_2020May4_V12V24only.csv"))

source("Prepare_data_for_XGBoost.R")

merged_df <- prepare_dataframe(ids ,demo_behav, cortex_parcel_df, icv_df, tissue_df)


