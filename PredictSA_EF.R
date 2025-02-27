library(dplyr)
library(xgboost)
library(Matrix)


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

final_df <- prepare_dataframe(ids ,demo_behav, cortex_parcel_df, icv_df, tissue_df)

# Define response variable
response <- "Flanker_Standard_Age_Corrected"

# Remove rows where the response variable is NA
final_df <- final_df %>% drop_na(all_of(response))

# List of columns to exclude from predictors
exclude_cols <- c("CandID", "Identifiers", "Combined_ASD_DX", "Risk", "Group", "AB_12_Percent", "AB_24_Percent", "BRIEF2_GEC_T_score", 
                  "BRIEF2_GEC_raw_score", "DCCS_Standard_Age_Corrected", "ICV_V12", "ICV_V24", "totTiss_V12", "totTiss_V24")  

# Select predictor columns (all except response and exclude_cols)
predictors <- final_df %>%
  select(-all_of(c(response, exclude_cols)))

# Convert response variable to numeric vector
y <- final_df[[response]]

predictors <- predictors %>%
  mutate(Sex = ifelse(Sex == "Male", 1, 0))  # Female = 0, Male = 1

# Convert predictors to a numeric matrix 
X <- as.matrix(predictors)

# Convert to XGBoost DMatrix
dtrain <- xgb.DMatrix(data = X, label = y, missing = NA)

# Define basic parameters for regression
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # For continuous target values
  eval_metric = "rmse",            # Root Mean Squared Error
  eta = 0.1,                       # Learning rate
  max_depth = 6,                    # Maximum depth of trees
  subsample = 0.8,                  # Row sampling to reduce overfitting
  colsample_bytree = 0.8            # Feature sampling
)

# Perform 10 fold cross validation. This will output the RMSE for each fold and stop early if performance
# does not improve. 
set.seed(123)  # Ensure reproducibility
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,          # Max number of boosting rounds
  nfold = 10,             # 10-fold cross-validation
  early_stopping_rounds = 10,  # Stop if no improvement after 10 rounds
  verbose = TRUE
)

# Train the final model
final_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = cv_results$best_iteration,
  verbose = TRUE
)

