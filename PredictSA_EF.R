# install.packages("ParBayesianOptimization")
# install.packages("xgboost")

library(dplyr)
library(xgboost)
library(Matrix)
library(ParBayesianOptimization)
library(xgboost)
library(Metrics)  
library(ggplot2)

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

# Removed Group from excluded values
# exclude_cols <- c("CandID", "Identifiers", "Combined_ASD_DX", "Risk", "AB_12_Percent", "AB_24_Percent", "BRIEF2_GEC_T_score", 
                  # "BRIEF2_GEC_raw_score", "DCCS_Standard_Age_Corrected", "ICV_V12", "ICV_V24", "totTiss_V12", "totTiss_V24")  


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
# dtrain <- xgb.DMatrix(data = X, label = y, missing = NA)

# Split into train (80%) and test (20%) sets
set.seed(123)
train_idx <- sample(seq_len(nrow(final_df)), size = 0.8 * nrow(final_df))
train_data <- final_df[train_idx, ]
test_data <- final_df[-train_idx, ]

# Prepare train and test sets
X_train <- train_data %>% select(-all_of(c(response, exclude_cols))) %>%
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(where(is.factor), as.numeric)) %>% 
  as.matrix()
y_train <- train_data[[response]]

X_test <- test_data %>% select(-all_of(c(response, exclude_cols))) %>%
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(where(is.factor), as.numeric)) %>% 
  as.matrix()
y_test <- test_data[[response]]

dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

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

# Check best RSME
print(cv_results$evaluation_log)

# Get final RSME from cross validation
best_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
print(paste("Best CV RMSE:", best_rmse))

# Define Bayesian Optimization Function
bayes_optimize_xgb <- function(eta, max_depth, min_child_weight, subsample, colsample_bytree) {
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = eta,  
    max_depth = round(max_depth),  
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    eval_metric = "rmse"
  )
  
  # Perform 10-fold cross-validation
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 100,
    nfold = 10,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # Return the negative RMSE (because Bayesian Optimization maximizes)
  list(Score = -min(cv$evaluation_log$test_rmse_mean), Pred = 0)
}

# Run Bayesian Optimization
set.seed(123)  # Ensure reproducibility

opt_res <- bayesOpt(
  FUN = bayes_optimize_xgb,
  bounds = list(
    eta = c(0.01, 0.3),
    max_depth = c(3, 10),
    min_child_weight = c(1, 10),
    subsample = c(0.5, 1),
    colsample_bytree = c(0.5, 1)
  ),
  initPoints = 10,  # Initial random samples
  iters.n = 20,  # Optimization iterations
  acq = "ucb",  # Acquisition function
  kappa = 2.576,  # Exploration-exploitation balance
  verbose = 1
)

# Train final model with best parameters. (eta 0.118 max_depth 10 min_child_weight 10 subsample 0.5 colsample_bytree 1)
best_params <- getBestPars(opt_res)

final_model <- xgb.train(
  params = c(best_params, list(objective = "reg:squarederror", eval_metric = "rmse")),
  data = dtrain,
  nrounds = 100
)

# Make predictions
predictions <- predict(final_model, dtrain)


# # Get predictions
# y_pred <- predict(final_model, dtrain)
# 
# # Compute RMSE
# rmse_value <- rmse(y, y_pred)
# print(paste("Final Model RMSE:", rmse_value))
# 
# # Compute R² (coefficient of determination)
# r2_value <- cor(y, y_pred)^2
# print(paste("Final Model R²:", r2_value))

# Make predictions on the test set
y_test_pred <- predict(final_model, dtest)

# Compute RMSE on test set
rmse_test <- rmse(y_test, y_test_pred)
print(paste("Test RMSE:", rmse_test))

# Compute R² on test set
r2_test <- cor(y_test, y_test_pred)^2
print(paste("Test R²:", r2_test))

# Plot actual vs. predicted values 

df_results <- data.frame(Actual = y_test, Predicted = y_test_pred)

ggplot(df_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = paste("Actual vs. Predicted Test Set", response, "based on volumetric data and sex"),
       x = "Actual Values", y = "Predicted Values")

# Calculate feature importance
importance_matrix <- xgb.importance(model = final_model)
xgb.plot.importance(importance_matrix)


