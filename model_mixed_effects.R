# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("lme4")

library(ggplot2)
library(tidyr)
library(lme4)
library(dplyr)

rm(list = ls())

source("./plot_boxplot_pairs.R")

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Clean the Group column: convert empty strings to NA
ibis_behav$Group <- trimws(ibis_behav$Group)  # Remove spaces
ibis_behav$Group[ibis_behav$Group == ""] <- NA  # Convert empty strings to NA

# Filter rows
ibis_behav_filtered <- ibis_behav[!is.na(ibis_behav$Group), ]

# # Check for NA values in your key columns
# sum(is.na(ibis_behav_filtered$Score_Value))
# sum(is.na(ibis_behav_filtered$Group))

plot_boxplot_pairs(ibis_behav_filtered, "AB_12_Percent", "AB_24_Percent", "AB Scores Box Plots")

plot_boxplot_pairs(ibis_behav_filtered, "AB_Reversals_12_Percent", "AB_Reversals_24_Percent", "AB Reversal Box Plots")

plot_boxplot_pairs(ibis_behav_filtered, "Flanker_Standard_Age_Corrected", "DCCS_Standard_Age_Corrected", 
                   "School Age Executive Function Measure Box Plots")

# Scale score columns
scaled_columns <- scale(ibis_behav[, c("AB_12_Percent", "AB_24_Percent", "AB_Reversals_12_Percent",
                                       "AB_Reversals_24_Percent", "Flanker_Standard_Age_Corrected",
                                       "DCCS_Standard_Age_Corrected")])

# Replace the original columns with the scaled values
ibis_behav[, c("AB_12_Percent", "AB_24_Percent", "AB_Reversals_12_Percent",
               "AB_Reversals_24_Percent", "Flanker_Standard_Age_Corrected",
               "DCCS_Standard_Age_Corrected")] <- scaled_columns

# Create a long format dataframe for mixed effects modeling.
# Need age in one column, and all Z scores in another column
scores_long_df <- ibis_behav %>%
  pivot_longer(cols = ends_with("Age"),
               values_to = "Age",
               names_to = NULL
  ) %>%
  pivot_longer(c("AB_12_Percent", "AB_24_Percent", "Flanker_Standard_Age_Corrected", 
                 "DCCS_Standard_Age_Corrected"),
               values_to = "Zscore", 
               names_to = NULL
  )

# Create a model that accounts for group and age and includes random intercepts and slopes by ID to capture
# individual variations. 
# Group*Age allows for interaction between group and age. Group:Age will show whether the groups
# have different trajectories over time
model <- lmer(Zscore ~ Risk * Age + (1 + Age | Identifiers), data = scores_long_df)

# See estimates for fixed and random effects and test hypotheses about group differences
summary(model)

# Generate predictions for the model
scores_long_df$Predicted <- predict(model)

# Plot model and model predictions
ggplot(scores_long_df, aes(x = Age, y=Score, color = Group)) + 
  # Create scatter plot of actual data
  geom_point(alpha = 0.6) +
  # Create a dashed line showing the models' predicted trajectory for each individual
  geom_line(aes(y = Predicted, group = ID), linetype = "dashed") +
  # Smooth trend line for groups  
  geom_smmooth(aes(y = Predicted), method = "loess", se = FALSE, linetype = "solid")
