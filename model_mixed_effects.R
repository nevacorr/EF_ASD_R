# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("lme4")

library(ggplot2)
library(tidyr)
library(lme4)
library(dplyr)

rm(list = ls())

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Clean the Group column: convert empty strings to NA
# ibis_behav$Group <- trimws(ibis_behav$Group)  # Remove spaces
ibis_behav$Group[ibis_behav$Group == ""] <- NA  # Convert empty strings to NA

# Remove rows with no Group
ibis_behav_filtered <- ibis_behav[!is.na(ibis_behav$Group), ]

# Remove rows with no Flanker Score or only Flanker Score
long_flanker <- ibis_behav_filtered %>%
  filter(!is.na(Flanker_Standard_Age_Corrected) & (!is.na(AB_12_Percent) | !is.na(AB_24_Percent)))

# Create a model that relates Flanker Score to EF scores at 12 and 24 months, takes sex and group into account
# and includes subject number as random factor
model <- lmer(Flanker_Standard_Age_Corrected ~ AB_12_Percent + AB_24_Percent + Sex + Group + (1 | Identifiers), data=ibis_behav_filtered)

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
