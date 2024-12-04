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

# Remove rows where there is no Flanker score or Flanker score but no AB_12_Percent or AB_24_Percent
ibis_behav_filtered_2 <- ibis_behav_filtered %>%
  filter(!is.na(Flanker_Standard_Age_Corrected) & 
           (!is.na(AB_12_Percent) | !is.na(AB_24_Percent)))

# Convert 'Group' to a factor
ibis_behav_filtered_2$Group <- factor(ibis_behav_filtered_2$Group)

# Set 'GroupLR-' as the reference level
ibis_behav_filtered_2$Group <- relevel(ibis_behav_filtered_2$Group, ref = "LR-")


# Reshape the data from wide to long format
long_data <- ibis_behav_filtered_2 %>%
  pivot_longer(
    cols = c("AB_12_Percent", "AB_24_Percent", "Flanker_Standard_Age_Corrected"),
    names_to = "Time",
    values_to = "Score"
  ) %>%
  mutate(Time = case_when(
    Time == "AB_12_Percent" ~ "12_months",
    Time == "AB_24_Percent" ~ "24_months",
    Time == "Flanker_Standard_Age_Corrected" ~ "school_age"
  ))


# Create a model that relates Flanker Score to EF scores at 12 and 24 months, takes sex and group into account
# and includes subject number as random factor
model <- lmer(Score ~ Sex + Group + Time + Age_SchoolAge + (1 | Identifiers), data = long_data)

# See estimates for fixed and random effects and test hypotheses about group differences
summary(model)

# Get predicted values from the model
long_data$predicted_score <- predict(model, newdata = long_data)

# Plot the predicted scores by Group and Time
ggplot(long_data, aes(x = Group, y = predicted_score, color = Time)) +
  geom_boxplot() +
  labs(title = "Predicted Scores by Group and Time",
       x = "Group", y = "Predicted Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))