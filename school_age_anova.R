
library(dplyr)

rm(list = ls())

# Load data
ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Remove all but columns to keep
flanker_df <-ibis_behav %>% select(c("Group", "Flanker_Standard_Age_Corrected"))
dccs_df <-ibis_behav %>% select(c("Group", "DCCS_Standard_Age_Corrected"))

# Convert empty Group string values to NA 
flanker_df$Group[flanker_df$Group == ""] <- NA  
dccs_df$Group[dccs_df$Group == ""] <- NA 

# Remove all rows with NA in any column from both dataframes
flanker_df_clean <- na.omit(flanker_df)
dccs_df_clean <- na.omit(dccs_df)

# Remove LR+ group
flanker_df_clean <- flanker_df_clean %>% filter(Group != "LR+")
dccs_df_clean <- dccs_df_clean %>% filter(Group != "LR+")

# Set Group as a factor (categorical variable)
flanker_df_clean$Group <- as.factor(flanker_df_clean$Group)
dccs_df_clean$Group <- as.factor(dccs_df_clean$Group)

# Count the number of rows for each risk category for the two dataframes
flanker_group_counts <- table(flanker_df_clean$Group)
dccs_group_counts <- table(dccs_df_clean$Group)

# Print group counts
cat('\nFlanker Group Counts')
print(flanker_group_counts)
cat('\nDCCS Group Counts')
print(dccs_group_counts)

# Calculate one-way ANOVA to determine whether scores for the groups are statistically different
flanker_anova_result <- aov(Flanker_Standard_Age_Corrected ~ Group, data = flanker_df_clean)
dccs_anova_result <- aov(DCCS_Standard_Age_Corrected ~ Group, data = dccs_df_clean)

# Print ANOVA results
cat("\nANOVA results for Flanker")
print(summary(flanker_anova_result))
cat('\nANOVA results for DCCS')
print(summary(dccs_anova_result))

# Perform Tukey post-hoc tests
flanker_tukey_result <- TukeyHSD(flanker_anova_result)
dccs_tukey_result <- TukeyHSD(dccs_anova_result)

# Print Tukey results
cat('\nTukey results for Flanker')
print(flanker_tukey_result)
cat('\nTukey results for DCCS')
print(dccs_tukey_result)

mystop=1

