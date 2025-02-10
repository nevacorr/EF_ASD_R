# install.packages("car")

library(dplyr)
library(car)

rm(list = ls())

# Load data
ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS_BRIEF2.csv"))

# Remove duplicate rows
ibis_behav <- ibis_behav[!duplicated(ibis_behav$Identifiers), ]

standardize <- 0 # Indicate whether to convert EF scores to Z scores

if (standardize == 1) {
  # Calculate z-scores for scores
  ibis_behav$Flanker_Standard_Age_Corrected <- (ibis_behav$Flanker_Standard_Age_Corrected -
                                                  mean(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)
  ibis_behav$DCCS_Standard_Age_Corrected <- (ibis_behav$DCCS_Standard_Age_Corrected -
                                               mean(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)
  ibis_behav$BRIEF2_GEC_T_score <- (ibis_behav$BRIEF2_GEC_T_score -
                                               mean(ibis_behav$BRIEF2_GEC_T_score, na.rm = TRUE)) / sd(ibis_behav$BRIEF2_GEC_T_score, na.rm = TRUE)
  }

# Remove all but columns to keep
# flanker_df <-ibis_behav %>% select(c("Group", "Flanker_Standard_Age_Corrected"))
# dccs_df <-ibis_behav %>% select(c("Group", "DCCS_Standard_Age_Corrected"))
# brief2_df <- ibis_behav %>% select(c("Group", "BRIEF2_GEC_T_score"))
twelvemo_df <- ibis_behav %>% select(c("Group", "AB_12_Percent"))
twenty4mo_df <- ibis_behav %>% select(c("Group", "AB_24_Percent"))

# Convert empty Group string values to NA 
# flanker_df$Group[flanker_df$Group == ""] <- NA  
# dccs_df$Group[dccs_df$Group == ""] <- NA 
# brief2_df$Group[brief2_df$Group == ""] <- NA
twelvemo_df$Group[twelvemo_df$Group == ""] <- NA
twenty4mo_df$Group[twenty4mo_df$Group == ""] <- NA

# Remove all rows with NA in any column from both dataframes
# flanker_df_clean <- na.omit(flanker_df)
# dccs_df_clean <- na.omit(dccs_df)
# brief2_df_clean <- na.omit(brief2_df)
twelvemo_df_clean <- na.omit(twelvemo_df)
twenty4mo_df_clean <- na.omit(twenty4mo_df)

# Remove LR+ group
# flanker_df_clean <- flanker_df_clean %>% filter(Group != "LR+")
# dccs_df_clean <- dccs_df_clean %>% filter(Group != "LR+")
# brief2_df_clean <- brief2_df_clean %>% filter(Group != "LR+")
twelvemo_df_clean <- twelvemo_df_clean %>% filter(Group != "LR+")
twenty4mo_df_clean <- twenty4mo_df_clean %>% filter(Group != "LR+")

# Set Group as a factor (categorical variable)
# flanker_df_clean$Group <- as.factor(flanker_df_clean$Group)
# dccs_df_clean$Group <- as.factor(dccs_df_clean$Group)
# brief2_df_clean$Group <- as.factor(brief2_df_clean$Group)
twelvemo_df_clean$Group <- as.factor(twelvemo_df_clean$Group)
twenty4mo_df_clean$Group <- as.factor(twenty4mo_df_clean$Group)

# Count the number of rows for each risk category for the two dataframes
# flanker_group_counts <- table(flanker_df_clean$Group)
# dccs_group_counts <- table(dccs_df_clean$Group)
# brief2_group_counts <- table
twelvemo_group_counts <- table(twelvemo_df_clean$Group)
twenty4mo_group_counts <- table(twenty4mo_df_clean$Group)

# # Print group counts
# cat('\nFlanker Group Counts')
# print(flanker_group_counts)
# cat('\nDCCS Group Counts')
# print(dccs_group_counts)
# cat('\nBrief2 Group Counts')
# print(brief2_group_counts)
cat('\n12 Month Group Counts')
print(twelvemo_group_counts)
cat('\n24 Month Group Counts')
print(twenty4mo_group_counts)

# Calculate one-way ANOVA to determine whether scores for the groups are statistically different
# flanker_anova_result <- aov(Flanker_Standard_Age_Corrected ~ Group, data = flanker_df_clean)
# dccs_anova_result <- aov(DCCS_Standard_Age_Corrected ~ Group, data = dccs_df_clean)
# brief2_anova_result <- aov(BRIEF2_GEC_T_score ~ Group, data = brief2_df_clean)
twelvemo_anova_result <- aov(AB_12_Percent ~ Group, data = twelvemo_df_clean)
twenty4mo_anova_result <- aov(AB_24_Percent ~ Group, data = twenty4mo_df_clean)

# # Print ANOVA results
# cat("\nANOVA results for Flanker")
# print(summary(flanker_anova_result))
# cat('\nANOVA results for DCCS')
# print(summary(dccs_anova_result))
# cat('\nANOVA results for Brief2')
# print(summary(brief2_anova_result))
cat('\nANOVA results for 12 Months')
print(summary(twelvemo_anova_result))
cat('\nANOVA results for 24 months')
print(summary(twenty4mo_anova_result))

# Perform Tukey post-hoc tests
# flanker_tukey_result <- TukeyHSD(flanker_anova_result)
# dccs_tukey_result <- TukeyHSD(dccs_anova_result)
# brief2_tukey_result <- TukeyHSD(brief2_anova_result)
twelvemo_tukey_result <- TukeyHSD(twelvemo_anova_result)
twenty4mo_tukey_result <- TukeyHSD(twenty4mo_anova_result)

# # Print Tukey results
# cat('\nTukey results for Flanker')
# print(flanker_tukey_result)
# cat('\nTukey results for DCCS')
# print(dccs_tukey_result)
# cat('\nTukey results for Brief2')
# print(brief2_tukey_result)
cat('\nTukey results for 12 months')
print(twelvemo_tukey_result)
cat('\nTukey results for 24 months')
print(twenty4mo_tukey_result)

# # Boxplot
# boxplot(Flanker_Standard_Age_Corrected ~ Group, data = flanker_df_clean,
#         main = "School Age Data: Flanker Score by Group",
#         xlab = "Group",
#         ylab = "Score",
#         col = c("skyblue", "lightgreen", "pink"))
# 
# # Boxplot
# boxplot(DCCS_Standard_Age_Corrected ~ Group, data = dccs_df_clean,
#         main = "School Age Data: DCCS Score by Group",
#         xlab = "Group",
#         ylab = "Score",
#         col = c("skyblue", "lightgreen", "pink"))
# # Boxplot
# boxplot(BRIEF2_GEC_T_score ~ Group, data = brief2_df_clean,
#         main = "School Age Data: BRIEF2 Score by Group",
#         xlab = "Group",
#         ylab = "Score",
#         col = c("skyblue", "lightgreen", "pink"))
boxplot(AB_12_Percent ~ Group, data = twelvemo_df_clean,
        main = "12 Month Data: AB Score by Group",
        xlab = "Group",
        ylab = "Score",
        col = c("skyblue", "lightgreen", "pink"))
boxplot(AB_24_Percent ~ Group, data = twenty4mo_df_clean,
        main = "24 Month Data: AB Score by Group",
        xlab = "Group",
        ylab = "Score",
        col = c("skyblue", "lightgreen", "pink"))

source("Utility_Functions.R")

# Plot histograms of data
# plot_histogram(flanker_df_clean, "Flanker_Standard_Age_Corrected")
# plot_histogram(dccs_df_clean, "DCCS_Standard_Age_Corrected")
# plot_histogram(brief2_df_clean, "BRIEF2_GEC_T_score")
plot_histogram(twelvemo_df_clean, "AB_12_Percent")
plot_histogram(twenty4mo_df_clean, "AB_24_Percent")

# Test for normality of anova residuals
# plot_qq(flanker_anova_result, "Flanker School Age")
# plot_qq(dccs_anova_result, "DCCS School Age")
# plot_qq(brief2_anova_result, "Brief2 GEC T-score")
plot_qq(twelvemo_anova_result, "AB_12_Percent")
plot_qq(twenty4mo_anova_result, "AB_24_Percent")

# Test for equal variances using Levene's test
# flanker_levene_result <- leveneTest(Flanker_Standard_Age_Corrected ~ Group, data = flanker_df_clean)
# print(flanker_levene_result)
# dccs_levene_result <- leveneTest(DCCS_Standard_Age_Corrected ~ Group, data = dccs_df_clean)
# print(dccs_levene_result)
# brief2_levene_result <- leveneTest(BRIEF2_GEC_T_score ~ Group, data = brief2_df_clean)
# print(brief2_levene_result)
twelvemo_levene_result <- leveneTest(AB_12_Percent ~ Group, data = twelvemo_df_clean)
print(twelvemo_levene_result)
twenty4mo_levene_result <- leveneTest(AB_24_Percent ~ Group, data = twenty4mo_df_clean)
print(twenty4mo_levene_result)