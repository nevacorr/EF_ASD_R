# install.packages("xgboost")
# install.packages("caTools")
# install.packages("cvms")
# install.packages("caret")

library(xgboost)
library(caTools)
library(cvms)
library(dplyr)
library(caret)

rm(list = ls())

ibis_behav <- read.csv(file.path("/Users/nevao/Documents/IBIS_EF/source data/IBIS_behav_dataframe_demographics_AnotB_Flanker_DCCS.csv"))

# Convert empty strings in some columns to NA 
ibis_behav$Group[ibis_behav$Group == ""] <- NA  
ibis_behav$Flanker_Standard_Age_Corrected[ibis_behav$Flanker_Standard_Age_Corrected == ""] <- NA
ibis_behav$DCCS_Standard_Age_Corrected[ibis_behav$DCCS_Standard_Age_Corrected == ""] <- NA

# Calculate z-scores for 12mo, 24mo and school age columns
ibis_behav$AB_12_Percent <- (ibis_behav$AB_12_Percent -
                             mean(ibis_behav$AB_12_Percent, na.rm = TRUE)) / sd(ibis_behav$AB_12_Percent, na.rm = TRUE)
ibis_behav$AB_24_Percent <- (ibis_behav$AB_24_Percent -
                             mean(ibis_behav$AB_24_Percent, na.rm = TRUE)) / sd(ibis_behav$AB_24_Percent, na.rm = TRUE)
ibis_behav$Flanker_Standard_Age_Corrected <- (ibis_behav$Flanker_Standard_Age_Corrected -
                             mean(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$Flanker_Standard_Age_Corrected, na.rm = TRUE)
ibis_behav$DCCS_Standard_Age_Corrected <- (ibis_behav$DCCS_Standard_Age_Corrected -
                              mean(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)) / sd(ibis_behav$DCCS_Standard_Age_Corrected, na.rm = TRUE)
ibis_behav$Average_Flanker_DCCS <- rowMeans(ibis_behav[, c("Flanker_Standard_Age_Corrected", "DCCS_Standard_Age_Corrected")])

# Remove rows with no Group
ibis_behav_filtered <- ibis_behav[!is.na(ibis_behav$Group), ]

# Remove LR+ group
ibis_behav_filtered <- ibis_behav_filtered %>% filter(Group != "LR+")

# Convert Group and Sex to factors
ibis_behav_filtered$Sex <- factor(ibis_behav_filtered$Sex)
ibis_behav_filtered$Group <- factor(ibis_behav_filtered$Group)
ibis_behav_filtered$Identifiers <- factor(ibis_behav_filtered$Identifiers)

# Set 'GroupLR-' as the reference level
ibis_behav_filtered$Group <- relevel(ibis_behav_filtered$Group, ref = "LR-")

# Remove unwanted columns
ibis_behav_filtered <- ibis_behav_filtered %>% select(-c(V12prefrontal_taskCandidate_Age, V24prefrontal_taskCandidate_Age, Age_SchoolAge, 
                                                         AB_Reversals_12_Percent, AB_Reversals_24_Percent))



stop <- 1
