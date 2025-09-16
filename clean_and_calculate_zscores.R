# Helper function to clean data and calculate z-scores
clean_and_calculate_zscores <- function(df, column, use_covariates = TRUE) {
  
  # Convert empty Group string values to NA 
  df$Group[df$Group == ""] <- NA
  
  # Convert empty maternal education string values to NA 
  df$V06.tsi.mother_education[df$V06.tsi.mother_education == ""] <- NA

  # Remove all rows containing NA values except for V24 mullens and mother education columns
  df <- df %>%
    filter(if_all(-c(V24.mullen.composite_standard_score, 
                     V06.tsi.mother_education), ~ !is.na(.)))
  
  if (use_covariates) {
    # # Remove rows where V24 mullens is NA
    num_removed_IQ <- df %>% filter(is.na(V24.mullen.composite_standard_score)) %>% nrow()
    df_clean <- df %>% filter(!is.na(V24.mullen.composite_standard_score))
    cat(column, "removed", num_removed_IQ, "rows missing IQ\n")
    
    # # # Remove rows where maternal education is NA
    num_removed_ME <- df_clean %>% filter(is.na(V06.tsi.mother_education)) %>% nrow()
    df_clean <- df_clean %>% filter(!is.na(V06.tsi.mother_education))
    cat(column, "removed", num_removed_ME, "rows missing maternal education\n")
  }
  else {
    df_clean <-  df
  }
    
  # Remove mother education string column
  df_clean <- df_clean %>% select(-V06.tsi.mother_education)
  
  # Remove LR+ group
  df_clean <- df_clean %>% filter(Group != "LR+")
  
  # Rename groups
  df_clean <- df_clean %>%
    mutate(Group = recode(Group, 
                          "HR+" = "HL-ASD", 
                          "HR-" = "HL-noASD", 
                          "LR-" = "LL"))
  
  # Replace score with z score  
  df_clean[[column]]<- (df_clean[[column]] - mean(df_clean[[column]], na.rm = TRUE)) / sd(df_clean[[column]], na.rm = TRUE)
  
  return(df_clean)
}