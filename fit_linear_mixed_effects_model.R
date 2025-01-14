
fit_linear_mixed_effects_model <- function(score_column, data) {
  
  # Reshape the data from wide to long format
  long_data <- data %>%
    pivot_longer(
      cols = c("AB_12_Percent", "AB_24_Percent", score_column),
      names_to = "Time",
      values_to = "Score"
    ) %>%
    mutate(Time = case_when(
      Time == "AB_12_Percent" ~ "12_months",
      Time == "AB_24_Percent" ~ "24_months",
      Time == score_column ~ "school_age"
    ))
  
  long_data_cleaned <- long_data %>%
    filter(!is.na(Score))  # Remove rows with missing scores
  
  # Keep only columns that will be used in modeling
  final_data <- long_data_cleaned[, c("Identifiers", "Group", "Sex", "Age_SchoolAge", "Time", "Score")]
  
  # Create a model that relates scores at all ages to group and time point, takes sex into account
  # and includes subject as random factor
  model <- lmer(Score ~ Sex + Group + Time + Group * Time + (1 | Identifiers), data = final_data)
  
  # See estimates for fixed and random effects and test hypotheses about group differences
  print(paste("Model using", score_column, "as EF"))
  print(summary(model))
  print(anova(model))
  
  # Return both final_data and model as a list
  return(list(final_data = final_data, model = model))

}