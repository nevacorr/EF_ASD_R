fit_linear_mixed_effects_model_Final <- function(score_column, data, use_covariates) {
  
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
  
  # long_data$Time = factor(long_data$Time)
  
  final_data <- long_data %>%
    filter(!is.na(Score))  # Remove rows with missing scores
  
  # Calculate number of subjects per age group
  counts <- final_data %>%
    group_by(Time, Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  if (use_covariates) {
  # Create a model that relates scores at all ages to group and time point, takes sex
  # IQ and ME into account and includes subject as random factor
    model <- lmer(Score ~ Sex + IQ + ME + Group + Time + Group * Time
              + (1 | Identifiers), data = final_data)
  }
  else {
    model <- lmer(Score ~ Sex + Group + Time + Group * Time
                  + (1 | Identifiers), data = final_data)
  }
    
 # Compute emmeans for all factors involved
  emm <- emmeans(model,  ~ Group * Time)

  if (use_covariates) {
    covstr = '_IQandMEcovariates'
  }
  else {
    covstr = ''
  }
  
  # Print results to screen and to file
  print_and_save_summary_Final(score_column, model, counts, emm, 
                               covstr, final_data, use_covariates)

  return(list(final_data = final_data, model = model))

 }