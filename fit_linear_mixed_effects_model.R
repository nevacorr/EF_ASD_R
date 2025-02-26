
fit_linear_mixed_effects_model <- function(score_column, data, dummy_encode) {
  
  # Reshape the data from wide to long format
  long_data <- data %>%
    pivot_longer(
      cols = c("AB_12_Percent", "AB_24_Percent", score_column),
      names_to = "Time",
      values_to = "Score"
    ) 
  
  # Remove columns for school age EF variables not to be modeled 
  columns_to_keep <- c("Identifiers", "Group", "Sex", "Time", "Score")  # Specify columns to keep
  long_data <- long_data %>% select(all_of(columns_to_keep))
  
  # Change name of score_column to school_age
  # Make time an ordered factor so it is interpreted chronologically
  long_data <- long_data %>%
    mutate(Time = recode(Time, !!score_column := "school_age")) %>%
    mutate(Time = factor(Time, levels = c("school_age", "AB_24_Percent", "AB_12_Percent")))
  
  final_data <- long_data %>%
   filter(!is.na(Score))  # Remove rows with missing scores
  
  if (dummy_encode == 1) {
    print("Dummy coding Time")
    # Apply dummy coding to Time
    contrasts(final_data$Time) <- contr.treatment(3, base=3)
  } else {
    print("Effect coding Time")
    # Apply effect coding to Time
    contrasts(final_data$Time) <- contr.sum(length(levels(final_data$Time)))
  }
  
  # print coding
  print(contrasts(final_data$Time))
  
  counts <- final_data %>%
    group_by(Time, Group) %>%
    summarise(Count = n(), .groups = "drop")
 
  print('Number of Subjects Per Age Point')
  print(counts)
  
  # Create a model that relates scores at all ages to group and time point, takes sex into account
  # and includes subject as random factor
  model <- lmer(Score ~ Sex + Group + Time + Group * Time + (1 | Identifiers), data = final_data)
  
  print(summary(model))
  print(anova(model))
  
  
  
  
  
  # # Extract the model coefficients
  # model_coeffs <- summary(model)$coefficients
  # 
  # # Coefficients of interest
  # HR_minus_coeff <- model_coeffs["Group2", "Estimate"]
  # HR_plus_coeff <- model_coeffs["Group3", "Estimate"]
  # 
  # # Extract the interaction terms for each time point
  # interaction_Time1 <- model_coeffs["Group2:Time1", "Estimate"] - model_coeffs["Group3:Time1", "Estimate"]
  # interaction_Time2 <- model_coeffs["Group2:Time2", "Estimate"] - model_coeffs["Group3:Time2", "Estimate"]
  # 
  # # Calculate the contrast between HR- and HR+ at each time point
  # # Contrast at Time 1 (AB_12_Percent)
  # contrast_time1 <- HR_minus_coeff + interaction_Time1
  # # Contrast at Time 2 (AB_24_Percent)
  # contrast_time2 <- HR_plus_coeff + interaction_Time2
  # 
  # # Print the results
  # cat("Contrast at Time 1 (HR- vs HR+):", contrast_time1, "\n")
  # 
  # # Standard errors of coefficients for HR- and HR+
  # HR_minus_se <- model_coeffs["Group2", "Std. Error"]
  # HR_plus_se <- model_coeffs["Group3", "Std. Error"]
  # 
  # # Calculate the pooled standard error for the contrast
  # pooled_se <- sqrt(HR_minus_se^2 + HR_plus_se^2)
  # 
  # # t-statistic for the contrast
  # t_stat <- contrast_time1 / pooled_se
  # # Degrees of freedom can be approximated using the model output
  # df <- model$dframe$n - length(model_coeffs)
  # 
  # # p-value for the contrast
  # p_value <- 2 * pt(-abs(t_stat), df)
  # 
  # cat("t-statistic:", t_stat, "\n")
  # cat("p-value:", p_value, "\n")
  # 
  # Return both final_data and model as a list
  return(list(final_data = final_data, model = model))

}