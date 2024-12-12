
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
  
  browser()
  
  # Keep only columns that will be used in modeling
  final_data <- long_data_cleaned[, c("Identifiers", "Group", "Sex", "Age_SchoolAge", "Time", "Score")]
  
  # Create a model that relates School Age EF Score to EF scores at 12 and 24 months, takes sex and group into account
  # and includes subject as random factor
  # model <- lmer(Score ~ Sex + Group + Time + Age_SchoolAge + (1 | Identifiers), data = final_data)
  model <- lmer(Score ~ Sex + Group + Time + (1 | Identifiers), data = final_data)
  
  # See estimates for fixed and random effects and test hypotheses about group differences
  print(paste("Model using", score_column, "as EF"))
  print(summary(model))
  print(anova(model))
  
  # Get predicted values from the model
  final_data$predicted_score <- predict(model)
  
  # Plot the predicted scores by Group and Time
  plot <- ggplot(final_data, aes(x = Group, y = predicted_score, color = Time)) +
    geom_boxplot() +
    labs(
      title = paste("Predicted Scores by Group and Time\n(", score_column,
                    " Used for School Age Score)", sep = ""),
      x = "Group",
      y = "Predicted Score"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
  
  # Filter the data to keep only rows for school age scores
  school_age_data <- final_data %>%
    filter(Time == "school_age")

  # Predict the scores for school age using the model
  school_age_data$predicted_score <- predict(model, newdata = school_age_data)

  # Calculate the correlation between predicted and actual school age scores
  correlation <- cor(school_age_data$predicted_score, school_age_data$Score)

  # Create the plot with the correlation value in the title, with a regression line
  plot = ggplot(school_age_data, aes(x = predicted_score, y = Score)) +
    geom_point() +  # scatter plot of predicted vs actual
    geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add linear regression line without confidence interval shading
    labs(
      title = paste("Predicted vs Actual Scores at School Age:", score_column,"\nCorrelation: ", round(correlation, 2)),
      x = "Predicted Score (from 12 and 24 months)",
      y = "Actual School Age Score"
    ) +
    theme_minimal()

  print(plot)
  
  # Return both final_data and model as a list
  return(list(final_data = final_data, model = model))

}