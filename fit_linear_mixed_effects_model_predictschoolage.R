
fit_linear_mixed_effects_model_predictschoolage <- function(score_column, data) {
  
  # Remove rows with no school age score
  data_filtered0 <- data[!is.na(data[[score_column]]), ]
  
  data_no12 <- data_filtered0[is.na(data_filtered0[["AB_12_Percent"]]), ]
  data_no24 <- data_filtered0[is.na(data_filtered0[["AB_24_Percent"]]), ]
  
  # Remove rows where there is missing School Age score  AB_12_Percent or AB_24_Percent
  data_filtered <- data_filtered0 %>%
    filter(!is.na(AB_12_Percent) & !is.na(AB_24_Percent))
  
  # Keep only columns that will be used in modeling
  final_data <- data_filtered[, c("Identifiers", "Group", "Sex", "AB_12_Percent", "AB_24_Percent", "Risk", score_column)]

  final_data$Sex <- factor(final_data$Sex)
  
  # browser()
  
  # Create a model that relates School Age EF Score to EF scores at 12 and 24 months, takes sex and group into account
  # and includes subject as random factor
  
  # Construct the formula
  formula1 <- as.formula(paste(score_column, "~ AB_12_Percent"))
  formula2 <- as.formula(paste(score_column, "~ AB_24_Percent"))
  formula3 <- as.formula(paste(score_column, "~ AB_12_Percent + AB_24_Percent"))
  formula4 <- as.formula(paste(score_column, "~ Risk + AB_12_Percent + AB_24_Percent"))
  formula5 <- as.formula(paste(score_column, "~ Group + AB_12_Percent + AB_24_Percent"))
  formula6 <- as.formula(paste(score_column, "~ Sex + Group + AB_12_Percent + AB_24_Percent"))
  formula7 <- as.formula(paste(score_column, "~ Sex + Risk + AB_12_Percent + AB_24_Percent"))
  
  model1 <- lm(formula1, data = final_data)
  model2 <- lm(formula2, data = final_data)
  model3 <- lm(formula3, data = final_data)
  model4 <- lm(formula4, data = final_data)
  model5 <- lm(formula5, data = final_data)
  model6 <- lm(formula6, data = final_data)
  model7 <- lm(formula6, data = final_data)
  
  # Get the AIC values
  aic_value1 <- AIC(model1)
  aic_value2 <- AIC(model2)
  aic_value3 <- AIC(model3)
  aic_value4 <- AIC(model4)
  aic_value5 <- AIC(model5)
  aic_value6 <- AIC(model6)
  aic_value7 <- AIC(model7)
  
  # Print AIC
  print(paste("model 1 AIC: ", aic_value1))
  print(paste("model 2 AIC: ", aic_value2))
  print(paste("model 3 AIC: ", aic_value3))
  print(paste("model 4 AIC: ", aic_value4))
  print(paste("model 5 AIC: ", aic_value5))
  print(paste("model 6 AIC: ", aic_value6))
  print(paste("model 7 AIC: ", aic_value7))
  
  print(summary(model6))
  
  correlation12 <- cor(final_data$AB_12_Percent, final_data[[score_column]])
  # Scatter plot 
  p1 <- ggplot(data = final_data, aes(x = AB_12_Percent, y = .data[[score_column]])) +
    geom_point() +  # Add points to the plot
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      title = paste("Scatter plot of EF 12 mo vs", score_column, "\nCorrelation: ", round(correlation12, 2)),
      x = "EF 12mo", 
      y = "EF School Age") +
    theme_minimal()
  
  correlation24 <- cor(final_data$AB_24_Percent, final_data[[score_column]])
  # Scatter plot 
  p2 <- ggplot(data = final_data, aes(x = AB_24_Percent, y = .data[[score_column]])) +
    geom_point() +  # Add points to the plot
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      title = paste("Scatter plot of EF 24 mo vs", score_column, "\nCorrelation: ", round(correlation24, 2)),
      x = "EF 24mo", 
      y = "EF School Age") +
    theme_minimal()
  
  print(p1)
  print(p2)
  
}