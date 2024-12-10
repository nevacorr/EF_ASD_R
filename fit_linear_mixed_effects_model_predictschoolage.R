
fit_linear_mixed_effects_model_predictschoolage <- function(score_column, data) {
  
  # Remove rows with no school age score
  data_filtered <- data[!is.na(data[[score_column]]), ]
  
  # Remove rows where there is no School Age score or School Age score but no AB_12_Percent or AB_24_Percent
  data_filtered <- data_filtered %>%
    filter(!is.na(score_column) & (!is.na(AB_12_Percent) & !is.na(AB_24_Percent)))
  
  # data_filtered <- data_filtered %>%
    # filter(!(is.na(AB_12_Percent) & is.na(AB_24_Percent)))
  
  # Keep only columns that will be used in modeling
  final_data <- data_filtered[, c("Identifiers", "Group", "Sex", "AB_12_Percent", "AB_24_Percent", score_column)]

  final_data$Sex <- factor(final_data$Sex)
  
  lengths <- sapply(final_data, length)
  print(lengths)
  
  print(sum(is.na(final_data$Sex)))
  print(sum(is.na(final_data$Group)))
  print(sum(is.na(final_data$AB_12_Percent)))
  print(sum(is.na(final_data$AB_24_Percent)))
  
  print(class(final_data$Sex))  # should be "factor"
  print(class(final_data$Group))  # should be "factor"
  
  print(class(final_data$AB_12_Percent))  # should be "numeric"
  print(class(final_data$AB_24_Percent))  # should be "numeric"
  
  print(anyDuplicated(final_data))
  
  # browser()
  
  # Create a model that relates School Age EF Score to EF scores at 12 and 24 months, takes sex and group into account
  # and includes subject as random factor
  
  # Dynamically construct the formula
  formula <- as.formula(paste(score_column, "~ Sex + Group + AB_24_Percent"))
  
  model <- lm(formula, data = final_data)
  
  # See estimates for fixed and random effects and test hypotheses about group differences
  # print(paste("Model using", score_column, "as EF"))
  print(summary(model))
  # print(anova(model))
  
#   # Get predicted values from the model
#   final_data$predicted_score <- predict(model)
#   
#   # Plot the predicted scores by Group and Time
#   plot <- ggplot(final_data, aes(x = Group, y = predicted_score)) +
#     geom_boxplot() +
#     labs(
#       title = paste("Predicted Scores by Group and Time\n(", score_column,
#                     " Used for School Age Score)", sep = ""),
#       x = "Group",
#       y = "Predicted Score"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   print(plot)
#   
#   # Filter the data to keep only rows for school age scores
#   school_age_data <- final_data %>%
#     filter(Time == "school_age")
# 
#   # Predict the scores for school age using the model
#   school_age_data$predicted_score <- predict(model, newdata = school_age_data)
# 
#   # Calculate the correlation between predicted and actual school age scores
#   correlation <- cor(school_age_data$predicted_score, school_age_data$Score)
# 
#   # Create the plot with the correlation value in the title, with a regression line
#   plot = ggplot(school_age_data, aes(x = predicted_score, y = Score)) +
#     geom_point() +  # scatter plot of predicted vs actual
#     geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add linear regression line without confidence interval shading
#     labs(
#       title = paste("Predicted vs Actual Scores at School Age:", score_column,"\nCorrelation: ", round(correlation, 2)),
#       x = "Predicted Score (from 12 and 24 months)",
#       y = "Actual School Age Score"
#     ) +
#     theme_minimal()
# 
#   print(plot)
#   
#   # Return both final_data and model as a list
#   return(list(final_data = final_data, model = model))
# 
}
