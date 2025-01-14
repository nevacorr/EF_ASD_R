
plot_model_with_age_by_group <- function(result, score_column) {
  final_data <- result$final_data
  model <- result$model
  
  # Get predicted values from the model for each age
  final_data$predicted_score <- predict(model, newdata = final_data)
  
  # Plot the predicted scores by Group and Time (with 12_months, 24_months, school_age on the x-axis)
  plot <- ggplot(final_data, aes(x = Time, color = Group, group = Group)) +
    geom_line(aes(y = predicted_score, linetype = Group), size = 1) +  # Line for predicted scores
    # geom_point(aes(y = Score), size = 2, shape = 21, fill = "white") +  # Scatterplot of real data
    labs(
      title = paste("Model of Scores by Group and Age\n(", score_column, " Used for School Age Score)", sep = ""),
      x = "Age",
      y = "Score"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
}