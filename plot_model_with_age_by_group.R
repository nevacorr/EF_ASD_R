library(ggplot2)
library(ggtext)

plot_model_with_age_by_group <- function(result, score_column, mystr) {
  final_data <- result$final_data
  model <- result$model
  
  title <- paste("Standardized Raw Scores by Group and Age\n", score_column, "Used for School Age Score", sep = "")
  
  # Plot actual data and lines connected data from the same subject
  plot <- ggplot(final_data, aes(x = Time, y = Score)) + 
      geom_point(aes(color = Group), alpha = 0.6, size = 1) + # Observed scores
      geom_line(aes(group = Identifiers, color = Group)) +
      labs(title = title,
          x= 'Time',
          y = 'Score') +
      theme_minimal() + 
      theme(
        legend.position = "right",
        panel.grid = element_blank(),  # Remove grid lines
        axis.text.x = element_text(size = 24),   # Set x-axis tick label size
        axis.text.y = element_text(size = 24),    # Set y-axis tick label size
        legend.text = element_text(size = 24),    # Set legend label font size
        axis.title.x = element_text(size = 26),  # Set x-axis label font size
        axis.title.y = element_text(size = 26),   # Set y-axis label font size
        legend.title = element_text(size = 26)   # Set legend label font size
     )
  
  # Save plot to file
  ggsave(paste("Standardized Raw Scores by Group and Age", score_column, ".png"), plot = plot, dpi = 300, bg="white")
  
  # Get predicted values from the model for each age
  final_data$predicted_score <- predict(model, newdata = final_data)
  
  # Plot modeled data for each subject individually
  plot <- ggplot(final_data, aes(x = Time, y = Score, color = Group, group = interaction(Group, Identifiers))) +
    geom_point(alpha = 0.6, size = 1) +  # Observed scores
    geom_line(aes(y = predicted_score), size = 0.2) + # Predicted scores
    labs(title = paste("Model of Scores by Group and Age\n(", score_column, " Used for School Age Score)", sep = ""),
         x = "Time",
         y = "Score") +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),  # Remove grid lines
      axis.text.x = element_text(size = 11),   # Set x-axis tick label size
      axis.text.y = element_text(size = 11),    # Set y-axis tick label size
      legend.text = element_text(size = 11),    # Set legend label font size
      axis.title.x = element_text(size = 12),  # Set x-axis label font size
      axis.title.y = element_text(size = 12),   # Set y-axis label font size
      legend.title = element_text(size = 12)   # Set legend label font size
          )
  
  # Save plot to file
  ggsave(paste("Model of Individual Scores by Age", score_column, ".png"), plot = plot, dpi = 300, bg="white")

  # Average the data across all subjects in each group
  group_summary <- final_data %>%
    group_by(Group, Time) %>%
    summarize(mean_predicted_score = mean(predicted_score),
              mean_observed_score = mean(Score),
              .groups = "drop") 

  plot <- ggplot(group_summary, aes(x = Time, y = mean_predicted_score, color = Group, group = Group)) +
    geom_point(data = final_data,                        # Plot individual data points
               aes(x = Time, y = Score, color = Group), 
               alpha = 0.6, size = 2) + 
    #geom_point(aes(y = mean_observed_score), size = 4) +  # Mean observed scores (group level)
    geom_line(size = 1) +                                 # Group-level predicted score lines
    labs(title = paste("<b>Executive Function vs. Age by Group</b><br>", 
                       score_column, " used for school age score", sep = ""),
         x = "Age",
         y = "EF Score") +
    theme_minimal() +
    theme(
      plot.title = element_markdown(size = 18, hjust = 0.5),  # Center title
      legend.position = "right",
      panel.grid = element_blank(),  # Remove grid lines
      axis.text.x = element_text(size = 16),   # Set x-axis tick label size
      axis.text.y = element_text(size = 16),    # Set y-axis tick label size
      legend.text = element_text(size = 16),    # Set legend label font size
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Set x-axis label font size
      axis.title.y = element_text(size = 18, margin = margin(r = 10)),   # Set y-axis label font size
      legend.title = element_text(size = 18),   # Set legend label font size
    )
  
  print(plot)
  
  # Save plot to file
  ggsave(paste("Model of Standardized Scores by Group and Age", score_column, "Used for School Age Score", mystr, ".png"), plot = plot, dpi = 300, bg="white")
  
  }