
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
  
  # Keep only columns used in modeling 
  columns_to_keep <- c("Identifiers", "Group", "Sex", "Time", "Score")  # Specify columns to keep
  long_data <- long_data %>% select(all_of(columns_to_keep))
  
  # long_data$Time = factor(long_data$Time)
  
  final_data <- long_data %>%
   filter(!is.na(Score))  # Remove rows with missing scores
  
  counts <- final_data %>%
    group_by(Time, Group) %>%
    summarise(Count = n(), .groups = "drop")
 
  print('Number of Subjects Per Age Point')
  print(counts)
  
  # Create a model that relates scores at all ages to group and time point, takes sex into account
  # and includes subject as random factor
  model <- lmer(Score ~ Sex + Group + Time + Group * Time + (1 | Identifiers), data = final_data)
  
  print(summary(model))
  # print(anova(model))
  
  emm <- emmeans(model, pairwise ~ Group | Time, adjust = "tukey")
  
  print(score_column)
  print("emm")
  print(emm)

  
  return(list(final_data = final_data, model = model))

}