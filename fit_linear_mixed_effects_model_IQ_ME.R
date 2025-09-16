fit_linear_mixed_effects_model_IQ_ME <- function(score_column, data) {
  
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
  columns_to_keep <- c("Identifiers", "Group", "Sex", "IQ", "ME", "Time", "Score")  # Specify columns to keep
  long_data <- long_data %>% select(all_of(columns_to_keep))
  
  # long_data$Time = factor(long_data$Time)
  
  final_data <- long_data %>%
    filter(!is.na(Score))  # Remove rows with missing scores
  
  counts <- final_data %>%
    group_by(Time, Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  print('Number of Subjects Per Age Point')
  print(counts)
  
  # Create a model that relates scores at all ages to group and time point, takes sex 
  # IQ and ME into account and includes subject as random factor
  model <- lmer(Score ~ Sex + IQ + ME + Group + Time + Group * Time 
                + (1 | Identifiers), data = final_data)
  
  print(paste("Model using", score_column, "as School Age EF measure"))
  print(paste("All Scores Converted to Z-scores"))
  print(summary(model))
  print(anova(model))
  
  # Compute emmeans for all factors involved
  emm <- emmeans(model,  ~ Group * Time)
  
  # Group contrasts at each timepoint 
  group_by_time <- contrast(emm, "pairwise", by = "Time") %>%
    summary(adjust = "none") %>%
    mutate(
      EffectType = "Interaction: Group × Time",
      Interval = case_when(
        Time == "12_months" ~ "12 months",
        Time == "24_months" ~ "24 months",
        Time == "school_age" ~ "school age",
        TRUE ~ as.character(Time)
      ),
      ContrastLabel = contrast
    )
  
  # Time contrasts within each group 
  time_by_group <- contrast(emm, "pairwise", by = "Group") %>%
    summary(adjust = "none") %>%
    mutate(
      EffectType = "Interaction: Group × Time",
      GroupLabel = Group,  # keep the group for each row
      # clean up contrast labels like "12_months - 24_months"
      Interval = gsub("12_months", "12 months", contrast),
      Interval = gsub("24_months", "24 months", Interval),
      Interval = gsub("school_age", "school age", Interval),
      ContrastLabel = ""
    )
  
  # Combine and apply global FDR correction
  final_table <- bind_rows(group_by_time, time_by_group) %>%
    mutate(
      p.raw = p.value,
      p.adj.fdr = p.adjust(p.raw, method = "fdr") 
    ) %>%
    select(
      EffectType, Interval, GroupLabel, ContrastLabel, estimate, SE, df, t.ratio, p.raw, p.adj.fdr
    )
  
  
  # Format numbers for manuscript
  final_table <- final_table %>%
    mutate(
      estimate = ifelse(is.na(estimate), "", sprintf("%.3f", estimate)),
      SE       = ifelse(is.na(SE), "", sprintf("%.3f", SE)),
      t.ratio  = ifelse(is.na(t.ratio), "", sprintf("%.3f", t.ratio)),
      p.raw  = ifelse(is.na(p.raw), "", sprintf("%.3f", p.raw)),
      p.adj.fdr = ifelse(is.na(p.adj.fdr), "", sprintf("%.3f", p.adj.fdr))
    )
  
  final_table <- final_table %>%
    mutate(
      GroupLabel = ifelse(is.na(GroupLabel), "", as.character(GroupLabel))
    )

  # Export to CSV
  write_csv(final_table, paste0(score_column, "_Interaction_Contrasts_Table.csv"))
  
  # View table
  print(final_table)
 
  # # Compute contrasts separately
  # group_contrasts <- contrast(emm, "pairwise", by = "Time")   # Group | Time
  # time_contrasts  <- contrast(emm, "pairwise", by = "Group")  # Time | Group
  # 
  # # Convert to data frames and add a label column
  # df_group <- as.data.frame(group_contrasts) %>%
  #   mutate(contrast_type = "Group | Time")
  # 
  # df_time <- as.data.frame(time_contrasts) %>%
  #   mutate(contrast_type = "Time | Group")
  # 
  # # Combine both sets into one table
  # all_contrasts <- bind_rows(df_group, df_time)
  # 
  # # Apply global p-value adjustment across all contrasts
  # all_contrasts <- all_contrasts %>%
  #   mutate(
  #     p.adj.global = p.adjust(p.value, method = "fdr"),
  #     # Format p-values as decimal with 3 digits
  #     p.value = formatC(p.value, format = "f", digits = 3),
  #     p.adj.global = formatC(p.adj.global, format = "f", digits = 3)
  #     )   
  # 
  # # Reorder and select columns to print
  # all_contrasts <- all_contrasts %>%
  #   select(Time, Group, contrast, estimate, SE, df, t.ratio, p.value, p.adj.global, contrast_type)
  # 
  # # Replace NA values with empty space
  # all_contrasts_print <- all_contrasts %>%
  #   mutate(across(where(is.factor), as.character)) %>%  # convert only factors to character
  #   mutate(across(everything(), ~ ifelse(is.na(.), "", .)))  # replace NA with blanks
  # 
  # # Print to screen
  # print(all_contrasts_print)
  
  
  return(list(final_data = final_data, model = model))
  
}