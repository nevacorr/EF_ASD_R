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
  
  # Calculate number of subjects per age group
  counts <- final_data %>%
    group_by(Time, Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Create a model that relates scores at all ages to group and time point, takes sex 
  # IQ and ME into account and includes subject as random factor
  model <- lmer(Score ~ Sex + IQ + ME + Group + Time + Group * Time 
                + (1 | Identifiers), data = final_data)
  
  # Print results to screen and to file
  print_and_save_summary(score_column, model, counts, '_IQandMEcovariates')
  
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
 
  return(list(final_data = final_data, model = model))
  
}