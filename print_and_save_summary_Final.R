print_and_save_summary_Final <- function(score_column, model, counts, emm, covstr, 
                                   final_data, use_covariates) {
  
  # Filename to save summary
  model_summary_file <- paste0(score_column, covstr, "_LME_model_summary.xlsx")
  
  # MODEL SUMMARY
  fixed_effects <- summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Term") %>%
    dplyr::rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      df = `df`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) 

  # COUNTS
  counts_df <- counts %>%
    mutate(Term = paste("Count -", Time, "-", Group),
           Count = as.character(Count)) %>%
    select(Term, Count)
  
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
    ) %>%
    mutate(
      estimate = ifelse(is.na(estimate), "", sprintf("%.3f", estimate)),
      SE       = ifelse(is.na(SE), "", sprintf("%.3f", SE)),
      t.ratio  = ifelse(is.na(t.ratio), "", sprintf("%.3f", t.ratio)),
      p.raw    = ifelse(is.na(p.raw), "", sprintf("%.3f", p.raw)),
      p.adj.fdr = ifelse(is.na(p.adj.fdr), "", sprintf("%.3f", p.adj.fdr)),
      GroupLabel = ifelse(is.na(GroupLabel), "", as.character(GroupLabel))
    )
  
  # Unique subjects
  subjects_df <- final_data %>%
    distinct(Identifiers) %>%
    arrange(Identifiers)
  
  # Model equation
  model_eq <- data.frame(Model_Equation = deparse(formula(model)))
  
  # Save all to excel
  write_xlsx(
    list(
      Model_Summary = fixed_effects,
      Counts = counts_df,
      Contrasts = final_table, 
      Unique_Subjects = subjects_df,
      Model_Equation = model_eq
    ),
    path = model_summary_file
  )
  
  # Also print to screen
  print(score_column)
  print(model_eq)
  print(fixed_effects)
  print(counts_df)
  print(final_table)
  
}