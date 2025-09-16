print_and_save_summary <- function(score_column, model, counts, mystr) {
  
  # Filename to save summary
  model_summary_file <- paste0(score_column, mystr, "_LME_model_summary.csv")
  
  # Extract fixed effects table
  fixed_effects <- summary(model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Term") %>%
    dplyr::rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      df = `df`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    mutate(across(everything(), as.character))  # all character for CSV
  
  # Prepare counts as a two-column table
  counts_df <- counts %>%
    mutate(Term = paste("Count -", Time, "-", Group),
           Count = as.character(Count)) %>%
    select(Term, Count)
  
  # Optional blank row between model and counts
  blank_row <- data.frame(matrix("", nrow = 1, ncol = ncol(fixed_effects)))
  colnames(blank_row) <- colnames(fixed_effects)
  
  # Write fixed effects first with headers
  write.table(
    fixed_effects,
    file = model_summary_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )
  
  # Append blank row
  write.table(
    blank_row,
    file = model_summary_file,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE,
    quote = FALSE
  )
  
  # Append counts rows without headers
  # Expand counts to have same columns as fixed_effects for clean CSV
  counts_write <- counts_df %>%
    rename(Estimate = Count) %>%
    mutate(
      SE = "",
      df = "",
      t_value = "",
      p_value = ""
    ) %>%
    select(colnames(fixed_effects))
  
  write.table(
    counts_write,
    file = model_summary_file,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE,
    quote = FALSE
  )
  
  # Print to screen
  print(bind_rows(fixed_effects, blank_row, counts_write))
}