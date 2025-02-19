
library(dplyr)
library(tidyr)

# Plot histogram of a dataframe column
plot_histogram <- function(df, column_name) {
  # Check if the column exists in the dataframe
  if (!(column_name %in% colnames(df))) {
    stop("Column not found in the dataframe")
  }
  
  # Convert the column to numeric if necessary
  column_data <- df[[column_name]]
  
  # If the column is not numeric, try converting it
  if (!is.numeric(column_data)) {
    column_data <- as.numeric(as.character(column_data))
  }
  
  # Remove NAs before plotting
  column_data <- na.omit(column_data)
  
  # Create the histogram
  hist(column_data, 
       main = paste("Histogram of", column_name), 
       xlab = "Values", 
       col = "blue", 
       border = "black")
}

# Check assumptions of normality
# Q-Q Plot of Residuals
plot_qq <- function(anova_result, title_text) {
  stats_shapiro = shapiro.test(residuals(anova_result))
  p_value <- stats_shapiro$p.value
  p_value_exp <- formatC(p_value, format = "e", digits = 2)
  qqnorm(residuals(anova_result), 
       main = paste(title_text, " Q-Q Plot of Residuals \nShapiro p =", p_value_exp))
  qqline(residuals(anova_result), col = "red") 
}

remove_bad_quality_data <- function(df) {
  
  df[df == "."] <- NA
  
  qc_cols <- grep("_PassFail$", names(df), value = TRUE)
  
  region_substrings <- sub("_PassFail$", "", qc_cols)
  
  for (i in seq_along(qc_cols)) {
    qc_col <- qc_cols[i]
    region_matches <- grep(region_substrings[i], names(df), value = TRUE)
    
    for (region_col in region_matches) {
      df[[region_col]][df[[qc_col]] == "Fail"] <- NA
    }
  }
  
  df <- df[, !names(df) %in% qc_cols]
  
  # Remove columns that contain "VQC" or "Exclude_Reason"
  df <- df[, !grepl("VQC|Exclude_Reason|Visit", colnames(df))]
}

remove_bad_quality_data_12_24 <- function(df, region_substrings) {
  
  df[df == "."] <- NA
  
  qc_cols <- grep("_VQC$", names(df), value = TRUE)
  
  for (i in seq_along(region_substrings)) {
    region_matches <- grep(region_substrings[i], names(df), value = TRUE)
    region_matches <- region_matches[!grepl("VQC", region_matches)]
    qc_match <- grep(paste0(region_substrings[i], ".*_VQC$"), names(df), value = TRUE)
    
    for (region_col in region_matches) {
      df[[region_col]][df[[qc_match]] == "fail"] <- NA
    }
  }
  
  # Remove columns that will not be used in modeling
  df <- df[, !grepl("VQC|ExcludeReason|Occ|Temp|Edited|Combined", colnames(df))]
  
  # Find all column names that contain any of the region substrings
  var_names <- grep(paste(region_substrings, collapse = "|"), names(df), value = TRUE)
  
  # Ensure that we found columns
  if (length(var_names) == 0) {
    stop("No columns found matching the specified substrings.")
  }
  
  # Make separate columns for 12 and 24 month data
  df <- df %>%
    pivot_wider(
      names_from = Visit,  # Spread the "Visit" values into columns
      values_from = all_of(var_names),  # Use the columns that match the substrings
      names_glue = "{.value}_{Visit}"  # New column names with Visit suffix
    )
  
  # Rename DCCID column
  df <- df %>%
    rename(CandID = DCCID)
  
  
  return(df)
}