
# Function to scale specified columns to Z-scores, keeping NaNs 
scale_columns_to_zscore <- function(df, columns) {
  scale_with_na <- function(x) {
    # Calculate mean and standard deviation, ignoring NaNs
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    
    # Return scaled values, preserving NaNs
    (x - mean_x) / sd_x
  }
  
  # Apply the scaling function to the specified columns
  df[columns] <- lapply(df[columns], scale_with_na)
  
  # Return the updated dataframe
  return(df)
}