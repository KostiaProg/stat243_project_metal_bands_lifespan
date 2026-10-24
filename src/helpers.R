library(dplyr)

# calculate gaps between releases
calc_gap <- function(years, stat = c("mean", "median")) {
  valid_yrs <- sort(years[!is.na(years)])
  if (length(valid_yrs) < 2) return(NA_real_)
  
  gaps <- diff(valid_yrs)
  if (match.arg(stat) == "mean") mean(gaps) else median(gaps)
}

# remove outliers using IQR
remove_outliers <- function(df, cols, na.rm = TRUE, keep_na = FALSE) {
  # extract all columns
  target_cols <- names(dplyr::select(df, {{ cols }}))
  
  # for each check outliers
  for (col in target_cols) {
    col_vec <- df[[col]]
    
    first <- quantile(col_vec, 0.25, na.rm = na.rm)
    third <- quantile(col_vec, 0.75, na.rm = na.rm)
    iqr   <- third - first
    
    lower <- first - 1.5 * iqr
    upper <- third + 1.5 * iqr
    
    if (keep_na) {
      df <- df %>% 
        filter(is.na(.data[[col]]) | (.data[[col]] >= lower & .data[[col]] <= upper))
    } else {
      df <- df %>% 
        filter(.data[[col]] >= lower & .data[[col]] <= upper)
    }
  }
  
  df
}