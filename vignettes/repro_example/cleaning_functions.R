#' Load raw data
#' @export
load_data <- function(path) {
  "
  :name: Load Data
  :short: Reads CSV file
  :long: Reads a CSV file from the specified path into a dataframe.
  "
  read.csv(path)
}

#' Remove missing values
#' @export
clean_missing <- function(df) {
  "
  :name: Clean Missing
  :short: Removes NA types
  :long: Filters out any rows that contain NA values in the metric column.
  "
  df[!is.na(df$metric), ]
}

#' Normalize metrics
#' @export
normalize_metric <- function(df) {
  "
  :name: Normalize
  :short: Z-score normalization
  :long: Standardizes the metric column by subtracting the mean and dividing by the standard deviation.
  "
  df$metric <- scale(df$metric)
  df
}
