#' Load raw data
#' @export
load_data <- function(path) {
  # @outliner_step
  # label: Load raw measurements
  # summary: Read the source CSV into a tabular dataset
  # details:
  # - import the raw file specified by the path argument
  # - keep the data in a rectangular structure for downstream cleaning
  read.csv(path)
}

#' Remove missing values
#' @export
clean_missing <- function(df) {
  # @outliner_step
  # label: Drop incomplete measurements
  # summary: Remove rows with missing values in the metric column
  # modifies: df
  # details:
  # - keep only rows where the metric is observed
  # - preserve the original columns for later normalization
  df[!is.na(df$metric), ]
}

#' Normalize metrics
#' @export
normalize_metric <- function(df) {
  # @outliner_step
  # label: Standardize metric values
  # summary: Convert the metric column to z-scores
  # modifies: df
  # details:
  # - subtract the sample mean from each metric value
  # - divide by the sample standard deviation
  df$metric <- scale(df$metric)
  df
}
