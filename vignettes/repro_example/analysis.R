# Example Analysis Pipeline
source("cleaning_functions.R")

# 1. Load Data
raw <- load_data("data/raw_data.csv")

# 2. Clean Data
cleaned <- clean_missing(raw)

# 3. Process
final <- normalize_metric(cleaned)

# 4. Save (Simulated)
# write.csv(final, "data/clean_data.csv")
