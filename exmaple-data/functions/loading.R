load_airplane_data <- function(folder) {
    files <- list.files(folder, pattern = "txt", full.names = TRUE)
    df_airplanes <- data.frame()
    for (f in files) {
        timestamp <- gsub("(.*)_airplanes.txt", "\\1", basename(f))
        date <- gsub("(.*)_(.*)", "\\1", timestamp)
        time <- gsub("(.*)_(.*)", "\\2", timestamp)
        hour <- gsub("(\\d*)-(\\d.*)-(\\d*)", "\\1", time)
        # if the file is empty skip it
        if (file.size(f) == 0) next
        df <- read.table(f, sep = ";", header = FALSE)
        df$timestamp <- timestamp
        df$date <- date
        df$hour <- hour
        df_airplanes <- rbind(df_airplanes, df)
    }
    return(df_airplanes)
}
