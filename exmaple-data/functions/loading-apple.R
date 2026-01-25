library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)

load_health_data <- function(pth) {
  #load apple health export.xml file
  xml <- xmlParse(pth, encoding = "UTF-8")
  #transform xml file to data frame - select the Record rows from the xml file
  df_health <- XML:::xmlAttrsToDataFrame(xml["//Record"]) %>%
    mutate(startDate = as_datetime(startDate, tz = "Europe/Prague"),
           endDate = as_datetime(endDate, tz = "Europe/Prague"),
           creationDate = as_datetime(creationDate, tz = "Europe/Prague"))
  df_workouts <- XML:::xmlAttrsToDataFrame(xml["//Workout"]) %>%
  mutate(startDate = as_datetime(startDate, tz = "Europe/Prague"),
         endDate = as_datetime(endDate, tz = "Europe/Prague"))

  return(list(health = df_health, workouts = df_workouts))
}

merge_health_workouts <- function(df_health, df_workouts) {
  df_health$workout_starttime <- NA
  for (i in seq_len(nrow(df_workouts))) {
    workout <- df_workouts[i, ]
    df_health$workout_starttime[between(df_health$endDate, workout$startDate, workout$endDate)] <- workout$startDate
  }
  df_health$workout_starttime <- as_datetime(df_health$workout_starttime, tz = "Europe/Prague")
  df_health$time_since_start <- df_health$endDate - df_health$workout_starttime
  return(df_health)
}

process_health_data <- function(df_health) {
  #make value variable numeric
  df_health$value <- as.numeric(as.character(df_health$value))
  # make endDate in a datetime variable POSIXct using lubridate with eastern time zone
  df_health$endDate <- ymd_hms(df_health$endDate, tz = "Europe/Prague")

  ##add in year month date dayofweek hour columns
  df_health$month <- format(df_health$endDate, "%m")
  df_health$year <- format(df_health$endDate, "%Y")
  df_health$date <- format(df_health$endDate, "%Y-%m-%d")
  df_health$dayofweek <- wday(df_health$endDate,
                              label = TRUE, abbr = FALSE)
  df_health$hour <- format(df_health$endDate, "%H")
  return(df_health)
}
