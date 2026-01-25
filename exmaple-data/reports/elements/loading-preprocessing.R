library(dplyr)
library(stringr)

add_first_condition <- function(df_forest) {
  first_condition <- df_forest %>%
    filter(session == 1) %>%
    select(ID, first_condition = condition)
  df_forest <- df_forest %>%
    left_join(first_condition, by = "ID")
  return(df_forest)
}

remove_unnecessary_columns <- function(data) {
  # Removing unnecessary columns
  data <- data %>%
    select(-c(password, MissOther), -starts_with("Big5"),
           -starts_with("Metainfo"), -starts_with("Location"),
           -c(StartDate, EndDate, Progress, Finished, ResponseId,
              DistributionChannel, UserLanguage, IPAddress, Status),
           -starts_with("preRelax"), -starts_with("postRelax"),
           -starts_with("prePOMS"), -starts_with("postPOMS"),
           -starts_with("preBlood"), -starts_with("postBlood"),
           -matches("p.*Pulse\\d"), -starts_with("submitted"),
           -(general_discomfort:burping),
           -(extraversion_sociability:openmindedness_creativeimagination),
           -ends_with("percentile"),
           -c(timeOnSpot, timeRelaxStart,timeRelaxEnd, sex_3_TEXT))
  return(data)
}

get_participants_without_both_sessions <- function(data) {
  ids <- data %>%
    group_by(ID) %>%
    count(ID) %>%
    filter(n < 2) %>%
    pull(ID)
  return(ids)
}

format_important_measures <- function(df_forest) {
  df_ros_poms <- df_forest %>%
    select(ID, condition, starts_with("relax"),
           starts_with("poms"), -ends_with("diff")) %>%
    pivot_longer(-c(ID, condition)) %>%
    separate_wider_delim(cols = "name", names = c("questionnaire", "what", "when"),
                         delim = "_") %>%
    mutate(when = factor(when, levels = c("pre", "post")),
           category = questionnaire) %>%
    select(-questionnaire)

  df_blood_analysis <- df_forest %>%
    select(ID, condition, starts_with("blood"), -ends_with("diff")) %>%
    pivot_longer(-c(ID, condition)) %>%
    separate_wider_delim(cols = "name", names = c("measure", "what", "when"),
                         delim = "_") %>%
    select(-measure) %>%
    mutate(when = factor(when, levels = c("pre", "post")),
           category = "Physiological")

  df_measures_original <- bind_rows(df_ros_poms, df_blood_analysis)
  return(df_measures_original)
}

scale_measures <- function(df_measures_original) {
  df_measures <- df_measures_original %>%
    group_by(what) %>%
    mutate(z_value = scale(value)[, 1]) %>%
    ungroup()
  return(df_measures)
}

filter_measures <- function(df_measures) {
  df_measures <- df_measures %>%
    group_by(what) %>%
    filter(abs(z_value) < 3) %>%
    ungroup()
  return(df_measures)
}

add_diff_measures <- function(df_measures) {
  out <- df_measures %>%
    pivot_wider(names_from = when, values_from = c(z_value, value),
                id_cols = c(ID, condition, what, category)) %>%
    mutate(z_value_diff = z_value_post - z_value_pre,
           value_diff = value_post - value_pre) %>%
    pivot_longer(cols = c(value_pre, value_post, value_diff),
                 names_to = "when", values_to = "value") %>%
    # rename columns starting with "z_" by removing the "z_" part
    rename_with(~str_remove(., "^z_"), starts_with("z_")) %>%
    pivot_longer(cols = c(value_pre, value_post, value_diff),
                 names_to = "when_diff", values_to = "z_value", ) %>%
    filter(when == when_diff) %>%
    select(-when_diff) %>%
    mutate(when = str_remove(when, "value_"))
  return(out)
}
