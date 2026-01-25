library(dplyr)
library(repsy.bfi)
library(stringr)
library(lubridate)

process_ssq <- function(df_ssq) {
  colnames(df_ssq) <- c("ID", "general_discomfort", "fatigue",
                        "headache", "eyestrain", "difficulty_focusing",
                        "increased_salivation", "sweating", "nausea",
                        "difficulty_concentrating", "fullness_of_head",
                        "blurredvission", "dizzy_eyesopen", "dizzy_eyes_closed",
                        "vertigo", "stomach_awareness", "burping", "submitted")
  df_ssq <- df_ssq %>%
    mutate(across(.cols = everything(),
      ~stringr::str_trim(.x)))
  # Codes to upper
  df_ssq$ID <- toupper(df_ssq$ID)

  # Recodes to 1-4
  question_keys <- c("Vůbec" = 1, "Mírně" = 2, "Středně" = 3, "Silně" = 4)
  df_ssq <- df_ssq %>%
    mutate(across(-c(ID, submitted), ~recode(.x, !!!question_keys)))
  # calculates sum
  df_ssq <- df_ssq %>%
    rowwise() %>%
    mutate(ssq_sum = sum(across(-c(ID, submitted))),
           ssq_avg = ssq_sum/16) %>%
    ungroup()

  df_ssq <- df_ssq %>%
    mutate(
      ssq_nausea_total = general_discomfort +
        increased_salivation + sweating + nausea +
        difficulty_concentrating + burping + stomach_awareness,
      ssq_kim_oculomotor = general_discomfort + fatigue + eyestrain + difficulty_focusing,
      ssq_kim_disorientation = headache + fullness_of_head + blurredvission +
        dizzy_eyes_closed + vertigo,
    ssq_nausea = ssq_nausea_total/7)
  return(df_ssq)
}

# This replaces NA values
add_poms_scores <- function(df_data) {
  warning("replacing NA values in POMS with median values")
  ## TODO - fix this here
  # Count NAs before imputation
  n_na_before <- sum(is.na(df_data %>% select(contains("POMS"))))
  df_data <- df_data %>%
    mutate(across(contains("POMS"), ~replace_na(.x,  median(., na.rm = TRUE))))
  # Count NAs after imputation
  n_na_after <- sum(is.na(df_data %>% select(contains("POMS"))))
  n_imputed <- n_na_before - n_na_after
  message(glue::glue("Number of POMS values imputed: {n_imputed}"))
  poms_scales_czech <-
    list(anger = c(19, 2, 25, 21, 11, 31),
         vitality = c(32, 9, 5, 24, 35, 13),
         fatigue = c(29, 18, 37, 26, 3, 17),
         depression = c(4, 12, 33, 23, 14, 20, 15),
         confusion = c(36, 34, 10, 6),
         tension = c(22, 16, 1))
  poms_scales_names <- sapply(poms_scales_czech, function(x) paste0("POMS", x))
  for (name in names(poms_scales_names)){
    pre_vals <- paste0("pre", poms_scales_names[[name]])
    post_vals <- paste0("post", poms_scales_names[[name]])
    df_data <- df_data %>%
      rowwise()  %>%
      mutate(
        "POMS_{name}_pre" := sum(c_across(all_of(pre_vals)), na.rm = TRUE),
        "POMS_{name}_post" := sum(c_across(all_of(post_vals)), na.rm = TRUE),
        "POMS_{name}_diff" := get(str_glue("POMS_{name}_post")) -
          get(str_glue("POMS_{name}_pre"))) %>%
      ungroup()
  }
  return(df_data)
}

add_relax_scores <- function(df_data) {
  df_data <- df_data %>%
    rowwise() %>%
    mutate(relax_ros_pre = sum(across(contains("preRelax"))),
           relax_ros_post = sum(across(contains("postRelax"))),
           relax_ros_diff = relax_ros_post - relax_ros_pre) %>%
    ungroup()
  return(df_data)
}

process_quality <- function(df_quality) {
  colnames(df_quality) <-
    c("ID", "authenticity", "authenticity_visual", "authenticity_sound",
      "notes_authenticity_blocking", "notes_liking", "notes_missing",
      "notes_wouldchange", "submitted")
  question_keys <- c("Vůbec ne" = 1,  "Spíš ne" = 2, "Něco mezi" = 3,
                     "Spíš ano" = 4, "Velice" = 5)
  df_quality <- df_quality %>%
    mutate(authenticity = recode(authenticity, !!!question_keys),
           authenticity_visual = recode(authenticity_visual, !!!question_keys),
           authenticity_sound = recode(authenticity_sound, !!!question_keys),
           ID = toupper(ID))
  return(df_quality)
}

process_immersion <- function(df_data) {
  colnames(df_data) <- c("ID", "experience_comparability",
    "experience_reality", "experience_imagery", "experience_place",
    "experience_memory", "experience_presence", "submitted")
  df_data <- readr::type_convert(df_data)
  # Count NAs before imputation
  n_na_before <- sum(is.na(df_data %>% select(starts_with("experience"))))
  df_data <- df_data %>%
    mutate(across(starts_with("experience"),
                  ~replace_na(.x,  median(., na.rm = TRUE))),
           ID = toupper(ID)) %>%
    rowwise() %>%
    mutate(immersion_total = sum(c_across(starts_with("experience")))) %>%
    ungroup()
  # Count NAs after imputation
  n_na_after <- sum(is.na(df_data %>% select(starts_with("experience"))))
  n_imputed <- n_na_before - n_na_after
  message(glue::glue("Number of immersion values imputed: {n_imputed}"))
  return(df_data)
}

add_blood_pressure_scores <- function(df_data) {
  df_data <- df_data %>%
    mutate(blood_sys_pre = (preBloodSys2 + preBloodSys3)/2,
          blood_dia_pre = (preBloodDia3 + preBloodDia3)/2,
          blood_pulse_pre = (prePulse2 + prePulse3)/2,
          blood_sys_post = (postBloodSys2 + postBloodSys3)/2,
          blood_dia_post = (postBloodDia3 + postBloodDia3)/2,
          blood_pulse_post = (postPulse2 + postPulse3)/2,
          blood_sys_diff = blood_sys_post - blood_sys_pre,
          blood_dia_diff = blood_dia_post - blood_dia_pre,
          blood_pulse_diff = blood_pulse_post - blood_pulse_pre)
  return(df_data)
}

code_weather <- function(df_data) {
  # dummy code a weather which is in a string separatedby by commas
  # and then add it to the dataframe
  df_weather <-  df_data %>%
    select(.data$ID, .data$session, .data$preWeather) %>%
    separate_longer_delim(cols =  preWeather, delim = ",") %>%
    mutate(preWeather = str_trim(preWeather)) %>%
    pivot_wider(names_from = preWeather, values_from = preWeather,
                values_fn = ~as.logical(length(.x)), values_fill = FALSE,
                names_prefix = "weather_")
  df_data <- left_join(df_data, df_weather, by = c("ID", "session"))

  df_data <- df_data %>%
    mutate(humidity_avg = (preHumidity + postHumidity)/2,
           temperature_avg = (preTemperature + postTemperature)/2)
  return(df_data)
}
