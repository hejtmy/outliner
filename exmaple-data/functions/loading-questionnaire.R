library(googlesheets4)
library(stringr)

source("functions/processing-questionnaires.R", encoding = "UTF-8")

load_paper_questionnaire_data <- function(name) {
  GS_SHEET <- "1Cp9mc7GGKKMGybn63QLDlZctYChlda-3rHSiVudxgl8"
  df_questionnaire <- read_sheet(GS_SHEET, sheet = name,
    skip = 1, na = c("", "NA"))
  return(df_questionnaire)
}

load_online_questionnaire_data <- function(questionnaire, ...) {
  GS_SHEET <- "1Cp9mc7GGKKMGybn63QLDlZctYChlda-3rHSiVudxgl8"
  df_questionnaire <- read_sheet(GS_SHEET, sheet = questionnaire,
    na = c("", "NA"), ...)
  return(df_questionnaire)
}
