library(googlesheets4)
library(tidyverse)
library(repsy.bfi)

GS_SHEET <- "1Cp9mc7GGKKMGybn63QLDlZctYChlda-3rHSiVudxgl8"

df_bfi <- read_sheet(GS_SHEET, sheet = "Qualtrics", col_types = "c",
    na = c("", "NA"))

df_session1 <- read_sheet(GS_SHEET, sheet = "Session1", col_types = "c",
    na = c("", "NA"), skip = 1)

save(df_bfi, file = "df_bfi.RData")
load("df_bfi.RData")

df_bfi <- df_bfi[-1, ]
df_bfi <- readr::type_convert(df_bfi, na = c("", "NA"))
df_bfi <- process_bfi(df_bfi, "Big5", "60")
df_bfi <- mutate(df_bfi, gender = recode(sex, `1` = "male", `2` = "female"))
#recode columnn gender in the tale df_bfi so that 2 is female and 1 is male
df_bfi <- standardize_bfi(df_bfi, "CZ60z")
df_bfi %>%
    select(ends_with("percentile")) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(value, fill = name)) +
        geom_histogram(binwidth = 0.05) +
        facet_wrap(~name)

create_profile <- function(data, participant_code){
    sidestem <-  data %>%
        filter(toupper(ID) == participant_code) %>%
        select(ends_with("percentile"))
    out <- convert_to_profile(sidestem)
    return(out)
}

convert_to_profile <- function(df_table){
    sidestem <- df_table %>%
        pivot_longer(everything()) %>%
        separate(name, into = c("trait", "valuetype"), sep = "_") %>%
        mutate(value = round(value * 100, 0))

    trait.names <- unique(sidestem$trait)
    res <- vector("list", length(trait.names))
    names(res) <- trait.names

    for(i in 1:nrow(sidestem)){
        dimension <- as.character(sidestem[i, "trait"])
        valuetype <- as.character(sidestem[i, "valuetype"])
        res[[dimension]][[valuetype]] <- as.numeric(sidestem[i, "value"])
    }
    return(list(profile = res))
}

ids <- df_session1$ID
df_output <- data.frame(id = df_session1$ID, password = df_session1$password)
df_output$output_file <- NA
for(id in ids){
    res <- create_profile(df_bfi, id)
    res$code <- id
    output_file <- stringr::str_glue("report/{id}.html")
    df_output$output_file[toupper(df_output$id) == toupper(id)] <- output_file
    quarto::quarto_render(input = "report/individual.qmd",
                output_file = output_file,
                execute_params = res)
}

# zip the reports
for (i in seq_len(nrow(df_output))) {
    line <- df_output[i, ]
    if (is.na(line$output_file)) next()
    print(getwd())
    zip(zipfile = str_glue("zips/personality-{line$id}"),
        files = line$output_file,
        flags = paste("--password", line$password))
}
