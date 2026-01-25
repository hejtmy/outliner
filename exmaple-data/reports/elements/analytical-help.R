library(effsize)
library(stringr)
library(dplyr)


create_post_table <- function(df_measures, df_forest, variable_name) {
  out <- df_measures %>%
    left_join(select(df_forest, ID, condition, variable = one_of(variable_name)),
              by = c("ID", "condition")) %>%
    filter(when == "post")
  return(out)
}

check_correlations <- function(df_measures, df_forest, variable_name) {
  df_cors <- create_post_table(df_measures, df_forest, variable_name) %>%
    group_by(what) %>%
    group_modify(~report_table(cor.test(.x$variable, .x$value, use="complete.obs")),
                 .keep = FALSE) %>%
    select(-c(Parameter1, Parameter2, Method, Alternative), -starts_with("d_CI"))
  return(df_cors)
}

plot_relationship <- function(df_measures, df_forest, variable_name) {
  create_post_table(df_measures, df_forest, variable_name) %>%
    ggplot(aes(variable, value)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(~what)
}

check_difference <- function(df_measures, df_forest, variable_name, type = "post") {
  df_diff <- create_post_table(df_measures, df_forest, variable_name) %>%
    group_by(what) %>%
    group_modify(~report_table(t.test(.x$value ~ .x$variable)), .keep = FALSE) %>%
    select(-c(Parameter, Group, Method, Alternative, CI), -contains("CI"))
  return(df_diff)
}

t_test_with_effect_table_paired <- function(one, second, digits = 3, CI = 0.95) {
  t <- t.test(one, second, paired = TRUE)
  d <- effsize::cohen.d(one, second, paired = TRUE, conf.level = CI, na.rm = TRUE)
  res <- data.frame(t.value = t$statistic,
                    df = t$parameter,
                    t.report = str_glue("{round(t$statistic, digits)} ({round(t$parameter, digits)})"),
                    p.value = apa_p(t$p.value),
                    cohen.d = d$estimate,
                    cohen.d.ci = paste0(round(d$conf.int, digits), collapse=", "),
                    stringsAsFactors = FALSE)
    return(res)
}

t_test_with_effect_table <- function(f, data, paired = FALSE, digits = 3, CI = 0.95){
  t <- t.test(formula = f, data = data, paired = paired)
  d <- effsize::cohen.d(f, data, paired = paired, conf.level = CI)
  agg_sd <- aggregate(f, data, sd)
  group_names <- extract_group_names(t)
  m_sd <- sapply(1:2, function(x) paste0(round(t$estimate[x], digits),
                                         "(", round(agg_sd[agg_sd[1] == group_names[x], 2], digits), ")"))
  res <- data.frame(group1.mean = m_sd[1], 
                    group2.mean = m_sd[2],
                    t.value = t$statistic,
                    df = t$parameter,
                    t.report = str_glue("{round(t$statistic, digits)} ({round(t$parameter, digits)})"),
                    p.value = apa_p(t$p.value),
                    cohen.d = d$estimate,
                    cohen.d.ci = paste0(round(d$conf.int, digits), collapse=", "),
                    stringsAsFactors = FALSE)
  colnames(res)[1:2] <- group_names
  return(res)
}

## extract names of group from t.test object
extract_group_names <- function(t) {
  group_names <- gsub("mean in group ", "", names(t$estimate))
  return(group_names)
}
