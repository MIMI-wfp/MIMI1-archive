##### survey weights calculation

library(srvyr)

# Kevin's code ------------------------------------------------------------------
w_data <- data %>% srvyr::as_survey_design(id = cluster, strata =NULL, weights = weight, nest=T)

w_data %>%
  srvyr::group_by(sep_cat) %>%
  srvyr::summarise(
    vasupp = (survey_mean(vas == "yes", proportion = TRUE, vartype = "ci",na.rm = T)) * 100)