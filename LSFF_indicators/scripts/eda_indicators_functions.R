################################################################################
################### FUNCTIONS FOR "eda_indicators.R" SCRIPT ####################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 15-Feb-2024
# Last edited: 

# Script for creating functions required to perform exploratory analyses of the 
# LSFF indicators. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "wesanderson")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

stratified_plots <- function(svy_indicators) {
  
  # Firstly need to create a new data-frame to contain the values: 
  res <- (c("Urban", "Urban", "Urban", "Urban", "Urban",
            "Rural", "Rural", "Rural", "Rural", "Rural"))
  
  res_quintile <- (c("1 - Poorest", "2", "3", "4", "5 - Wealthiest",
                     "1 - Poorest", "2", "3", "4", "5 - Wealthiest"))
  
  df_indicators <- data.frame(res, res_quintile)
  
  urb_1 <- svy_indicators %>% 
    filter(res == "Urban", res_quintile == "1") %>% 
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "1 - Poorest")
  
  urb_2 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "2") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "2")
  
  urb_3 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "3") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "3")
  
  urb_4 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "4") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "4")
  
  urb_5 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "5") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "5 - Wealthiest")
  
  rur_1 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "1") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "1 - Poorest")
  
  rur_2 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "2") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "2")
  
  rur_3 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "3") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "3")
  
  rur_4 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "4") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "4")
  
  rur_5 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "5") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "5 - Wealthiest")
  
  # Append data from all socio-economic groups to create a single data-frame:
  urb_1[nrow(urb_1) + 1,] <- urb_2[1,]
  urb_1[nrow(urb_1) + 1,] <- urb_3[1,]
  urb_1[nrow(urb_1) + 1,] <- urb_4[1,]
  urb_1[nrow(urb_1) + 1,] <- urb_5[1,]
  urb_1[nrow(urb_1) + 1,] <- rur_1[1,]
  urb_1[nrow(urb_1) + 1,] <- rur_2[1,]
  urb_1[nrow(urb_1) + 1,] <- rur_3[1,]
  urb_1[nrow(urb_1) + 1,] <- rur_4[1,]
  urb_1[nrow(urb_1) + 1,] <- rur_5[1,]
  
  # Remove objects no longer required: 
  rm(list = c("rur_1", "rur_2", "rur_3", "rur_4", "rur_5", "urb_2", "urb_3", "urb_4", "urb_5"))
  
  # Put together analysis df:
  df_indicators <- df_indicators %>% dplyr::left_join(urb_1, by = c("res", "res_quintile"))
  
  # Convert all numbers in df_indicators to percentages by multiplying numeric columns by 100:
  df_indicators[, 3:ncol(df_indicators)] <- df_indicators[, 3:ncol(df_indicators)] * 100
  
  rm(urb_1)
  
  # Specify colours for urban/rural:
  point_colours <- wes_palette("AsteroidCity1", 2)
  point_colours <- setNames(point_colours, c("Rural", "Urban"))
  
  
  # Plot reach by population sub-group:
  reach_plot <<- ggplot(df_indicators, aes (x = res_quintile, y = reach)) +
    geom_point(aes(colour = res), size = 2.8) +
    geom_errorbar(aes(ymin = reach_low, ymax = reach_upp), width = 0.05) +
    ylab("Survey weighted reach (%) of potentially \n fortifiable staple grains (combined)") +
    xlab("Socio-economic quintile") +
    # Change colours using wesanderson package:
    scale_colour_manual(values = point_colours) +
    # remove legend title:
    guides(colour = guide_legend(title = NULL)) +
    # Position legend at bottom: 
    theme(legend.position = "bottom") +
    ylim(0,100) + theme_bw()
  # facet_wrap(~res, ncol = 1, scales = "free_x"
  
  # Plot dose3 by population sub-group:
  dose_plot <<- ggplot(df_indicators, aes (x = res_quintile, y = dose3)) +
    geom_point(aes(colour = res), size = 2.8) + 
    geom_errorbar(aes(ymin = dose3_low, ymax = dose3_upp), width = 0.05) +
    ylab("Survey weighted (%) of households consuming sufficient dose \n of potentially fortifiable staple grains to meet MN needs") +
    xlab("Socio-economic quintile") +
    # Change colours using wesanderson package:
    scale_colour_manual(values = point_colours) +
    # remove legend title:
    guides(colour = guide_legend(title = NULL)) +
    # Position legend at bottom: 
    theme(legend.position = "bottom") +
    ylim(0,100) + theme_bw()
  
}

