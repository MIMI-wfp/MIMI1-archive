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

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "wesanderson",
                 "ggrepel")

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
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "1 - Poorest")
  
  urb_2 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "2") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "2")
  
  urb_3 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "3") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "3")
  
  urb_4 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "4") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "4")
  
  urb_5 <- svy_indicators %>%
    filter(res == "Urban", res_quintile == "5") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Urban", res_quintile = "5 - Wealthiest")
  
  rur_1 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "1") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "1 - Poorest")
  
  rur_2 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "2") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "2")
  
  rur_3 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "3") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "3")
  
  rur_4 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "4") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
    mutate(res = "Rural", res_quintile = "4")
  
  rur_5 <- svy_indicators %>%
    filter(res == "Rural", res_quintile == "5") %>%
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
              dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci")) %>% 
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
  df_indicators[, 3:ncol(df_indicators)] <- round((df_indicators[, 3:ncol(df_indicators)] * 100), digits = 1)
  
  rm(urb_1)
  
  # Specify colours for urban/rural:
  point_colours <- wes_palette("AsteroidCity2", 2)
  point_colours <- setNames(point_colours, c("Rural", "Urban"))
  
  
  # Plot reach by population sub-group:
  reach_plot <<- ggplot(df_indicators, aes (x = res_quintile, y = reach)) +
    geom_point(aes(colour = res), size = 2.8) +
    # geom_errorbar(aes(ymin = reach_low, ymax = reach_upp), width = 0.05) +
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
  dose_plot <<- ggplot(df_indicators, aes (x = res_quintile, y = dose4)) +
    geom_point(aes(colour = res), size = 2.8) + 
    geom_errorbar(aes(ymin = dose4_low, ymax = dose4_upp), width = 0.05) +
    ylab("Survey weighted (%) of households consuming sufficient dose \n of potentially fortifiable staple grains to meet MN needs") +
    xlab("Socio-economic quintile") +
    # Change colours using wesanderson package:
    scale_colour_manual(values = point_colours) +
    # remove legend title:
    guides(colour = guide_legend(title = NULL)) +
    # Position legend at bottom: 
    theme(legend.position = "bottom") +
    ylim(0,100) + theme_bw()
  
  # Create an additional plot to show both reach and dose3 on the same plot
  # (reach on the y-axis and increased bubble size for dose3): 
  reach_dose_plot <<- ggplot(df_indicators, aes (x = res_quintile, y = reach)) +
    geom_point(aes(colour = res, size = dose4), alpha = 0.85) +
    # geom_errorbar(aes(ymin = reach_low, ymax = reach_upp), width = 0.05) +
    geom_text(aes(label = dose4), size = 3, colour = "white") +
    ylab("Survey weighted reach (%) of potentially \n fortifiable staple grains (combined)") +
    xlab("Socio-economic quintile") +
    # Specify bubble sizes:
    scale_size_area(breaks = c(20, 40, 60, 80),
                    labels = c(20, 40, 60, 80),
                    max_size = 26,
                    limits = c(0,100)) +
    # Change colours using wesanderson package:
    scale_colour_manual(values = point_colours) +
    # remove legend title:
    guides(colour = guide_legend(title = NULL)) +
    # Edit legend: 
    guides(size = guide_legend(title = "Dose* %",
                               reverse = TRUE)) +
    ylim(0,100) + theme_bw() +
    theme(plot.caption = element_text(hjust = 0, 
                                      margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
}

# Now create an alternative function that can take multiple svy_indicators dataframes 
# and create a single plot for each indicator:

stratified_countries <- function(svy_nga_indicators, svy_eth_indicators) { 
  
  # Firstly need to create a new data frame to contain all the values: 
  sep_quintile <- c("1 - Poorest", "2", "3", "4", "5 - Wealthiest",
                    "1 - Poorest", "2", "3", "4", "5 - Wealthiest")
  
  country <- c("NGA", "NGA", "NGA", "NGA", "NGA",
               "ETH", "ETH", "ETH", "ETH", "ETH")
  
  reach_countries <- data.frame(sep_quintile, country)
  
  # Now calculate survey weighted indicators for each quintile in each country: 
  nga_
  
  }

