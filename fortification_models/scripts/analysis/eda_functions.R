################################################################################
################# FUNCTIONS FOR EXPLORATORY DATA ANALYSIS ######################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 16-Jan-2024
# Last edited: 30-Jan-2024

# This script is for creating functions required in the exploratory data analysis.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges",
                 "wesanderson", "purrr", "janitor")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# REACH: 

# Create a function that calculates the reach of each fortification vehicle, and
# present this information using a ggdotchart:

plot_reach <- function(svy_data) {
  
  # Calculate survey weighted reach for each vehicle: 
  rice_reach <- svy_data %>% 
    group_by(rice) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(rice, percentage)
  
  wheatflour_reach <- svy_data %>% 
    group_by(wheatflour) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(wheatflour, percentage)
  
  maizeflour_reach <- svy_data %>%
    group_by(maizeflour) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(maizeflour, percentage)
  
  staplegrain_reach <- svy_data %>%
    group_by(staple_grain) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  # For now I'm focussing on staple grains only, therefore commenting out other
  # vehicles: 
  
  # edibleoil_reach <- svy_data %>%
  #   group_by(edible_oil) %>% 
  #   summarise(n = survey_total()) %>%
  #   mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  #   dplyr::select(edible_oil, percentage)
  # 
  # sugar_reach <- svy_data %>%
  #   group_by(sugar) %>% 
  #   summarise(n = survey_total()) %>%
  #   mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  #   dplyr::select(sugar, percentage)
  # 
  # salt_reach <- svy_data %>%
  #   group_by(salt) %>% 
  #   summarise(n = survey_total()) %>%
  #   mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  #   dplyr::select(salt, percentage)
  
  # Create a data-frame for reach of each vehicle:
  vehicle <- c("Rice", "Wheat flour", "Maize flour", "Potentially \nfortifiable \nstaple grains \n(combined)")
  
  reach <- c(rice_reach[2,2], wheatflour_reach[2,2], maizeflour_reach[2,2], 
             staplegrain_reach[2,2])
  
  reach <- as.double(reach)
  
  vehicle_reach <- data.frame(vehicle, reach)
  
  # Convert "vehicle" to an odered factor with custom levels: 
  vehicle_reach$vehicle <- factor(vehicle_reach$vehicle, 
                                  levels = c("Rice", "Wheat flour", "Maize flour", 
                                             "Potentially \nfortifiable \nstaple grains \n(combined)"))
  
  # Specify bar colours: 
  bar_colours <- wes_palette("AsteroidCity1", 4)
  bar_colours <- setNames(bar_colours, c("Potentially \nfortifiable \nstaple grains \n(combined)", 
                                         "Maize flour", "Wheat flour", "Rice"))
  
  # Plot the reach of each vehicle (only including vehicles that are consumed):
  if (any(vehicle_reach$reach == 0 | is.na(vehicle_reach$reach))) {
    
    # Filter out vehicles that are not consumed:
    vehicle_reach <- vehicle_reach %>% 
      filter(reach != 0)
    
    # Instead of bar plot, use ggdotchart: 
    ggpar(ggdotchart(vehicle_reach, x = "vehicle", y = "reach", 
                     color = "vehicle", 
                     x.text.col = FALSE,
                     palette = bar_colours, 
                     add = "segments", 
                     sorting = "none",
                     rotate = TRUE,
                     position = position_dodge(0.5), 
                     dot.size = 16, 
                     label = paste0(vehicle_reach$reach, "%"),
                     font.label = list(size = 11, color = "white", vjust = 0.5),
                     ylab = "Reach of Fortification Vehicle (%)",
                     xlab = "",
                     ggtheme = theme_pubr()),
          legend = "none")
    
  } else {
    
    # Otherwise plot reach as normal:
    return(
      
      ggpar(ggdotchart(vehicle_reach, x = "vehicle", y = "reach", 
                       color = "vehicle", 
                       x.text.col = FALSE,
                       palette = bar_colours, 
                       add = "segments", 
                       sorting = "none",
                       rotate = TRUE,
                       position = position_dodge(0.5), 
                       dot.size = 16, 
                       label = paste0(vehicle_reach$reach, "%"),
                       font.label = list(size = 11, color = "white", vjust = 0.5),
                       ylab = "Reach of Fortification Vehicle (%)",
                       xlab = "",
                       ggtheme = theme_pubr()),
            legend = "none")
    
    )
  }
}

#-------------------------------------------------------------------------------


# QUANTITIES CONSUMED: 

# Create a function that extracts quantity consumed of each vehicle, and presents
# this data in a density plot using ggridges:

plot_quantites <- function(vehicle_quantities) {
  long_data <- vehicle_quantities %>% select(hhid, rice_100g, wheatflour_100g,
                                             maizeflour_100g, staplegrain_100g) %>% 
                                             # edible_oil_100g,
                                             # sugar_100g, salt_100g) %>%
    gather(key = "vehicle", value = "quantity", -hhid)
  
  # Mutate df so that quantity is in grams:
  long_data <- long_data %>% 
    mutate(quantity_g = quantity*100) %>% 
    dplyr::select(-quantity)
  
  long_data$vehicle <- recode(long_data$vehicle, 
                              "rice_100g" = "Rice", 
                              "wheatflour_100g" = "Wheat flour", 
                              "maizeflour_100g" = "Maize flour", 
                              "staplegrain_100g" = "Potentially \nfortifiable \nstaple grains \n(combined)")
                              # Focus is currently on staple grains only, 
                              # therefore comment out other vehicles.
                              # "edible_oil_100g" = "Edible oil", 
                              # "sugar_100g" = "Sugar", 
                              # "salt_100g" = "Salt")
  
  # Convert "vehicle" to an odered factor with custom levels: 
  long_data$vehicle <- factor(long_data$vehicle, 
                                  levels = c("Potentially \nfortifiable \nstaple grains \n(combined)", 
                                             "Maize flour", "Wheat flour", "Rice"))
  
  # Ridge colours: 
  ridge_colours <- wes_palette("AsteroidCity1", 4)
  ridge_colours <- setNames(ridge_colours, c("Potentially \nfortifiable \nstaple grains \n(combined)", 
                                             "Maize flour", "Wheat flour", 
                                             "Rice"))
  
  # Create ridge plot (use an if-else statement to ensure that any vehicles
  # that are not consumed are removed from the plot):
  if (any(is.na(long_data$quantity_g))) {
    
    # Filter out any rows with NA values in quantity_g:
    long_data <- long_data %>% 
      filter(!is.na(quantity_g))
    
    # Create a ridge plot with the remaining data:
    ggplot(long_data, aes(x = quantity_g, y = vehicle, 
                          fill = vehicle)) +
      geom_density_ridges(alpha = 0.8, na.rm = TRUE, scale = 0.9,
                          quantile_lines = T, quantiles = 2) + 
      theme_ridges() + 
      theme(legend.position = "none") + 
      scale_fill_manual(values = ridge_colours) +
      labs(x = "Quantity consumed per day, per AFE (grams)", 
           y = "Density") +
      theme(axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5)) 
  } else {
    # Otherwise just create the ridge-plot as normal:
    return(ggplot(long_data, aes(x = quantity_g, y = vehicle, 
                                 fill = vehicle)) +
             geom_density_ridges(alpha = 0.8, na.rm = TRUE, scale = 0.9,
                                 quantile_lines = T, quantiles = 2) + 
             theme_ridges() + 
             theme(legend.position = "none") + 
             scale_fill_manual(values = ridge_colours) +
             labs(x = "Quantity consumed per day, per AFE (grams)", 
                  y = "Density") +
             theme(axis.title.x = element_text(hjust = 0.5),
                   axis.title.y = element_text(hjust = 0.5)))
   }
  
}

#-------------------------------------------------------------------------------

# REACH STRATIFIED BY URBAN/RURAL AND SEP QUINTILE:

# Create a function that produces a summary table of reach stratified by urban/
# rural and SEP quintile:

# ***** NEED TO PROVIDE UPDATE TO FUNCTION BELOW SO THAT IT CAN HANDLE DATA WHERE 
# A FORTIFICATION VEHICLE IS NOT CONSUMED IN A PARTICULAR COUNTRY *****

stratified_table <- function(svy_data) {
  
  
  # Firstly handle missing data so that it is displayed correctly:
  svy_data <- svy_data %>% 
    dplyr::mutate(sep_quintile = factor(sep_quintile) %>% 
                    forcats::fct_na_value_to_level("Missing data"))
  
  # Stratified table for Rice:
  tbl_rice <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
    tbl_svysummary(by = rice,
                   include = c(res, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(res = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>{p}%") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Rice**")
  
  
  # Stratified table for Wheat flour:
  tbl_wheatflour <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
    tbl_svysummary(by = wheatflour,
                   include = c(res, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(res = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>{p}%") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Wheat Flour**")
  
  
  # Stratified table for Maize flour:
  # tbl_maizeflour <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>%
  #   tbl_svysummary(by = maizeflour,
  #                  include = c(res, sep_quintile),
  #                  percent = "row",
  #                  statistic = list(all_categorical() ~ "({p}%)"),
  #                  label = list(res = "Urban/Rural",
  #                               sep_quintile = "Socio-economic quintile")) %>%
  #   modify_header(label ~ "", all_stat_cols() ~ "{level}<br>{p}%") %>%
  #   modify_spanning_header(all_stat_cols() ~ "**Maize Flour**")
  
  
  # # Stratified table for Edible oil:
  # tbl_edible_oil <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
  #   tbl_svysummary(by = edible_oil,
  #                  include = c(res, sep_quintile),
  #                  percent = "row",
  #                  statistic = list(all_categorical() ~ "({p}%)"),
  #                  label = list(res = "Urban/Rural",
  #                               sep_quintile = "Socio-economic quintile")) %>%
  #   modify_header(label ~ "", all_stat_cols() ~ "{level}<br>{p}%") %>% 
  #   modify_spanning_header(all_stat_cols() ~ "**Edible Oil**")
  
  
  # Stratified for all staple grains: 
  tbl_staple_grains <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>%
    tbl_svysummary(by = staple_grain,
                   include = c(res, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(res = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>{p}%") %>%
    modify_spanning_header(all_stat_cols() ~ "**Potentially fortifiable<br>staple grains<br>(combined)**")
  
  # Overall: 
  tbl_overall <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>%
    tbl_svysummary(include = c(res, sep_quintile),
                   statistic = list(all_categorical() ~ "{n_unweighted}"),
                   label = list(res = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>% 
    modify_header(label ~ "", all_stat_cols() ~ "n={n_unweighted}")
  
  
  # Merge tables to produce a single table: 
  tbl_merge(tbls = list(tbl_rice, tbl_wheatflour, 
                        # tbl_maizeflour,
                        tbl_staple_grains,
                        # tbl_edible_oil,
                        tbl_overall),
            tab_spanner = c("**Rice**", "**Wheat Flour**", 
                            # "**Maize Flour**",
                            "**All Staple Grains**",
                            # "**Edible Oil**", 
                            "**Overall**")) %>% 
    modify_footnote(all_stat_cols() ~ "Unweighted n (survey weighted %)")
  
  
}

#-------------------------------------------------------------------------------

# REACH STRATIFIED BY SUBPOPULATIONS:

stratified_plot <- function(svy_data) {
  
  # Firstly need to create a new data-frame to contain the values: 
  res <- (c("Urban", "Urban", "Urban", "Urban", "Urban",
            "Rural", "Rural", "Rural", "Rural", "Rural"))
  
  res_quintile <- (c("1 - Poorest", "2", "3", "4", "5 - Wealthiest",
                     "1 - Poorest", "2", "3", "4", "5 - Wealthiest"))
  
  df_reach <- data.frame(res, res_quintile)
  
  # Add a new column to df_dumbell containing survey weighted reach of overall
  # staple grain consumption: 
  urb_1 <- svy_data %>% 
    filter(res == "Urban", res_quintile == "1") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_2 <- svy_data %>%
    filter(res == "Urban", res_quintile == "2") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_3 <- svy_data %>%
    filter(res == "Urban", res_quintile == "3") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_4 <- svy_data %>%
    filter(res == "Urban", res_quintile == "4") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_5 <- svy_data %>%
    filter(res == "Urban", res_quintile == "5") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_1 <- svy_data %>%
    filter(res == "Rural", res_quintile == "1") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_2 <- svy_data %>%
    filter(res == "Rural", res_quintile == "2") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_3 <- svy_data %>%
    filter(res == "Rural", res_quintile == "3") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_4 <- svy_data %>%
    filter(res == "Rural", res_quintile == "4") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>%
    dplyr::select(staple_grain, percentage)
  
  rur_5 <- svy_data %>%
    filter(res == "Rural", res_quintile == "5") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>%
    dplyr::select(staple_grain, percentage)
  
  df_reach$reach <- c(urb_1[2,2], urb_2[2,2], urb_3[2,2], urb_4[2,2], urb_5[2,2],
                         rur_1[2,2], rur_2[2,2], rur_3[2,2], rur_4[2,2], rur_5[2,2])
  
  rm(list = c("urb_1", "urb_2", "urb_3", "urb_4", "urb_5", 
              "rur_1", "rur_2", "rur_3", "rur_4", "rur_5"))
  
  # Use df_reach to create dumbell plot, with res_quintile on the y-axis, and
  # reach on the x-axis, using the res column to colour the dumbells by urban/rural:
  
  df_reach$reach <- as.numeric(df_reach$reach)
  
  # ggplot(df_reach, aes (x = reach, y = res_quintile)) +
  #   geom_line() +
  #   geom_point(aes(colour = res), size = 3) + 
  #   xlab("Survey weighted reach (%) of staple grains (combined)") +
  #   ylab("Socio-economic quintile") +
  #   # Change colours using wesanderson package:
  #   scale_colour_manual(values = wes_palette("AsteroidCity2", 2, type = "discrete")) +
  #   # remove legend title:
  #   guides(colour = guide_legend(title = NULL)) +
  #   # Position legend at bottom: 
  #   theme(legend.position = "bottom") +
  #   xlim(0,100) + theme_bw()

  # Commented out code above is for a dumbell plot, however I've decided to go 
  # with geom_point as below. I could return to dumbell plots when putting 
  # together figures for manuscript.
  
  # Point colours
  point_colours <- wes_palette("AsteroidCity1", 2)
  point_colours <- setNames(point_colours, c("Rural", "Urban"))


  ggplot(df_reach, aes (x = res_quintile, y = reach)) +
    geom_point(aes(colour = res), size = 5) + 
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
}


#-------------------------------------------------------------------------------

# Stratified quantities: 

# Create ridge plots as with the "plot_quantities" function, but this time produce
# ridges that are stratified by the "res" variables (urban/rural).

stratified_quantities <- function(vehicle_quantities) {
  
  long_data <- vehicle_quantities %>% select(hhid, rice_100g, wheatflour_100g,
                                  maizeflour_100g, staplegrain_100g,
                                  res) %>% 
    # Focus is currently on staple grains only, therefore commenting out other
    # vehicles:
    # edible_oil_100g,
    # sugar_100g, salt_100g) %>%
    gather(key = "vehicle", value = "quantity", -hhid, -res) %>% 
    # Mutate df so that quantity is in grams:
    mutate(quantity_g = quantity*100) %>% 
    dplyr::select(-quantity)
  
  long_data$vehicle <- recode(long_data$vehicle, 
                              "rice_100g" = "Rice", 
                              "wheatflour_100g" = "Wheat flour", 
                              "maizeflour_100g" = "Maize flour", 
                              "staplegrain_100g" = "Potentially \nfortifiable \nstaple grains \n(combined)")
  # "edible_oil_100g" = "Edible oil", 
  # "sugar_100g" = "Sugar", 
  # "salt_100g" = "Salt")
  
  # Convert "vehicle" to an odered factor with custom levels: 
  long_data$vehicle <- factor(long_data$vehicle, 
                              levels = c("Potentially \nfortifiable \nstaple grains \n(combined)", 
                                         "Maize flour", "Wheat flour", "Rice"))
  
  # Convert "res" to an odered factor with custom levels:
  long_data$res <- factor(long_data$res, 
                          levels = c("Rural", "Urban"))
  
  # Ridge colours: 
  ridge_colours <- wes_palette("AsteroidCity1", 2)
  ridge_colours <- setNames(ridge_colours, c("Rural", "Urban"))
  
  # Create ridge plot (use an if-else statement to ensure that any vehicles
  # that are not consumed are removed from the plot):
  if (any(is.na(long_data$quantity_g))) {
    
    # Filter out any rows with NA values in quantity_g:
    long_data <- long_data %>% 
      filter(!is.na(quantity_g))
    
    # Create a ridge plot  with the remaining data:
    ggplot(long_data, aes(x = quantity_g, y = vehicle, 
                          fill = res)) +
      geom_density_ridges(alpha = 0.8, na.rm = TRUE, scale = 0.9,
                          quantile_lines = T, quantiles = 2) + 
      theme_ridges() + 
      guides(fill = guide_legend(title = NULL, 
                                 override.aes = list(color = NA))) +
      scale_fill_manual(values = ridge_colours) +
      labs(x = expression(paste("Quantity consumed per day, per AFE (grams)"^"*")), 
           y = "Density",
           caption = "*among consumers only") +
      theme(axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5)) 
  } else {
    # Otherwise just create the ridge-plot as normal:
    return (# Create a ridge plot  with the remaining data:
      ggplot(long_data, aes(x = quantity_g, y = vehicle, 
                            fill = res)) +
        geom_density_ridges(alpha = 0.8, na.rm = TRUE, scale = 0.9,
                            quantile_lines = T, quantiles = 2) + 
        theme_ridges() + 
        guides(fill = guide_legend(title = NULL, 
                                   override.aes = list(color = NA))) +
        scale_fill_manual(values = ridge_colours) +
        labs(x = expression(paste("Quantity consumed per day, per AFE (grams)"^"*")), 
             y = "Density",
             caption = "*among consumers only") +
        theme(axis.title.x = element_text(hjust = 0.5),
              axis.title.y = element_text(hjust = 0.5))) 
  }
  
}

#-------------------------------------------------------------------------------

# Create csv files for reach data (stratified): 

stratified_csv <- function(svy_data) {
  
  # Firstly need to create a new data-frame to contain the values: 
  res <- (c("Urban", "Urban", "Urban", "Urban", "Urban",
            "Rural", "Rural", "Rural", "Rural", "Rural"))
  
  res_quintile <- (c("1 - Poorest", "2", "3", "4", "5 - Wealthiest",
                     "1 - Poorest", "2", "3", "4", "5 - Wealthiest"))
  
  df_reach <- data.frame(res, res_quintile)
  
  # Add a new column to df_dumbell containing survey weighted reach of overall
  # staple grain consumption: 
  urb_1 <- svy_data %>% 
    filter(res == "Urban", res_quintile == "1") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_2 <- svy_data %>%
    filter(res == "Urban", res_quintile == "2") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_3 <- svy_data %>%
    filter(res == "Urban", res_quintile == "3") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_4 <- svy_data %>%
    filter(res == "Urban", res_quintile == "4") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  urb_5 <- svy_data %>%
    filter(res == "Urban", res_quintile == "5") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_1 <- svy_data %>%
    filter(res == "Rural", res_quintile == "1") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_2 <- svy_data %>%
    filter(res == "Rural", res_quintile == "2") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_3 <- svy_data %>%
    filter(res == "Rural", res_quintile == "3") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(staple_grain, percentage)
  
  rur_4 <- svy_data %>%
    filter(res == "Rural", res_quintile == "4") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>%
    dplyr::select(staple_grain, percentage)
  
  rur_5 <- svy_data %>%
    filter(res == "Rural", res_quintile == "5") %>% 
    group_by(staple_grain) %>%
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>%
    dplyr::select(staple_grain, percentage)
  
  df_reach$reach <- c(urb_1[2,2], urb_2[2,2], urb_3[2,2], urb_4[2,2], urb_5[2,2],
                      rur_1[2,2], rur_2[2,2], rur_3[2,2], rur_4[2,2], rur_5[2,2])
  
  df_reach <<- df_reach
}

################################################################################
################################## END OF SCRIPT ###############################
################################################################################
