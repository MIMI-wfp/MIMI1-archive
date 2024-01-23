################################################################################
################# FUNCTIONS FOR EXPLORATORY DATA ANALYSIS ######################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 16-Jan-2024
# Last edited: 19-Jan-2024

# This script is for creating functions required in the exploratory data analysis.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges",
                 "wesanderson")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# REACH: 

# Create a function that calculates the reach of each fortification vehicle, and
# plots this in a ggplot bar chart:

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
  
  edibleoil_reach <- svy_data %>%
    group_by(edible_oil) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(edible_oil, percentage)
  
  sugar_reach <- svy_data %>%
    group_by(sugar) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(sugar, percentage)
  
  salt_reach <- svy_data %>%
    group_by(salt) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(salt, percentage)
  
  # Create a data-frame for reach of each vehicle:
  vehicle <- c("Rice", "Wheat flour", "Maize flour", "Edible oil", "Sugar", "Salt")
  
  reach <- c(rice_reach[2,2], wheatflour_reach[2,2], maizeflour_reach[2,2], 
             edibleoil_reach[2,2], sugar_reach[2,2], salt_reach[2,2])
  
  reach <- as.double(reach)
  
  vehicle_reach <- data.frame(vehicle, reach)
  
  # Convert "vehicle" to an odered factor with custom levels: 
  vehicle_reach$vehicle <- factor(vehicle_reach$vehicle, 
                                  levels = c("Rice", "Wheat flour", "Maize flour", 
                                             "Edible oil", "Sugar", "Salt"))
  
  # Specify bar colours: 
  bar_colours <- wes_palette("AsteroidCity2", 6)
  bar_colours <- setNames(bar_colours, c("Salt", "Sugar", 
                                             "Edible oil", "Maize flour", 
                                             "Wheat flour", "Rice"))
  
  # Plot the reach of each vehicle (only including vehicles that are consumed):
  if (any(vehicle_reach$reach == 0 | is.na(vehicle_reach$reach))) {
    
    # Filter out vehicles that are not consumed:
    vehicle_reach <- vehicle_reach %>% 
      filter(reach != 0)
    
    # Now plot the reach of the remaining vehicles:
    ggplot(vehicle_reach, aes(x = vehicle, 
                              y = reach, 
                              fill = vehicle)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(reach, "%")), vjust = -0.5, size = 3) +
      labs(x = "", y = "Reach of fortification vehicle (%)") +
      scale_fill_manual(values = bar_colours) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "none")
    
  } else {
    
    # Otherwise plot reach as normal:
    return(ggplot(vehicle_reach, aes(x = vehicle, 
                                     y = reach, 
                                     fill = vehicle)) +
             geom_bar(stat = "identity") +
             geom_text(aes(label = paste0(reach, "%")), 
                       vjust = -0.5, size = 3) +
             labs(x = "", y = "Reach of fortification vehicle (%)") +
             scale_fill_manual(values = bar_colours) +
             theme_bw() +
             theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                   legend.position = "none"))
  }
}

#-------------------------------------------------------------------------------

# QUANTITIES CONSUMED: 

# Create a function that extracts quantity consumed of each vehicle, and presents
# this data in a density plot using ggridges:

plot_quantites <- function(vehicle_quantities) {
  long_data <- vehicle_quantities %>% select(hhid, rice_100g, wheatflour_100g,
                                             maizeflour_100g, edible_oil_100g,
                                             sugar_100g, salt_100g) %>%
    gather(key = "vehicle", value = "quantity", -hhid)
  
  # Mutate df so that quantity is in grams:
  long_data <- long_data %>% 
    mutate(quantity_g = quantity*100) %>% 
    dplyr::select(-quantity)
  
  long_data$vehicle <- recode(long_data$vehicle, 
                              "rice_100g" = "Rice", 
                              "wheatflour_100g" = "Wheat flour", 
                              "maizeflour_100g" = "Maize flour", 
                              "edible_oil_100g" = "Edible oil", 
                              "sugar_100g" = "Sugar", 
                              "salt_100g" = "Salt")
  
  # Convert "vehicle" to an odered factor with custom levels: 
  long_data$vehicle <- factor(long_data$vehicle, 
                                  levels = c("Salt", "Sugar", "Edible oil", 
                                             "Maize flour", "Wheat flour", "Rice"))
  
  # Ridge colours: 
  ridge_colours <- wes_palette("AsteroidCity2", 6)
  ridge_colours <- setNames(ridge_colours, c("Salt", "Sugar", 
                                             "Edible oil", "Maize flour", 
                                             "Wheat flour", "Rice"))
  
  # Create ridge plot (use an if-else statement to ensure that any vehicles
  # that are not consumed are removed from the plot):
  if (any(is.na(long_data$quantity_g))) {
    
    # Filter out any rows with NA values in quantity_g:
    long_data <- long_data %>% 
      filter(!is.na(quantity_g))
    
    # Create a ridge plot with the remaining data:
    ggplot(long_data, aes(x = quantity_g, y = vehicle, 
                          fill = vehicle)) +
      geom_density_ridges(alpha = 0.8, na.rm = TRUE) + 
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
             geom_density_ridges(alpha = 0.8, na.rm = TRUE) + 
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
                   include = c(urbrur, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(urbrur = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>n={n_unweighted}") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Rice**")
  
  
  # Stratified table for Wheat flour:
  tbl_wheatflour <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
    tbl_svysummary(by = wheatflour,
                   include = c(urbrur, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(urbrur = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>n={n_unweighted}") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Wheat Flour**")
  
  
  # Stratified table for Maize flour:
  tbl_maizeflour <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
    tbl_svysummary(by = maizeflour,
                   include = c(urbrur, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(urbrur = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>n={n_unweighted}") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Maize Flour**")
  
  
  # Stratified table for Edible oil:
  tbl_edible_oil <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>% 
    tbl_svysummary(by = edible_oil,
                   include = c(urbrur, sep_quintile),
                   percent = "row",
                   statistic = list(all_categorical() ~ "({p}%)"),
                   label = list(urbrur = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>%
    modify_header(label ~ "", all_stat_cols() ~ "{level}<br>n={n_unweighted}") %>% 
    modify_spanning_header(all_stat_cols() ~ "**Edible Oil**")
  
  
  # Overall: 
  tbl_overall <- survey::svydesign(id = ~ 1, weights = ~ survey_wgt, data = svy_data) %>%
    tbl_svysummary(include = c(urbrur, sep_quintile),
                   statistic = list(all_categorical() ~ "{n_unweighted}"),
                   label = list(urbrur = "Urban/Rural",
                                sep_quintile = "Socio-economic quintile")) %>% 
    modify_header(label ~ "", all_stat_cols() ~ "n={n_unweighted}")
  
  
  # Merge tables to produce a single table: 
  tbl_merge(tbls = list(tbl_rice, tbl_wheatflour, tbl_maizeflour, tbl_edible_oil,
                        tbl_overall),
            tab_spanner = c("**Rice**", "**Wheat Flour**", "**Maize Flour**", 
                            "**Edible Oil**", "**Overall**")) %>% 
    modify_footnote(all_stat_cols() ~ "Unweighted n (survey weighted %)")
  
  
}

################################################################################
################################## END OF SCRIPT ###############################
################################################################################
