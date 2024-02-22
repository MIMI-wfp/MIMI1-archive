################################################################################
############### EXPLORATORY ANALYSES OF GENERALISABLE INDICATORS ###############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 08-Feb-2024
# Last edited: 16-Feb-2024

# This script is for performing exploratory analyses of the generalisable
# indicators: 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "wesanderson", 
                 "gridExtra")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

################################################################################

# NIGERIA:

# Load in required data: 

nga_lss1819_indicators <- read_csv("data_constrained/LSFF_indicators/data/nga_lss1819_indicators.csv")
nga_lss1819_food_consumption <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
nga_lss1819_vehicle_quantities <- read_csv("data_rich/fortification_models/data/nga_lss1819_vehicle_quantities.csv")
nga_lss1819_hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv")

#-------------------------------------------------------------------------------

# STAPLE GRAIN QUANTITIES:

# Explore quantity consumed of overall staple grain consumption:

nga_lss1819_vehicle_quantities %>% 
  ggplot(aes(x = staplegrain_100g)) +
  geom_histogram(bins = 20, color = "#e9ecef", fill = "#69b3a2", alpha = 0.8) + 
  scale_x_continuous(labels = function(x) paste0(x*100, "g")) +
  coord_cartesian(xlim = c(0, 6)) +
  labs(x = "Quantity consumed of all potentially fortifiable staple grains (grams per day, per AFE)",
       y = "Frequency") +
  # Add median line: 
  geom_vline(aes(xintercept = mean(staplegrain_100g, na.rm = T)), color = "red", linetype = "dashed")

# ggsave("data_constrained/LSFF_indicators/figures/staplegrains_hist.png", width = 6.83, height = 5.78,
#        dpi = 600)

#-------------------------------------------------------------------------------

# Additionally take a look at quantites consumed of roots and tubers:
nga_lss1819_food_consumption <- nga_lss1819_food_consumption %>% 
  dplyr::select("hhid", "food_group", "quantity_100g") %>% 
  filter(food_group == "Roots/tubers") %>% 
  # Sum quantities consumed of roots and tubers for each household: 
  group_by(hhid) %>%
  summarise(quantity_100g = sum(quantity_100g)) %>%
  ungroup()

# Now get these values per AFE, per day: 
nga_lss1819_food_consumption <- nga_lss1819_food_consumption %>% 
  left_join(nga_lss1819_hh_info %>% 
              dplyr::select("hhid", "afe"), by = "hhid") %>% 
  mutate(quantity_100g = (quantity_100g / afe) / 7)


# Now create a histogram, showing the distribution of quantities consumed of
# staple grains and roots/tubers combined: 
nga_lss1819_food_consumption %>% 
  dplyr::left_join(nga_lss1819_vehicle_quantities %>% 
                     dplyr::select("hhid", "staplegrain_100g"), by = "hhid") %>%
  ggplot(aes(x = staplegrain_100g + quantity_100g)) +
  geom_histogram(bins = 20, color = "#e9ecef", fill = "coral2", alpha = 0.8) +
  scale_x_continuous(labels = function(x) paste0(x*100, "g")) +
  coord_cartesian(xlim = c(0, 6)) +
  labs(x = "Quantity consumed of staple grains and roots/tubers combined (grams per day, per AFE)",
       y = "Frequency") +
  # Add median line:
  geom_vline(aes(xintercept = mean(staplegrain_100g + quantity_100g, na.rm = T)), 
             color = "blue", linetype = "dashed")

# ggsave("data_constrained/LSFF_indicators/figures/grains_roots_hist.png", width = 6.83, height = 5.78,
#        dpi = 600)

#-------------------------------------------------------------------------------

# DOSE VARIABLES: 

# Calculate survey weighted responses for each of the 3 dose variables.

# First attach survey weights to indicators: 
nga_lss1819_indicators <- nga_lss1819_indicators %>% 
  dplyr::left_join(nga_lss1819_hh_info %>% 
                     dplyr::select("hhid", "survey_wgt"), by = "hhid") %>% 
  # Transform base_adeq to base_inadeq: 
  mutate(base_inadeq = ifelse(base_adeq == 1, 0, 
                              ifelse(base_adeq == 0, 1, NA))) %>% 
  dplyr::select("hhid", "base_inadeq", "dose1", "dose2", "dose3", "staple_grain", "survey_wgt")
  

# Create tbl_svy object:
svy_nga_indicators <- nga_lss1819_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

# Now calculate survey weighted ratios/percentages for the 3 dose variables: 

svy_nga_indicators %>% 
  summarise(dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
            dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
            dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"))


table(nga_lss1819_indicators$dose1)
table(nga_lss1819_indicators$dose2)
table(nga_lss1819_indicators$dose3)

#-------------------------------------------------------------------------------

# Join population data to indicators data, ready for comparison with indicators 
# from other countries:

nga_indicators <- nga_lss1819_indicators %>% 
  dplyr::left_join(dplyr::select(nga_lss1819_hh_info, "hhid", "res", "sep_quintile",
                                 "res_quintile"), by = "hhid")

# Remove objects no longer required: 
rm(list = c("nga_lss1819_food_consumption", "nga_lss1819_hh_info",
            "nga_lss1819_indicators", "nga_lss1819_vehicle_quantities",
            "svy_nga_indicators"))

################################################################################

# ETHIOPIA: 

# Load in Ethiopia data:
eth_hices1516_indicators <- read_csv("data_constrained/LSFF_indicators/data/eth_hices1516_indicators.csv")
eth_hices1516_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")
eth_hices1516_vehicle_quantities <- read_csv("data_rich/fortification_models/data/eth_hices1516_vehicle_quantities.csv")
eth_hices1516_hh_info <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_hh_info.csv")

# Join popilation data to indicators data, ready for comparison with indicators
# from other countries:

eth_indicators <- eth_hices1516_hh_info %>% 
  dplyr::select("hhid", "res", "sep_quintile", "res_quintile", "survey_wgt") %>%
  # Only keep distinct household ID's
  dplyr::distinct(hhid, .keep_all = T) %>%
  dplyr::left_join(eth_hices1516_indicators, by = "hhid")
  

# Remove objects no longer required:
rm(list = c("eth_hices1516_food_consumption", "eth_hices1516_hh_info",
            "eth_hices1516_indicators", "eth_hices1516_vehicle_quantities"))

################################################################################

# EXPLORE INDICATORS DATA FROM ALL COUNTRIES:

# Transform base_adeq in Ethiopia to base_inadeq (as this will be used as the 
# denominator for the dose variables):
eth_indicators <- eth_indicators %>% 
  mutate(base_inadeq = ifelse(base_adeq == 1, 0, 
                              ifelse(base_adeq == 0, 1, NA))) %>% 
  dplyr::select("hhid", "base_inadeq", "dose1", "dose2", "dose3", "staple_grain", 
                "survey_wgt", "res", "sep_quintile", "res_quintile")

# Add a column to each of the data-frames called "ratio" which will be used for 
# ratio calculations:
nga_indicators$ratio <- 1
eth_indicators$ratio <- 1

# Create tbl_svy objects for each country's indicators data:
svy_nga_indicators <- nga_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

svy_eth_indicators <- eth_indicators %>%
  srvyr::as_survey_design(weights = survey_wgt)


# Source script required to create plots:
source("data_constrained/LSFF_indicators/scripts/eda_indicators_functions.R")

# Nigeria:
stratified_plots(svy_nga_indicators)
nga_reach_plot <- reach_plot + theme(legend.position = "none") + labs(title = "Nigeria")
nga_dose_plot <- dose_plot + theme(legend.position = "none") + labs(title = "Nigeria")
nga_reach_dose <- reach_dose_plot + theme(legend.position = "none") + 
  labs(caption = "Potentially fortifiable staple grains: Rice, Wheat flour, Maize flour",
       title = "Nigeria")

rm(list = c("reach_plot", "dose_plot", "reach_dose_plot"))


# Ethiopia:
stratified_plots(svy_eth_indicators)
eth_reach_plot <- reach_plot + labs(y = NULL) + labs(title = "Ethiopia")
eth_dose_plot <- dose_plot + labs(y = NULL) + labs(title = "Ethiopia")
eth_reach_dose <- reach_dose_plot + 
  labs(y = NULL,
       caption = "Potentially fortifiable staple grains: Rice, Wheat flour, Maize flour", 
       title = "Ethiopia")

rm(list = c("reach_plot", "dose_plot", "reach_dose_plot"))

# Combine reach plots from multiple countries: 
grid.arrange(nga_reach_plot, eth_reach_plot, ncol = 2)

# Save from viewer pane.

# Combine dose plots from multiple countries:
grid.arrange(nga_dose_plot, eth_dose_plot, ncol = 2)

# combine reach and dose plots from multiple countries:
grid.arrange(nga_reach_dose, eth_reach_dose, ncol = 2, 
             bottom = "* Percentage of households consuming sufficient quantity (dose) of potentially fortifiable staple grains to meet MN needs (survey weighted)")

rm(list = c("nga_reach_plot", "eth_reach_plot", "nga_dose_plot", "eth_dose_plot"))




################################################################################
############################## END OF SCRIPT ###################################
################################################################################


