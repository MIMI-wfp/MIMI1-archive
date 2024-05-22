################################################################################
############### EXPLORATORY ANALYSES OF GENERALISABLE INDICATORS ###############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 08-Feb-2024
# Last edited: 26-Feb-2024

# This script is for performing exploratory analyses of the generalisable
# indicators: 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "wesanderson", 
                 "gridExtra", "gt", "gtsummary", "plotrix", "ggsci")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

################################################################################

# READ IN REQUIRED DATA:

# NIGERIA:

nga_lss1819_indicators <- read_csv("data_constrained/LSFF_indicators/data/nga_lss1819_indicators.csv")
nga_lss1819_food_consumption <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
nga_lss1819_vehicle_quantities <- read_csv("data_rich/fortification_models/data/nga_lss1819_vehicle_quantities.csv")
nga_lss1819_hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv")

# Store all Nigeria data in a list to keep environment tidy:
nigeria_all_data <- list(nga_lss1819_indicators, nga_lss1819_food_consumption, 
                         nga_lss1819_vehicle_quantities, nga_lss1819_hh_info)

# Name each data-frame in the list:
names(nigeria_all_data) <- c("nga_indicators", "nga_food_consumption", 
                             "nga_vehicle_quantities", "nga_hh_info")

rm(list = c("nga_lss1819_indicators", "nga_lss1819_food_consumption", 
            "nga_lss1819_vehicle_quantities", "nga_lss1819_hh_info"))

# ETHIOPIA: 
eth_hices1516_indicators <- read_csv("data_constrained/LSFF_indicators/data/eth_hices1516_indicators.csv")
eth_hices1516_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")
eth_hices1516_vehicle_quantities <- read_csv("data_rich/fortification_models/data/eth_hices1516_vehicle_quantities.csv")
eth_hices1516_hh_info <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_hh_info.csv")

# Store all Ethiopia data in a list to keep environment tidy:
ethiopia_all_data <- list(eth_hices1516_indicators, eth_hices1516_food_consumption, 
                          eth_hices1516_vehicle_quantities, eth_hices1516_hh_info)

names(ethiopia_all_data) <- c("eth_indicators", "eth_food_consumption", 
                              "eth_vehicle_quantities", "eth_hh_info")

rm(list = c("eth_hices1516_indicators", "eth_hices1516_food_consumption", 
            "eth_hices1516_vehicle_quantities", "eth_hices1516_hh_info"))

# INDIA: 
# Note that indicators data for India is not ready yet - TO ADD IN LATER
ind_nss1112_food_consumption <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_food_consumption.csv")
ind_nss1112_vehicle_quantities <- read_csv("data_rich/fortification_models/data/ind_nss1112_vehicle_quantities.csv")
ind_nss1112_hh_info <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_hh_info.csv")

# Store all India data in a list to keep environment tidy:
india_all_data <- list(ind_nss1112_food_consumption, ind_nss1112_vehicle_quantities, 
                       ind_nss1112_hh_info)

names(india_all_data) <- c("ind_food_consumption", "ind_vehicle_quantities", 
                           "ind_hh_info")

rm(list = c("ind_nss1112_food_consumption", "ind_nss1112_vehicle_quantities", 
            "ind_nss1112_hh_info"))

################################################################################

# EXPLORATION OF STAPLE GRAIN QUANTITIES:

# Explore quantity consumed of overall staple grain consumption - Nigeria example:

nigeria_all_data$nga_vehicle_quantities %>% 
  ggplot(aes(x = staplegrain_100g)) +
  geom_histogram(bins = 20, color = "#e9ecef", fill = "#69b3a2", alpha = 0.8) + 
  scale_x_continuous(labels = function(x) paste0(x*100, "g")) +
  coord_cartesian(xlim = c(0, 6)) +
  labs(x = "Quantity consumed of all potentially fortifiable staple grains (grams per day, per AFE)",
       y = "Frequency") +
  # Add median line: 
  geom_vline(aes(xintercept = mean(staplegrain_100g, na.rm = T)), color = "red", linetype = "dashed")

# ggsave("data_constrained/LSFF_indicators/figures/nga_staplegrains_hist.png", width = 6.83, height = 5.78,
#        dpi = 600)

# Now perform a sense check by additionally looking at the quantity consumed of 
# staple grains, roots and tubers (all combined):

# Additionally take a look at quantites consumed of roots and tubers:
nigeria_all_data$nga_food_consumption <- nigeria_all_data$nga_food_consumption %>% 
  dplyr::select("hhid", "food_group", "quantity_100g") %>% 
  filter(food_group == "Roots/tubers") %>% 
  # Sum quantities consumed of roots and tubers for each household: 
  group_by(hhid) %>%
  summarise(quantity_100g = sum(quantity_100g)) %>%
  ungroup()

# Now get these values per AFE, per day: 
nigeria_all_data$nga_food_consumption <- nigeria_all_data$nga_food_consumption %>% 
  left_join(nigeria_all_data$nga_hh_info %>% 
              dplyr::select("hhid", "afe"), by = "hhid") %>% 
  mutate(quantity_100g = (quantity_100g / afe) / 7)


# Now create a histogram, showing the distribution of quantities consumed of
# staple grains and roots/tubers combined: 
nigeria_all_data$nga_food_consumption %>% 
  dplyr::left_join(nigeria_all_data$nga_vehicle_quantities %>% 
                     dplyr::select("hhid", "staplegrain_100g"), by = "hhid") %>%
  ggplot(aes(x = staplegrain_100g + quantity_100g)) +
  geom_histogram(bins = 20, color = "#e9ecef", fill = "coral2", alpha = 0.8) +
  scale_x_continuous(labels = function(x) paste0(x*100, "g")) +
  coord_cartesian(xlim = c(0, 6)) +
  labs(x = "Quantity consumed of staple grains, roots and tubers combined (grams per day, per AFE)",
       y = "Frequency") +
  # Add median line:
  geom_vline(aes(xintercept = mean(staplegrain_100g + quantity_100g, na.rm = T)), 
             color = "blue", linetype = "dashed")

# ggsave("data_constrained/LSFF_indicators/figures/nga_grains_roots_hist.png", width = 6.83, height = 5.78,
#        dpi = 600)

#-------------------------------------------------------------------------------

# STACKED HISTOGRAMS:

# NIGERIA

# Need to create a new variable to indicate which combination of staple grains 
# each household is consuming: 
nigeria_all_data$nga_vehicle_quantities <- nigeria_all_data$nga_vehicle_quantities %>% 
  mutate(which_grain = dplyr::case_when(
    rice == "Yes" & wheatflour == "No" & maizeflour == "No" ~ "Rice only",
    rice == "No" & wheatflour == "Yes" & maizeflour == "No" ~ "Wheat flour only",
    rice == "No" & wheatflour == "No" & maizeflour == "Yes" ~ "Maize flour only",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "No" ~ "Rice and wheat flour",
    rice == "Yes" & wheatflour == "No" & maizeflour == "Yes" ~ "Rice and maize flour",
    rice == "No" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Wheat and maize flour",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Rice, wheat flour and maize flour"
  ))

# relevel:
nigeria_all_data$nga_vehicle_quantities$which_grain <- 
  factor(nigeria_all_data$nga_vehicle_quantities$which_grain, 
         levels = c("Rice only", "Wheat flour only", "Maize flour only", 
                    "Rice and wheat flour", "Rice and maize flour", 
                    "Wheat and maize flour", "Rice, wheat flour and maize flour"))

# Get quantity of staple grain consumption in grams: 
nigeria_all_data$nga_vehicle_quantities$staplegrain_g <-
  nigeria_all_data$nga_vehicle_quantities$staplegrain_100g * 100

# Colour palette: 
stack_palette <- scale_colour_locuszoom()
stack_palette <- environment(environment(stack_palette[["palette"]])[["f"]])[["values"]]

histStack(staplegrain_g ~ which_grain, data = nigeria_all_data$nga_vehicle_quantities, 
          main = "Nigeria - LSS 2018-2019",
          legend.pos = "none", xlim = c(0, 600), col = stack_palette, 
          breaks = 40, xlab = "Staple grain consumption (grams per day, per AFE)",
          ylab = "Number of households")

# Create another version with a legend: 
histStack(staplegrain_g ~ which_grain, data = nigeria_all_data$nga_vehicle_quantities, 
          main = "Nigeria - LSS 2018-2019",
          legend.pos = "topright", xlim = c(0, 600), col = stack_palette, 
          breaks = 40, xlab = "Staple grain consumption (grams per day, per AFE)",
          ylab = "Number of households")

# Save from viewer

# ETHIOPIA

# Need to create a new variable to indicate which combination of staple grains
# each household is consuming:

ethiopia_all_data$eth_vehicle_quantities <- ethiopia_all_data$eth_vehicle_quantities %>% 
  mutate(which_grain = dplyr::case_when(
    rice == "Yes" & wheatflour == "No" & maizeflour == "No" ~ "Rice only",
    rice == "No" & wheatflour == "Yes" & maizeflour == "No" ~ "Wheat flour only",
    rice == "No" & wheatflour == "No" & maizeflour == "Yes" ~ "Maize flour only",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "No" ~ "Rice and wheat flour",
    rice == "Yes" & wheatflour == "No" & maizeflour == "Yes" ~ "Rice and maize flour",
    rice == "No" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Wheat and maize flour",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Rice, wheat flour and maize flour"
  ))

# relevel: 
ethiopia_all_data$eth_vehicle_quantities$which_grain <- 
  factor(ethiopia_all_data$eth_vehicle_quantities$which_grain, 
         levels = c("Rice only", "Wheat flour only", "Maize flour only", 
                    "Rice and wheat flour", "Rice and maize flour", 
                    "Wheat and maize flour", "Rice, wheat flour and maize flour"))

# Get quantity of staple grain consumption in grams:
ethiopia_all_data$eth_vehicle_quantities$staplegrain_g <-
  ethiopia_all_data$eth_vehicle_quantities$staplegrain_100g * 100

histStack(staplegrain_g ~ which_grain, data = ethiopia_all_data$eth_vehicle_quantities, 
          main = "Ethiopia - HICES 2015-2016",
          legend.pos = "none", xlim = c(0, 600), col = stack_palette, 
          breaks = 100, xlab = "Staple grain consumption (grams per day, per AFE)",
          ylab = "Number of households")

# INDIA

# Need to create a new variable to indicate which combination of staple grains
# each household is consuming:

india_all_data$ind_vehicle_quantities <- india_all_data$ind_vehicle_quantities %>% 
  mutate(which_grain = dplyr::case_when(
    rice == "Yes" & wheatflour == "No" & maizeflour == "No" ~ "Rice only",
    rice == "No" & wheatflour == "Yes" & maizeflour == "No" ~ "Wheat flour only",
    rice == "No" & wheatflour == "No" & maizeflour == "Yes" ~ "Maize flour only",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "No" ~ "Rice and wheat flour",
    rice == "Yes" & wheatflour == "No" & maizeflour == "Yes" ~ "Rice and maize flour",
    rice == "No" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Wheat and maize flour",
    rice == "Yes" & wheatflour == "Yes" & maizeflour == "Yes" ~ "Rice, wheat flour and maize flour"
  ))

# relevel: 
india_all_data$ind_vehicle_quantities$which_grain <- 
  factor(india_all_data$ind_vehicle_quantities$which_grain, 
         levels = c("Rice only", "Wheat flour only", "Maize flour only", 
                    "Rice and wheat flour", "Rice and maize flour", 
                    "Wheat and maize flour", "Rice, wheat flour and maize flour"))

# Get quantity of staple grain consumption in grams:
india_all_data$ind_vehicle_quantities$staplegrain_g <-
  india_all_data$ind_vehicle_quantities$staplegrain_100g * 100

histStack(staplegrain_g ~ which_grain, data = india_all_data$ind_vehicle_quantities, 
          main = "India - NSS 2011-2012",
          legend.pos = "none", xlim = c(0, 600), col = stack_palette, 
          breaks = 175, xlab = "Staple grain consumption (grams per day, per AFE)",
          ylab = "Number of households")

# save from viewer

#-------------------------------------------------------------------------------

# STAPLE GRAIN QUANTITIES BY COUNTRY: 

# Firstly create a new data-frame to include quantities from all countries, this
# will be passed through ggplot to create a violin plot: 

nga_quantities <- nigeria_all_data$nga_vehicle_quantities %>% 
  dplyr::select("staplegrain_100g") %>% 
  mutate(country = "NGA")

eth_quantities <- ethiopia_all_data$eth_vehicle_quantities %>% 
  dplyr::select("staplegrain_100g") %>% 
  mutate(country = "ETH")

ind_quantities <- india_all_data$ind_vehicle_quantities %>% 
  dplyr::select("staplegrain_100g") %>% 
  mutate(country = "IND")

# Join the data-frames together: 
country_quantities <- rbind(nga_quantities, eth_quantities, ind_quantities)

# Remove objects that are no longer required: 
rm(list = c("nga_quantities", "eth_quantities", "ind_quantities"))

# Colours for plots: 
colour_fill <- wes_palette("Darjeeling1", 3)
colour_fill <- set_names(colour_fill, c("ETH", "IND", "NGA"))

# Now produce violin plots, stratified by country: 
ggplot(data = country_quantities, 
                            aes(x = country, y = staplegrain_100g, fill = country)) +
  geom_violin(alpha = 0.8, show.legend = F, draw_quantiles = 0.5) +
  scale_fill_manual(values = colour_fill) +
  scale_y_continuous(labels = function(y) paste0(y*100, "g")) + 
  coord_cartesian(ylim = c(0, 7)) +
  labs(x = "Country",
       y = "Quantity consumed of all potentially fortifiable staple grains \n(grams per day, per AFE)",
       caption = "* Horizontal black line indicates median consumption")

# ggsave("data_constrained/LSFF_indicators/figures/staplegrains_violin.png", width = 6.5, height = 5.5,
       # dpi = 600)

# Remove unwanted objects: 
rm(list = c("country_quantities", "colour_fill"))

################################################################################

# EXPLORATION OF DOSE VARIABLES: 

# Calculate survey weighted responses for each of the options created for the 
# dose variables:

# NIGERIA

# First attach survey weights to indicators: 
nigeria_all_data$nga_indicators <- nigeria_all_data$nga_indicators %>% 
  dplyr::left_join(nigeria_all_data$nga_hh_info %>% 
                     dplyr::select("hhid", "survey_wgt"), by = "hhid") %>% 
  # Transform base_adeq to base_inadeq: 
  mutate(base_inadeq = ifelse(base_adeq == 1, 0, 
                              ifelse(base_adeq == 0, 1, NA))) %>% 
  dplyr::select("hhid", "base_inadeq", "dose1", "dose2", "dose3", "dose4", 
                "dose5", "staple_grain", "survey_wgt")
  

# Create tbl_svy object:
nigeria_all_data$svy_nga_indicators <- nigeria_all_data$nga_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

# Now calculate survey weighted ratios/percentages for the 5 dose variables: 

nga_dose <- nigeria_all_data$svy_nga_indicators %>% 
  summarise(dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
            dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
            dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
            dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci"),
            dose5 = survey_ratio(dose5, base_inadeq, na.rm = T, vartype = "ci")) 

# Create a data-frame with values stored ready for producing a table: 
nga_dose_tbl <- data.frame(indicator = c("Dose 1", "Dose 2", "Dose 3", "Dose 4"),
                           intake_obj = c("80%", "80%", "40%", "40%"),
                           n_MNs = c("All 5", "≥3", "All 5", "≥3"),
                           ratio = c(nga_dose$dose1, nga_dose$dose2, 
                                     nga_dose$dose3, nga_dose$dose4),
                           lower_ci = c(nga_dose$dose1_low, nga_dose$dose2_low, 
                                        nga_dose$dose3_low, nga_dose$dose4_low),
                           upper_ci = c(nga_dose$dose1_upp, nga_dose$dose2_upp, 
                                        nga_dose$dose3_upp, nga_dose$dose4_upp))

# Multiply ratio, lower_ci and upper_ci columns by 100 to convert to percentages:
nga_dose_tbl <- nga_dose_tbl %>% 
  mutate(ratio = round(ratio*100, digits = 2),
         lower_ci = round(lower_ci*100, digits = 2),
         upper_ci = round(upper_ci*100, digits = 2))

# Apply gt function: 
nga_dose_gt <- nga_dose_tbl %>% gt() %>% 
  cols_label(indicator = "Indicator",
             intake_obj = "Intake Objective",
             n_MNs = "n of MN's",
             ratio = "% of households",
             lower_ci = "Lower CI (%)",
             upper_ci = "Upper CI (%)") %>% 
  tab_footnote(locations = cells_column_labels(vars(ratio, lower_ci, upper_ci)),
               footnote = "All percentages are survey weighted") %>% 
  tab_footnote(locations = cells_column_labels(vars(lower_ci, upper_ci)),
               footnote = "95% confidence intervals") %>% 
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_column_labels(columns= everything())) %>% 
  cols_align(align = "right", columns = c(n_MNs))

# gtsave(nga_dose_gt, "data_constrained/LSFF_indicators/figures/nga_dose_tbl.png")

# FOR RAW HOUSEHOLD LEVEL COUNTS:
# table(nigeria_all_data$nga_indicators$dose1)
# table(nigeria_all_data$nga_indicators$dose2)
# table(nigeria_all_data$nga_indicators$dose3)
# table(nigeria_all_data$nga_indicators$dose4)
# table(nigeria_all_data$nga_indicators$dose5)

# Join population data to indicators data, ready for comparison with indicators 
# from other countries:

nigeria_all_data$nga_indicators <- nigeria_all_data$nga_indicators %>% 
  dplyr::left_join(dplyr::select(nigeria_all_data$nga_hh_info, "hhid", "res", "sep_quintile",
                                 "res_quintile"), by = "hhid")

# Remove objects no longer required: 
rm(list = c("nga_dose", "nga_dose_tbl", "nga_dose_gt"))

################################################################################

# ETHIOPIA: 

# Join population data to indicators data, ready for comparison with indicators
# from other countries:

ethiopia_all_data$eth_indicators <- ethiopia_all_data$eth_hh_info %>% 
  dplyr::select("hhid", "res", "sep_quintile", "res_quintile", "survey_wgt") %>%
  # Only keep distinct household ID's
  dplyr::distinct(hhid, .keep_all = T) %>%
  dplyr::left_join(ethiopia_all_data$eth_indicators, by = "hhid")
  

# Transform base_adeq in Ethiopia to base_inadeq (as this will be used as the 
# denominator for the dose variables):
ethiopia_all_data$eth_indicators <- ethiopia_all_data$eth_indicators %>% 
  dplyr::mutate(base_inadeq = ifelse(base_adeq == 1, 0, 
                              ifelse(base_adeq == 0, 1, NA))) %>% 
  dplyr::select("hhid", "base_inadeq", "dose1", "dose2", "dose3", "dose4", 
                "dose5", "staple_grain", 
                "survey_wgt", "res", "sep_quintile", "res_quintile")


# Create tbl_svy object:
ethiopia_all_data$svy_eth_indicators <- ethiopia_all_data$eth_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

# Now calculate survey weighted ratios/percentages for the 3 dose variables: 

eth_dose <- ethiopia_all_data$svy_eth_indicators %>% 
  summarise(dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
            dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
            dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci"),
            dose4 = survey_ratio(dose4, base_inadeq, na.rm = T, vartype = "ci"),
            dose5 = survey_ratio(dose5, base_inadeq, na.rm = T, vartype = "ci"))

# Create a data-frame with values stored ready to produce a table: 
eth_dose_table <- data.frame(indicator = c("Dose 1", "Dose 2", "Dose 3", "Dose 4"),
                             intake_obj = c("80%", "80%", "40%", "40%"),
                             n_MNs = c("All 5", "≥3", "All 5", "≥3"),
                             ratio = c(eth_dose$dose1, eth_dose$dose2, 
                                       eth_dose$dose3, eth_dose$dose4),
                             lower_ci = c(eth_dose$dose1_low, eth_dose$dose2_low, 
                                          eth_dose$dose3_low, eth_dose$dose4_low),
                             upper_ci = c(eth_dose$dose1_upp, eth_dose$dose2_upp, 
                                          eth_dose$dose3_upp, eth_dose$dose4_upp))

# Multiply ratio, lower_ci and upper_ci by 100 to convert to percentages:
eth_dose_table <- eth_dose_table %>% 
  dplyr::mutate(ratio = round(ratio * 100, digits = 2),
                lower_ci = round(lower_ci * 100, digits = 2),
                upper_ci = round(upper_ci * 100, digits = 2))

# Convert to a gt table: 
eth_dose_gt <- eth_dose_table %>% gt() %>% 
  cols_label(indicator = "Indicator",
             intake_obj = "Intake Objective",
             n_MNs = "n of MN's",
             ratio = "% of households",
             lower_ci = "Lower CI (%)",
             upper_ci = "Upper CI (%)") %>% 
  tab_footnote(locations = cells_column_labels(vars(ratio, lower_ci, upper_ci)),
               footnote = "All percentages are survey weighted") %>% 
  tab_footnote(locations = cells_column_labels(vars(lower_ci, upper_ci)),
               footnote = "95% confidence intervals") %>% 
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_column_labels(columns= everything())) %>% 
  cols_align(align = "right", columns = c(n_MNs))

# gtsave(eth_dose_gt, "data_constrained/LSFF_indicators/figures/eth_dose_tbl.png")

# Remove objects no longer required:
rm(list = c("eth_dose", "eth_dose_table", "eth_dose_gt"))


table(ethiopia_all_data$eth_indicators$dose1)
table(ethiopia_all_data$eth_indicators$dose2)
table(ethiopia_all_data$eth_indicators$dose3)
table(ethiopia_all_data$eth_indicators$dose4)
table(ethiopia_all_data$eth_indicators$dose5)

################################################################################

# VISUALISE INDICATORS DATA FROM ALL COUNTRIES:

# Add a column to each of the data-frames called "ratio" which will be used for 
# ratio calculations:
nigeria_all_data$nga_indicators$ratio <- 1
ethiopia_all_data$eth_indicators$ratio <- 1

# Create a tbl_svy object for Nigeria:
nigeria_all_data$svy_nga_indicators <- nigeria_all_data$nga_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

# Source script required to create plots:
source("data_constrained/LSFF_indicators/scripts/eda_indicators_functions.R")

# Nigeria:
stratified_plots(nigeria_all_data$svy_nga_indicators)

nga_reach_plot <- reach_plot + theme(legend.position = "right") + labs(title = "Nigeria")

nga_dose_plot <- dose_plot + theme(legend.position = "none") + labs(title = "Nigeria")
nga_reach_dose <- reach_dose_plot + theme(legend.position = "none") + 
  labs(caption = "Potentially fortifiable staple grains: Rice, Wheat flour, Maize flour",
       title = "Nigeria")

rm(list = c("reach_plot", "dose_plot", "reach_dose_plot"))

# Ethiopia:

# Create a tbl_svy object for Ethiopia:
ethiopia_all_data$svy_eth_indicators <- ethiopia_all_data$eth_indicators %>% 
  srvyr::as_survey_design(weights = survey_wgt)

stratified_plots(ethiopia_all_data$svy_eth_indicators)

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

# Save from viewer pane.

rm(list = c("nga_reach_plot", "eth_reach_plot", "nga_dose_plot", "eth_dose_plot",
            "nga_reach_dose", "eth_reach_dose"))

################################################################################
############################## END OF SCRIPT ###################################
################################################################################

# UNUSED CODE BELOW

# REACH AND DOSE BY COUNTRY: 

# Calculate survey weighted reach for each population group:

nga_dfs <- list()
eth_dfs <- list()

for (i in 1:5) {
  nga_dfs[[i]] <- svy_nga_indicators %>% 
    dplyr::filter(sep_quintile == i) %>% 
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>%
    mutate(sep_quintile = i, country = "NGA")
  
  eth_dfs[[i]] <- svy_eth_indicators %>%
    dplyr::filter(sep_quintile == i) %>% 
    summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
              dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
              dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
              dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>%
    mutate(sep_quintile = i, country = "ETH")
}

nga_reach <- do.call(rbind, nga_dfs)
eth_reach <- do.call(rbind, eth_dfs)

rm(list = c("nga_dfs", "eth_dfs", "i"))

# Combine the two data frames:
reach <- rbind(nga_reach, eth_reach)

# Re-order so that "country" and "SEP_quintile" are the first two columns:
reach_countries <- reach %>% 
  dplyr::select("country", "sep_quintile", everything())

rm("reach", "nga_reach", "eth_reach")

# Convert all numbers in reach_countries to percentages by multiplying numeric
# columns by 100: 
reach_countries[, 3:ncol(reach_countries)] <- reach_countries[, 3:ncol(reach_countries)] * 100

point_colours <- wes_palette("Darjeeling1", 2)
point_colours <- setNames(point_colours, c("Nigeria", "Ethiopia"))

# Reach_dose plot:
ggplot(reach_countries, aes(x = sep_quintile, y = reach)) +
  geom_point(aes(colour = country, size = dose3), alpha = 0.7) +
  scale_colour_manual(values = point_colours) +
  scale_size_area(breaks = c(20, 40, 60, 80),
                  labels = c(20, 40, 60, 80),
                  max_size = 10,
                  limits = c(0,100)) +
  guides(colour = guide_legend(title = NULL)) +
  guides(size = guide_legend(title = "Dose* %",
                             reverse = TRUE)) +
  labs(x = "SEP quintile", y = "Survey weighted reach (%) of potentially \n
       fortifiable staple grains (combined)", 
       caption = "* Percentage of households consuming sufficient quantity (dose) of potentially fortifiable staple grains to meet MN needs (survey weighted)") +
  # Modify x-ticks: 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("1 - Poorest", "2", "3", "4", "5 - Wealthiest")) +
  theme_bw()

#-------------------------------------------------------------------------------

# REACH AND DOSE BY COUNTRY - STRATIFIED BY URBAN/RURAL:  

nga_dfs <- list()
eth_dfs <- list()

for (i in c("Urban", "Rural")) {
  for (j in 1:5) {
    
    nga_dfs[[i]][[j]] <- svy_nga_indicators %>% 
      dplyr::filter(res_quintile == j, res == i) %>% 
      summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
                dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
                dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
                dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>%
      mutate(res_quintile = j, res = i, country = "NGA")
    
    eth_dfs[[i]][[j]] <- svy_eth_indicators %>%
      dplyr::filter(res_quintile == j, res == i) %>% 
      summarise(reach = survey_ratio(staple_grain, ratio, na.rm = T, vartype = "ci"), 
                dose1 = survey_ratio(dose1, base_inadeq, na.rm = T, vartype = "ci"),
                dose2 = survey_ratio(dose2, base_inadeq, na.rm = T, vartype = "ci"),
                dose3 = survey_ratio(dose3, base_inadeq, na.rm = T, vartype = "ci")) %>%
      mutate(res_quintile = j, res = i, country = "ETH")
    
  }
}

nga_urban <- do.call(rbind, nga_dfs$Urban)
nga_rural <- do.call(rbind, nga_dfs$Rural)
eth_urban <- do.call(rbind, eth_dfs$Urban)
eth_rural <- do.call(rbind, eth_dfs$Rural)

rm(list = c("nga_dfs", "eth_dfs", "i", "j"))

# Combine the four data frames:
reach <- rbind(nga_urban, nga_rural, eth_urban, eth_rural)

# Re-order so that "country" and "res", and "res_quintile" are the first three columns:
reach <- reach %>% 
  dplyr::select("country", "res", "res_quintile", everything())

# LOOK AT REACH DF AND PLOT REACH AND DOSE BY COUNTRY - STRATIFIED BY URBAN/RURAL:

# Convert all numbers in reach to percentages by multiplying numeric columns by 100:
reach[, 4:ncol(reach)] <- reach[, 4:ncol(reach)] * 100

point_colours <- wes_palette("Darjeeling1", 2)
point_colours <- setNames(point_colours, c("Nigeria", "Ethiopia"))

# Reach_dose plot:
ggplot(reach, aes(x = res_quintile, y = reach)) +
  geom_point(aes(colour = country, size = dose3, shape = res), alpha = 0.7) +
  scale_colour_brewer(palette = "Dark2") +
  scale_size_area(breaks = c(20, 40, 60, 80),
                  labels = c(20, 40, 60, 80),
                  max_size = 10,
                  limits = c(0,100)) +
  guides(colour = guide_legend(title = NULL),
         shape = guide_legend(title = NULL)) +
  guides(size = guide_legend(title = "Dose* %",
                             reverse = TRUE)) +
  labs(x = "SEP quintile", y = "Survey weighted reach (%) of potentially \n
       fortifiable staple grains (combined)", 
       caption = "* Percentage of households consuming sufficient quantity (dose) of potentially fortifiable staple grains to meet MN needs (survey weighted)") +
  # Modify x-ticks: 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("1 - Poorest", "2", "3", "4", "5 - Wealthiest")) +
  theme_bw()

ggsave("data_constrained/LSFF_indicators/figures/reach_dose_country_res.png", 
       width = 8, height = 6, dpi = 600)

#-------------------------------------------------------------------------------




