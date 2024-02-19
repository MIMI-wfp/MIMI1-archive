################################################################################
################ EXPLORATORY ANALYSIS FOR FORTIFICATION VEHICLES ###############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 09-Jan-2024
# Last edited: 30-Jan-2024

# This script is for performing exploratory analysis of reach of fortification 
# vehicles, and quantities consumed. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges", "table1",
                 "gtsummary", "survey", "wesanderson")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN FUNCTIONS:
source("fortification_models/scripts/analysis/eda_functions.R")

#-------------------------------------------------------------------------------

# READ IN DATA: 

# Nigeria LSS:
nga_lss <- read_csv("fortification_models/data/nga_lss1819_vehicle_quantities.csv")

# India NSS:
ind_nss <- read_csv("fortification_models/data/ind_nss1112_vehicle_quantities.csv")

# Currently, not working with the Ethiopia ESS data - therefore comment this out
# for now (also for the rest of the script)

# # Ethiopia ESS:
# 
# eth_ess <- read_csv("fortification_models/data/eth_ess1819_vehicle_quantities.csv")

# Ethiopia HICES: 
eth_hices <- read_csv("fortification_models/data/eth_hices1516_vehicle_quantities.csv")

#-------------------------------------------------------------------------------

# Read in survey weights and other relevant variables from hh_info data: 

# Nigeria LSS:
nga_lss <- nga_lss %>% 
  left_join(read_csv("all_base_models/data/current/nga_lss1819_hh_info.csv") %>% 
              dplyr::select("hhid", "res", "adm1", "adm2", "sep_quintile", 
                            "res_quintile", "survey_wgt"), by = "hhid")

# India NSS:
ind_nss <- ind_nss %>% 
  left_join(read_csv("all_base_models/data/current/ind_nss1112_hh_info.csv") %>% 
              dplyr::select("hhid", "res", "adm1", "adm2", "sep_quintile", 
                            "res_quintile", "survey_wgt"), by = "hhid")

# # Ethiopia ESS:
# eth_ess <- eth_ess %>% 
#   left_join(read_csv("all_base_models/data/current/eth_ess1819_hh_info.csv") %>% 
#               dplyr::select("hhid", "res", "adm1", "adm2", "sep_quintile", 
#                             "res_quintile", "survey_wgt"), by = "hhid")

# Ethiopia HICES:
eth_hices <- eth_hices %>% 
  left_join(read_csv("all_base_models/data/current/eth_hices1516_hh_info.csv") %>% 
              dplyr::select("hhid", "res", "adm1", "adm2", "sep_quintile", 
                            "res_quintile", "survey_wgt"), by = "hhid")

#-------------------------------------------------------------------------------


# CREATE tbl_svy OBJECTS FOR EACH OF THE DATAFRAMES: 

# Nigeria LSS:
svy_nga_lss <- nga_lss %>%
  srvyr::as_survey_design(weights = survey_wgt)

# India NSS:
svy_ind_nss <- ind_nss %>%
  srvyr::as_survey_design(weights = survey_wgt)

# # Ethiopia ESS:
# svy_eth_ess <- eth_ess %>%
#   srvyr::as_survey_design(weights = survey_wgt)

# Ethiopia HICES:
svy_eth_hices <- eth_hices %>%
  srvyr::as_survey_design(weights = survey_wgt)

#-------------------------------------------------------------------------------

# REACH PLOTS:

plot_reach(svy_nga_lss) + labs(title = "Nigeria (LSS)")
# ggsave("fortification_models/figures/exploratory/nga_lss1819_reach.png",
#        width = 8, height = 6)

plot_reach(svy_ind_nss) + labs(title = "India (NSS)") + 
  scale_x_discrete(labels = c("Rice", "Wheat flour \n(Atta & Maida)",
                              "Staple grains \n(combined)"))
# ggsave("fortification_models/figures/exploratory/ind_nss1112_reach.png",
#        width = 8, height = 6)

# plot_reach(svy_eth_ess) + labs(title = "Ethiopia (ESS)")
# ggsave("fortification_models/figures/exploratory/eth_ess1819_reach.png",
#        width = 8, height = 6)

plot_reach(svy_eth_hices) + labs(title = "Ethiopia (HICES)")
# ggsave("fortification_models/figures/exploratory/eth_hices1516_reach.png",
#        width = 8, height = 6)

#-------------------------------------------------------------------------------

# QUANTITY OF CONSUMPTION DENSITY RIDGES (ALL HOUSEHOLDS):

# plot_quantites(nga_lss) + labs(title = "Nigeria (LSS)") + xlim(0, 250)
# # ggsave("fortification_models/figures/exploratory/nga_lss1819_quantity_density.png",
# #        width = 8, height = 6)
# 
# plot_quantites(ind_nss) + labs(title = "India (NSS)") + xlim(0, 600) +
#   scale_y_discrete(labels = c("Staple grains \n(combined)", "Wheat flour \n(Atta & Maida)",
#                               "Rice"))
# # ggsave("fortification_models/figures/exploratory/ind_nss1112_quantity_density.png",
# #        width = 8, height = 6)
# 
# plot_quantites(eth_ess) + labs(title = "Ethiopia (ESS)") + xlim(0, 500)
# # ggsave("fortification_models/figures/exploratory/eth_ess1819_quantity_density.png",
# #        width = 8, height = 6)
# 
# plot_quantites(eth_hices) + labs(title = "Ethiopia (HICES)") + xlim(0, 500)
# # ggsave("fortification_models/figures/exploratory/eth_hices1516_quantity_density.png",
# #        width = 8, height = 6)

#-------------------------------------------------------------------------------

# SURVEY WEIGHTED SUMMARY TABLES:
# 
stratified_table(nga_lss)
stratified_table(ind_nss)
stratified_table(eth_ess)
stratified_table(eth_hices)



# ** Changes to be made, deal with missing data row - no need to present 
# percentages here. 

# ** Column headings, use n's not percentages. 

# ** Ensure we can handle data if there is no consumption of a particular vehicle
# e.g. no maize flour consumption in India.

#-------------------------------------------------------------------------------

# Present survey weighted reach stratified by urban/rural and sep quintile:

stratified_plot(svy_nga_lss) + labs(title = "Nigeria (LSS)")
# ggsave("fortification_models/figures/exploratory/nga_lss1819_stratified_reach.png",
#        width = 10, height = 6.5)

stratified_plot(svy_ind_nss) + labs(title = "India (NSS)")
# ggsave("fortification_models/figures/exploratory/ind_nss1112_stratified_reach.png",
#        width = 10, height = 6.5)

stratified_plot(svy_eth_hices) + labs(title = "Ethiopia (HICES)")
# ggsave("fortification_models/figures/exploratory/eth_hices1516_stratified_reach.png",
#        width = 10, height = 6.5)


#-------------------------------------------------------------------------------

# Stratified quantities:
stratified_quantities(nga_lss) + xlim(0,400) + labs(title = "Nigeria (LSS)")
# ggsave("fortification_models/figures/exploratory/nga_lss1819_stratified_quantity.png",
#        width = 10, height = 6.5)

stratified_quantities(ind_nss) + xlim(0,600) + labs(title = "India (NSS)") + 
  scale_y_discrete(labels = c("Potentially \nfortifiable \nstaple grains \n(combined)", 
                              "Wheat flour \n(Atta & Maida)",
                              "Rice"))
# ggsave("fortification_models/figures/exploratory/ind_nss1112_stratified_quantity.png",
#        width = 10, height = 6.5)

stratified_quantities(eth_hices) + xlim(0,500) + labs(title = "Ethiopia (HICES)")
# ggsave("fortification_models/figures/exploratory/eth_hices1516_stratified_quantity.png",
#        width = 10, height = 6.5)

################################################################################
################################ END OF SCRIPT #################################
################################################################################





