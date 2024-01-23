################################################################################
################ EXPLORATORY ANALYSIS FOR FORTIFICATION VEHICLES ###############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 09-Jan-2024
# Last edited: 19-Jan-2024

# This script is for performing exploratory analysis of reach of fortification 
# vehicles, and quantities consumed. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges", "table1",
                 "gtsummary", "survey")

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

# Ethiopia ESS:

eth_ess <- read_csv("fortification_models/data/eth_ess1819_vehicle_quantities.csv")

# Ethiopia HICES: 
eth_hices <- read_csv("fortification_models/data/eth_hices1516_vehicle_quantities.csv")

#-------------------------------------------------------------------------------

# Read in survey weights and other relevant variables from hh_info data: 

# Nigeria LSS:
nga_lss <- nga_lss %>% 
  left_join(read_csv("all_base_models/data/current-20240108/nga_lss1819_hh_info.csv") %>% 
              dplyr::select("hhid", "urbrur", "adm1", "adm2", "sep_quintile", 
                            "ur_quintile", "survey_wgt"), by = "hhid")

# India NSS:
ind_nss <- ind_nss %>% 
  left_join(read_csv("all_base_models/data/current-20240108/ind_nss1112_hh_info.csv") %>% 
              dplyr::select("hhid", "urbrur", "adm1", "adm2", "sep_quintile", 
                            "ur_quintile", "survey_wgt"), by = "hhid")

# Ethiopia ESS:
eth_ess <- eth_ess %>% 
  left_join(read_csv("all_base_models/data/current-20240108/eth_ess1819_hh_info.csv") %>% 
              dplyr::select("hhid", "urbrur", "adm1", "adm2", "sep_quintile", 
                            "ur_quintile", "survey_wgt"), by = "hhid")

# Ethiopia HICES:
eth_hices <- eth_hices %>% 
  left_join(read_csv("all_base_models/data/current-20240108/eth_hices1516_hh_info.csv") %>% 
              dplyr::select("hhid", "urbrur", "adm1", "adm2", "sep_quintile", 
                            "ur_quintile", "survey_wgt"), by = "hhid")

#-------------------------------------------------------------------------------

# CREATE tbl_svy OBJECTS FOR EACH OF THE DATAFRAMES: 

# Nigeria LSS:
svy_nga_lss <- nga_lss %>%
  srvyr::as_survey_design(weights = survey_wgt)

# India NSS:
svy_ind_nss <- ind_nss %>%
  srvyr::as_survey_design(weights = survey_wgt)

# Ethiopia ESS:
svy_eth_ess <- eth_ess %>%
  srvyr::as_survey_design(weights = survey_wgt)

# Ethiopia HICES:
svy_eth_hices <- eth_hices %>%
  srvyr::as_survey_design(weights = survey_wgt)

#-------------------------------------------------------------------------------

# REACH BAR PLOTS

plot_reach(svy_nga_lss) + labs(title = "Nigeria (LSS)")
# ggsave("fortification_models/figures/exploratory/nga_lss1819_reach_bar.png", 
#        width = 8, height = 6)

plot_reach(svy_ind_nss) + labs(title = "India (NSS)")
# ggsave("fortification_models/figures/exploratory/ind_nss1112_reach_bar.png",
#        width = 8, height = 6)

plot_reach(svy_eth_ess) + labs(title = "Ethiopia (ESS)")
# ggsave("fortification_models/figures/exploratory/eth_ess1819_reach_bar.png",
#        width = 8, height = 6)

plot_reach(svy_eth_hices) + labs(title = "Ethiopia (HICES)")
# ggsave("fortification_models/figures/exploratory/eth_hices1516_reach_bar.png",
#        width = 8, height = 6)

#-------------------------------------------------------------------------------

# QUANTITY OF CONSUMPTION DENSITY RIDGES:

plot_quantites(nga_lss) + labs(title = "Nigeria (LSS)") + xlim(0, 250)
# ggsave("fortification_models/figures/exploratory/nga_lss1819_quantity_density.png", 
#        width = 8, height = 6)

plot_quantites(ind_nss) + labs(title = "India (NSS)") + xlim(0, 400)
ggsave("fortification_models/figures/exploratory/ind_nss1112_quantity_density.png",
       width = 8, height = 6)

plot_quantites(eth_ess) + labs(title = "Ethiopia (ESS)") + xlim(0, 500)
# ggsave("fortification_models/figures/exploratory/eth_ess1819_quantity_density.png",
#        width = 8, height = 6)

plot_quantites(eth_hices) + labs(title = "Ethiopia (HICES)") + xlim(0, 500)
# ggsave("fortification_models/figures/exploratory/eth_hices1516_quantity_density.png",
#        width = 8, height = 6)

#-------------------------------------------------------------------------------

# SURVEY WEIGHTED SUMMARY TABLES:

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
