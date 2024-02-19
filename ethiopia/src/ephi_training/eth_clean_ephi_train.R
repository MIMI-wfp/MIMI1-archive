# File to make sure we have clean data for
# EPHI training to be delivered March 2024


rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("all_base_models/scripts/base_model_functions.R"))

# read in the data 

eth_hices1516_hh_info <- household_data("eth_hices1516")
eth_hices1516 <- apparent_intake("eth_hices1516") 
eth_hices1516_all_items <- full_item_list("eth_hices1516")

path_to_vehilces <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/"
eth_hices1516_vehicles <- read.csv(paste0(path_to_vehilces,"eth_hices1516_vehicle_quantities.csv"))
hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))
rm(path_to_consumed)

eth_hces1516_demography <- as.data.frame(read.spss(here::here("../MIMI_data/Ethiopia/HICES/ETH-HICE-2016/Data/SPSS 16/HCES_2015_2016_DEMOGRAPHY.sav")))
eth_hces1516_demography$hhid <-  paste0(as.character(eth_hces1516_demography$CQ11),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ12),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ13),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ14),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ15),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ16),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ17),
                                        "_",
                                        as.character(eth_hces1516_demography$CQ18))

# ------------------------------------------------------------------------------

summary(eth_hices1516)

# no NAs in apparent intake

summary(eth_hices1516_hh_info)
eth_hices1516_hh_info %>% 
  filter(is.na(sep_quintile))

# no NAs in hh information 

# calculate demographic info
names( eth_hces1516_demography)

eth_under5 <- eth_hces1516_demography %>% 
  group_by(hhid) %>% 
  dplyr::summarise(under_5 = sum(CQ1105<=5, na.rm = T))

summary(eth_under5)


names(eth_hices1516_hh_info)

eth_hices1516_hh_info
eth_analysis <- 
  eth_hices1516 %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  left_join(eth_hices1516_hh_info, 
            by = "hhid") %>% 
  dplyr::select(-c(X, afe.x, afe.y, total_per_cap)) %>% 
  left_join(eth_hices1516_vehicles %>% 
              distinct(hhid, .keep_all = TRUE), 
            by = "hhid") %>% 
  mutate(across(
    c(rice_100g, wheatflour_100g,maizeflour_100g,sugar_100g, edible_oil_100g,salt_100g, ),
    ~replace_na(.,0)
  )) %>% 
  dplyr::select(!c(maizeflour, maizeflour_100g, salt, salt_100g,sugar,sugar_100g,staple_grain,
                   staplegrain_100g)) %>% 
  left_join(eth_under5,
            by = "hhid")

summary(eth_analysis)

haven::write_dta(eth_analysis, "ethiopia/src/ephi_training/data/hices1516.dta")

