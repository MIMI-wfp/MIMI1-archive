# File to make sure we have clean data for
# EPHI training to be delivered March 2024


rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

# read in the data 

eth_hices1516_hh_info <- household_data("eth_hices1516")
eth_hices1516 <- apparent_intake("eth_hices1516") 
eth_hices1516_all_items <- full_item_list("eth_hices1516")

path_to_vehilces <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/"
eth_hices1516_vehicles <- read.csv(paste0(path_to_vehilces,"eth_hices1516_vehicle_quantities.csv"))
hices_food_consumption <- read.csv("~/Documents/MIMI/MIMI_data/Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv")

# read in demographic data
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

#create ea variable
eth_hces1516_demography$ea <-  paste0(as.character(eth_hces1516_demography$CQ11),
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
                                        as.character(eth_hces1516_demography$CQ17))
                                        

#------------------------------------------------------------------------------
# consumption
hices_food_consumption <- read.csv(paste0("~/Documents/MIMI/MIMI_data/", "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))

# food composition table

hices_fct <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/ETH_HCES1516_fctmatch.csv"))

names(hices_fct)

hices_fct <- hices_fct %>% 
  select(ITEMC,
         energy_in_kcal,
         vitamina_in_rae_in_mcg,
         thiamin_in_mg,
         riboflavin_in_mg,
         niacin_in_mg,
         # pantothenate_in_mg,
         vitamind_in_mcg,
         vitaminb6_in_mg,
         folate_in_mcg,
         vitaminb12_in_mcg,
         vitaminc_in_mg,
         ca_in_mg,
         fe_in_mg,
         se_in_mcg,
         zn_in_mg
  ) %>% 
  rename(
    item_code = ITEMC,
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamin_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitd_mcg = vitamind_in_mcg,
    folate_mcg = folate_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    se_mcg = se_in_mcg,
    zn_mg = zn_in_mg
  ) %>% 
  mutate(item_name = item_code)

# read in fortification data
fort_data <- read_csv("data_rich/ethiopia/src/ephi_training/data/eth_hices1516_fortifiable_foods.csv")


fortifiable_contributions <- fort_data %>% 
  left_join(hices_fct %>% 
              mutate(item_code = gsub(" ","",item_code)),
            by = "item_code") %>% 
  mutate(across(
    -c(hhid,item_code,vehicle,quantity_100g,prop_fortifiable,item_name),
    ~.x*quantity_100g
  )) %>% 
  group_by(hhid,vehicle) %>% 
  summarise(
    
    across(
      -c(item_code,quantity_100g,prop_fortifiable,item_name),
      ~sum(.)
    )
  ) %>% 
  mutate(
    vehicle = tolower(gsub(" ","_",vehicle))
  ) %>% 
  pivot_wider(
    names_from = vehicle,
    values_from = -c(hhid,vehicle)
  ) %>% 
  ungroup() %>% 
  mutate(across(
    -c(hhid),
    ~ifelse(is.na(.),0,.)
  ))

# ------------------------------------------------------------------------------

# read in the hdds groups
hices_hdds <- read_xlsx(here::here("data_rich/ethiopia/src/ephi_training/data/hices_hdds.xlsx"))

eth_hdds <- eth_hices1516_all_items %>% 
  select(hhid, item_code, quantity_g) %>% 
  left_join(hices_hdds,by = "item_code") %>% 
  dplyr::group_by(hhid) %>% 
  dplyr::mutate(
    across(
      -c(item_code, quantity_g),
      ~ifelse(is.na(.),0,.)
    )
  ) %>% 
  dplyr::mutate(
    across(
      -c(item_code, quantity_g),
      ~.x*quantity_g
    )
  ) %>%
  dplyr::summarise(
    across(
      -c(item_code, quantity_g),
      ~ifelse(sum(., na.rm = T)>0,1,0)
    )
  ) 
  
# 

# no NAs in apparent intake

summary(eth_hices1516_hh_info)
eth_hices1516_hh_info %>% 
  filter(is.na(sep_quintile))

# no NAs in hh information 

# calculate demographic info
names( eth_hces1516_demography)
# number of children under 5
eth_under5 <- eth_hces1516_demography %>% 
  group_by(hhid,ea) %>% 
  dplyr::summarise(under_5 = sum(CQ1105<=5, na.rm = T),
                   capita = n())

summary(eth_under5)


names(eth_hices1516_hh_info)



# ---------------------------------------------------------------------------
# final join
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
    ~ifelse(is.na(.), 0,.)
  )) %>% 
  dplyr::select(!c(maizeflour, maizeflour_100g, salt, salt_100g,sugar,sugar_100g,staple_grain,
                   staplegrain_100g)) %>% 
  left_join(eth_under5,
            by = "hhid") %>% 
  left_join(fortifiable_contributions, by = 'hhid') %>% 
  left_join(eth_hdds, by = "hhid")

names(eth_analysis)

summary(eth_analysis)

haven::write_dta(eth_analysis, "data_rich/ethiopia/src/ephi_training/data/hices1516.dta")

names(eth_analysis)
