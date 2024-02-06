###

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("all_base_models/scripts/base_model_functions.R"))
source(here::here("dietary_assessment/processing/individual_level_clean.R"))

# read in the data ------------------------------------------------------------

#household apparent intake
ind_nss1112<- apparent_intake("ind_nss1112")
nga_lss1819 <- apparent_intake("nga_lss1819") 
eth_hices1516 <- apparent_intake("eth_hices1516") 
# household information
ind_nss1112_hh_info <- household_data("ind_nss1112")
nga_lss1819_hh_info <- household_data("nga_lss1819")
eth_hices1516_hh_info <- household_data("eth_hices1516")
#vehicle quantities purchased
path_to_vehilces <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/"
nga_lss1819_vehicles <- read.csv(paste0(path_to_vehilces,"nga_lss1819_vehicle_quantities.csv"))
nsso_nss1112_vehicles <- read.csv(paste0(path_to_vehilces,"ind_nss1112_vehicle_quantities.csv"))
eth_hices1516_vehicles <- read.csv(paste0(path_to_vehilces,"eth_hices1516_vehicle_quantities.csv"))

path_to_consumed <- "~/Documents/MIMI/gabriel/"

eth_hices1516_vehicle_cons <- read.csv(paste0(path_to_consumed,"eth_hices1516_vehicle_consumption.csv"))
nga_lss1819_vehicle_cons <- read.csv(paste0(path_to_consumed,"nga_lss1819_vehicle_consumption.csv"))

nga_zone <- read_csv("../MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/Household/secta_cover.csv") %>% 
  dplyr::select("hhid", "zone")
# write functions to get the data in the correct order -------------------------



nga_calculate_percent_reach <- function(food_item, group_var, cons_purch) {
 
  if(cons_purch == "consumed"){
  result <- nga_lss1819_hh_info %>%
    left_join(nga_lss1819_vehicle_cons %>% filter(food_item == {{food_item}}), by = "hhid") %>%
    
    mutate(consumed = replace_na(consumed, "No")) %>% 
    full_join(nga_zone, by = "hhid") %>%
    select(hhid,consumed,food_purchased, quantity_100g,zone,
           adm1, adm2, res,sep_quintile, res_quintile,
           age_head,sex_head,educ_head, month,survey_wgt,afe)  %>%
    filter(!is.na(sep_quintile)) %>%
    srvyr::as_survey(id = hhid, strata = group_var, weights = survey_wgt) %>%
    srvyr::group_by(!!rlang::sym(group_var)) %>%
    srvyr::summarise(
      "percent_reach_{{cons_purch}}" := sum(!!rlang::sym(cons_purch) == "Yes", na.rm = TRUE) / n()
    )
  }
  else if(cons_purch == "food_purchased"){
    result <- nga_lss1819_hh_info %>%
      left_join(nga_lss1819_vehicle_cons %>% filter(food_item == {{food_item}}), by = "hhid") %>%
      # mutate(consumed = replace_na(consumed, "No")) %>% 
      full_join(nga_zone, by = "hhid") %>%
      select(hhid,zone,food_purchased,
             adm1, adm2, res,sep_quintile, res_quintile,
             age_head,sex_head,educ_head, month,survey_wgt,afe)  %>%
      filter(!is.na(sep_quintile)) %>%
      srvyr::as_survey(id = hhid, strata = group_var, weights = survey_wgt) %>%
      srvyr::group_by(!!rlang::sym(group_var)) %>%
      srvyr::summarise(
        "percent_reach_{{cons_purch}}" := sum(!!rlang::sym(cons_purch) == "Yes", na.rm = TRUE) / n()
      )
  
  
  return(result)
  }
}

## EDIBLE OIL 

# consumed vehciles
nga_oil_total <-
  nga_lss1819_hh_info %>% 
  left_join(nga_lss1819_vehicle_cons %>% 
              filter(food_item == "Edible oil") , by = "hhid") %>% 
  mutate(consumed = replace_na(consumed, "No")) %>% 
  select(hhid,consumed,food_purchased, quantity_100g,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>% 
  filter(!is.na(sep_quintile)) %>% 
  srvyr::as_survey(id = hhid,strata =  , weights = survey_wgt) %>% 
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"consumed"` = sum(consumed == "Yes",na.rm = TRUE)/n(),
  ) %>% cross_join(

    nga_lss1819_hh_info  %>%
      left_join(nga_lss1819_vehicle_cons %>% 
                  filter(food_item == "Edible oil"), by = "hhid") %>%
      full_join(nga_zone, by = "hhid") %>%
      select(hhid,zone,food_purchased,
             adm1, adm2, res,sep_quintile, res_quintile,
             age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
      filter(!is.na(sep_quintile)) %>%
      srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
      srvyr::group_by() %>%
      srvyr::summarise(
        `percent_reach_"food_purchased"` = sum(food_purchased == "Yes",na.rm = T)/n(),
      )
  )


nga_oil_res_cons <-nga_calculate_percent_reach("Edible oil", "res","consumed") 


nga_oil_res_purch <- nga_calculate_percent_reach("Edible oil", "res","food_purchased") 
  nga_lss1819_hh_info %>%
  left_join(nga_lss1819_vehicles , by = "hhid") %>%
  full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,edible_oil_100g,zone,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(edible_oil == "Yes",na.rm = T)/n(),
  )

nga_oil_zone_cons <- nga_calculate_percent_reach("Edible oil", "zone","consumed") 
nga_oil_zone_purch <- nga_calculate_percent_reach("Edible oil", "zone","food_purchased") 


nga_oil_cons <- nga_calculate_percent_reach("Edible oil", "sep_quintile","consumed") 
nga_oil_sep_purch <-nga_calculate_percent_reach("Edible oil", "sep_quintile","food_purchased") 

## Wheat flour

# consumed vehciles
nga_wheat_total <-
  nga_lss1819_hh_info %>% 
  left_join(nga_lss1819_vehicle_cons %>% 
              filter(food_item == "Wheat flour") , by = "hhid") %>% 
  mutate(consumed = replace_na(consumed, "No")) %>% 
  select(hhid,consumed,food_purchased, quantity_100g,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>% 
  filter(!is.na(sep_quintile)) %>% 
  srvyr::as_survey(id = hhid,strata =  , weights = survey_wgt) %>% 
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"consumed"` = sum(consumed == "Yes",na.rm = TRUE)/n(),
  ) %>% cross_join(
    
    nga_lss1819_hh_info   %>%
      left_join(nga_lss1819_vehicles, by = "hhid") %>%
      full_join(nga_zone, by = "hhid") %>%
      select(hhid,wheatflour,zone,
             adm1, adm2, res,sep_quintile, res_quintile,
             age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
      filter(!is.na(sep_quintile)) %>%
      srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
      srvyr::group_by() %>%
      srvyr::summarise(
        `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm =T)/n(),
      )
  )


nga_wheat_res_cons <-nga_calculate_percent_reach("Wheat flour", "res","consumed") 

nga_wheat_res_purch <- nga_calculate_percent_reach("Wheat flour", "res","food_purchased") 
  # nga_lss1819_hh_info %>%
  # left_join(nga_lss1819_vehicles , by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  # select(hhid,wheatflour,zone,
  #        adm1, adm2, res,sep_quintile, res_quintile,
  #        age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  # filter(!is.na(sep_quintile)) %>%
  # srvyr::as_survey(id = hhid,strata = res, weights = survey_wgt) %>%
  # srvyr::group_by(res) %>%
  # srvyr::summarise(
  #   `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm = T)/n(),
  # )

nga_wheat_zone_cons <- nga_calculate_percent_reach("Wheat flour", "zone","consumed") 

nga_wheat_zone_purch <- nga_calculate_percent_reach("Wheat flour", "zone","food_purchased") 
  # nga_lss1819_hh_info  %>%
  # left_join(nga_lss1819_vehicles, by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  # select(hhid,wheatflour,zone,
  #        adm1, adm2, res,sep_quintile, res_quintile,
  #        age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  # filter(!is.na(sep_quintile)) %>%
  # srvyr::as_survey(id = hhid,strata = zone, weights = survey_wgt) %>%
  # srvyr::group_by(zone) %>%
  # srvyr::summarise(
  #   `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm = T)/n(),
  # )

nga_wheat_sep_cons <- nga_calculate_percent_reach("Wheat flour", "sep_quintile","consumed") 

nga_wheat_sep_purch <- nga_calculate_percent_reach("Wheat flour", "sep_quintile","food_purchased") 
  
  # nga_lss1819_hh_info  %>%
  # left_join(nga_lss1819_vehicles, by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  # select(hhid,edible_oil,wheatflour,zone,
  #        adm1, adm2, res,sep_quintile, res_quintile,
  #        age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  # filter(!is.na(sep_quintile)) %>%
  # srvyr::as_survey(id = hhid,strata = sep_quintile, weights = survey_wgt) %>%
  # srvyr::group_by(sep_quintile) %>%
  # srvyr::summarise(
  #   `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm = T)/n(),
  # )


## Maize flour

# consumed vehciles
nga_maize_total <-
  nga_lss1819_hh_info %>% 
  left_join(nga_lss1819_vehicle_cons %>% 
              filter(food_item == "Maize flour") , by = "hhid") %>% 
  mutate(consumed = replace_na(consumed, "No")) %>% 
  select(hhid,consumed,food_purchased, quantity_100g,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>% 
  filter(!is.na(sep_quintile)) %>% 
  srvyr::as_survey(id = hhid,strata =  , weights = survey_wgt) %>% 
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"consumed"` = sum(consumed == "Yes",na.rm = TRUE)/n(),
  ) %>% cross_join(
    
    nga_lss1819_hh_info %>%
      left_join(nga_lss1819_vehicles , by = "hhid") %>%
      full_join(nga_zone, by = "hhid") %>%
      select(hhid,maizeflour,zone,
             adm1, adm2, res,sep_quintile, res_quintile,
             age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
      filter(!is.na(sep_quintile)) %>%
      srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
      srvyr::group_by() %>%
      srvyr::summarise(
        `percent_reach_"food_purchased"` = sum(maizeflour == "Yes",na.rm =T)/n(),
      )
  )


nga_maize_res_cons <-nga_calculate_percent_reach("Maize flour", "res","consumed") 

nga_maize_res_purch <-nga_lss1819_hh_info  %>%
  left_join(nga_lss1819_vehicles, by = "hhid") %>%
  full_join(nga_zone, by = "hhid") %>%
  select(hhid,maizeflour,zone,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = res, weights = survey_wgt) %>%
  srvyr::group_by(res) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(maizeflour == "Yes",na.rm = T)/n(),
  )

nga_maize_zone_cons <- nga_calculate_percent_reach("Maize flour", "zone","consumed") 
y <- nga_calculate_percent_reach("Maize flour", "zone","food_purchased") 
nga_maize_zone_purch <- nga_lss1819_hh_info %>%
  left_join(nga_lss1819_vehicles , by = "hhid") %>%
  full_join(nga_zone, by = "hhid") %>%
  select(hhid,maizeflour,zone,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = zone, weights = survey_wgt) %>%
  srvyr::group_by(zone) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(maizeflour == "Yes",na.rm=T)/n(),
  )

nga_maize_sep_cons <- nga_calculate_percent_reach("Maize flour", "sep_quintile","consumed") 

nga_maize_sep_purch <-nga_lss1819_hh_info  %>%
  left_join(nga_lss1819_vehicles, by = "hhid") %>%
  full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,maizeflour,zone,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = sep_quintile, weights = survey_wgt) %>%
  srvyr::group_by(sep_quintile) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(maizeflour == "Yes",na.rm=T)/n(),
  )

## Rice

# consumed vehciles
# nga_rice_total <-
#   nga_lss1819_hh_info %>% 
#   left_join(nga_lss1819_vehicle_cons %>% 
#               filter(food_item == "Rice") , by = "hhid") %>% 
#   mutate(consumed = replace_na(consumed, "No")) %>% 
#   select(hhid,consumed,food_purchased, quantity_100g,
#          adm1, adm2, res,sep_quintile, res_quintile,
#          age_head,sex_head,educ_head, month,survey_wgt,afe) %>% 
#   filter(!is.na(sep_quintile)) %>% 
#   srvyr::as_survey(id = hhid,strata =  , weights = survey_wgt) %>% 
#   srvyr::group_by() %>%
#   srvyr::summarise(
#     `percent_reach_"consumed"` = sum(consumed == "Yes",na.rm = TRUE)/n(),
#   ) %>% cross_join(
#     
#     nga_lss1819_hh_info    %>%
#       left_join(nga_lss1819_vehicles, by = "hhid") %>%
#       full_join(nga_zone, by = "hhid") %>%
#       select(hhid,rice,zone,
#              adm1, adm2, res,sep_quintile, res_quintile,
#              age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
#       filter(!is.na(sep_quintile)) %>%
#       srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
#       srvyr::group_by() %>%
#       srvyr::summarise(
#         `percent_reach_"food_purchased"` = sum(rice == "Yes", na.rm = T)/n(),
#       )
#   )
# 
# 
# nga_rice_res_cons <-nga_calculate_percent_reach("Rice", "res","consumed") 
# 
# nga_rice_res_purch <- nga_lss1819_hh_info  %>%
#   left_join(nga_lss1819_vehicles, by = "hhid") %>%
#   full_join(nga_zone, by = "hhid") %>%
#   select(hhid,rice,zone,
#          adm1, adm2, res,sep_quintile, res_quintile,
#          age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
#   filter(!is.na(sep_quintile)) %>%
#   srvyr::as_survey(id = hhid,strata = res, weights = survey_wgt) %>%
#   srvyr::group_by(res) %>%
#   srvyr::summarise(
#     `percent_reach_"food_purchased"` = sum(rice == "Yes",na.rm = T)/n(),
#   )
# 
# nga_rice_zone_cons <- nga_calculate_percent_reach("Rice", "zone","consumed") 
# nga_rice_zone_purch <- nga_lss1819_hh_info  %>%
#   left_join(nga_lss1819_vehicles, by = "hhid") %>%
#   full_join(nga_zone, by = "hhid") %>%
#   select(hhid,rice,zone,
#          adm1, adm2, res,sep_quintile, res_quintile,
#          age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
#   filter(!is.na(sep_quintile)) %>%
#   srvyr::as_survey(id = hhid,strata = zone, weights = survey_wgt) %>%
#   srvyr::group_by(zone) %>%
#   srvyr::summarise(
#     `percent_reach_"food_purchased"` = sum(rice == "Yes",na.rm =T)/n(),
#   )
# 
# nga_rice_sep_cons <- nga_calculate_percent_reach("Rice", "sep_quintile","consumed") 
# nga_rice_sep_purch <- nga_lss1819_hh_info %>%
#   left_join(nga_lss1819_vehicles, by = "hhid") %>%
#   full_join(nga_zone, by = "hhid") %>%
#   select(hhid,edible_oil,rice,zone,
#          adm1, adm2, res,sep_quintile, res_quintile,
#          age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
#   filter(!is.na(sep_quintile)) %>%
#   srvyr::as_survey(id = hhid,strata = sep_quintile, weights = survey_wgt) %>%
#   srvyr::group_by(sep_quintile) %>%
#   srvyr::summarise(
#     `percent_reach_"food_purchased"` = sum(rice == "Yes",na.rm = T)/n(),
#   )
# 
# 
# 


#### Ethiopia ------------------------------------------------------------------

eth_hices1516_hh_info  %>%
  left_join(distinct(eth_hices1516_vehicles,.keep_all = T), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,maizeflour,wheatflour,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = adm1, weights = survey_wgt) %>%
  srvyr::group_by(adm1) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm=T)/n(),
  )
  

eth_hices1516_hh_info  %>%
  left_join(distinct(eth_hices1516_vehicles,.keep_all = T), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,maizeflour,wheatflour,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = adm1, weights = survey_wgt) %>%
  srvyr::group_by(adm1) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(edible_oil == "Yes",na.rm=T)/n(),
  )


eth_hices1516_hh_info  %>%
  left_join(distinct(eth_hices1516_vehicles,.keep_all = T), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,maizeflour,wheatflour,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = res, weights = survey_wgt) %>%
  srvyr::group_by(res) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm=T)/n(),
  )

eth_hices1516_hh_info  %>%
  left_join(distinct(eth_hices1516_vehicles,.keep_all = T), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,edible_oil,maizeflour,wheatflour,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(wheatflour == "Yes",na.rm=T)/n(),
  )
