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

ind_nss1112_all_items <- full_item_list("ind_nss1112")
nga_lss1819_all_items <- full_item_list("nga_lss1819")
eth_hices1516_all_items <- full_item_list("eth_hices1516")
#vehicle quantities purchased
path_to_vehilces <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/"
nga_lss1819_vehicles <- read.csv(paste0(path_to_vehilces,"nga_lss1819_vehicle_quantities.csv"))
nsso_nss1112_vehicles <- read.csv(paste0(path_to_vehilces,"ind_nss1112_vehicle_quantities.csv"))
eth_hices1516_vehicles <- read.csv(paste0(path_to_vehilces,"eth_hices1516_vehicle_quantities.csv"))

path_to_consumed <- "~/Documents/MIMI/gabriel/"

eth_hices1516_vehicle_cons <- read.csv(paste0(path_to_consumed,"eth_hices1516_vehicle_consumption.csv"))
nga_lss1819_vehicle_cons <- read.csv(paste0(path_to_consumed,"nga_lss1819_vehicle_consumption.csv"))
ind_nss1112_vehicle_cons <- read.csv(paste0(path_to_consumed,"ind_nss1112_vehicle_consumption.csv"))

nga_zone <- read_csv("../MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/Household/secta_cover.csv") %>% 
  dplyr::select("hhid", "zone")

# add in a food group variable for each survey from MDD-W
hices_mdd_w <- read.csv(here::here("dietary_assessment/data/hices_mdd_w.csv"))
hices_mdd_w_tubers <- read.csv(here::here("dietary_assessment/data/hices_mdd_w_tubers.csv"))
ess_mdd_w <- read.csv(here::here("dietary_assessment/data/ess_mdd_w.csv"))
nnmb_mdd_w <- read.csv(here::here("dietary_assessment/data/nnmb_mdd_w.csv"))
nsso_mdd_w <- read.csv(here::here("dietary_assessment/data/nsso_mdd_w.csv"))
fcs_mdd_w <- read.csv(here::here("dietary_assessment/data/fcs_mdd_w.csv"))


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

## -----------------------------------------------------------------------------
# NIGERIA
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
  srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
  srvyr::group_by() %>%
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
  srvyr::as_survey(id = hhid,strata = adm1, weights = survey_wgt) %>%
  srvyr::group_by(adm1) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(edible_oil == "Yes",na.rm=T)/n(),
  )

unique(eth_hices1516_vehicles$edible_oil)
eth_hices1516_hh_info  %>%
  left_join(eth_hices1516_vehicle_cons %>% 
              filter(food_item == "Wheat flour"), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,food_purchased,consumed,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
  srvyr::group_by() %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(food_purchased == "Yes",na.rm=T)/n(),
  )

eth_hices1516_vehicle_cons

eth_hices1516_hh_info  %>%
  left_join(eth_hices1516_vehicle_cons %>% 
              filter(food_item == "Wheat flour"), by = "hhid") %>%
  # full_join(nga_zone, by = "hhid") %>%
  select(hhid,consumed,food_purchased,
         adm1, adm2, res,sep_quintile, res_quintile,
         age_head,sex_head,educ_head, month,survey_wgt,afe) %>%
  filter(!is.na(sep_quintile)) %>%
  srvyr::as_survey(id = hhid,strata = , weights = survey_wgt) %>%
  srvyr::group_by(sep_quintile, res) %>%
  srvyr::summarise(
    `percent_reach_"food_purchased"` = sum(food_purchased == "Yes",na.rm=T)/n(),
  )

summary(factor(eth_hices1516_hh_info$res))

###############################################################################
# Create data for looking at time of survey
#
# Ethiopia

eth_hices1516_hh_info %>% 
  left_join(eth_hices1516_vehicle_cons, by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  dplyr::filter(food_item == "Edible oil") %>% 
  group_by(month) %>% 
  dplyr::summarise(mean = mean(purchased_100g)) %>% 
  ggplot(aes(x= month, y =mean ))+
  geom_col()
  

eth_hices1516_hh_info %>% 
  left_join(eth_hices1516_vehicle_cons, by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  dplyr::filter(food_item == "Wheat flour") %>% 
  group_by(sex_head,res) %>% 
  # dplyr::summarise(mean = mean(purchased_100g)) %>% 
  ggplot(aes(x= sex_head, y =quantity_100g*100 ))+
  geom_boxplot()+
  facet_wrap(facets = vars(res))

#sex of head of household

eth_reach_food_group <- eth_hices1516_all_items %>% 
  # dplyr::filter(quantity_g>0) %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(quantity_afe = quantity_g) %>% 
  dplyr::summarise(
    quantity_afe = sum(quantity_afe, na.rm = T)
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() 

eth_hices1516_hh_info$month <- ordered(eth_hices1516_hh_info$month,
       levels = c("January","February","March","Paquma",
                "April","May","June","July","August",
                "September","October","November","December"))


eth_reach_food_group %>% 
  dplyr::filter(food_group == "cereals") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,1500))+
  xlab("Month")+
  ylab("Cereal consumed (g)")+
  labs(title = "Ethiopia Cereal Seasonality (HICES 2015-16)")+
  theme_ipsum()
    
  
eth_reach_food_group %>% 
  dplyr::filter(food_group == "pulses") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,200))+
  xlab("Month")+
  ylab("Cereal consumed (g)")+
  labs(title = "Ethiopia Pulses Seasonality (HICES 2015-16)")+
  theme_ipsum()

eth_reach_food_group %>% 
  dplyr::filter(food_group == "asf") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,300))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "Ethiopia ASF Seasonality (HICES 2015-16)")+
  theme_ipsum()

eth_reach_food_group %>% 
  dplyr::filter(food_group == "green_veg") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,300))+
  xlab("Month")+
  ylab("Cereal consumed (g)")+
  labs(title = "Ethiopia leafy green veg Seasonality (HICES 2015-16)")+
  theme_ipsum()

eth_reach_food_group %>% 
  dplyr::filter(food_group == "vita_fruit_veg") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,100))+
  xlab("Month")+
  ylab("Cereal consumed (g)")+
  labs(title = "Ethiopia Vitamin A rich fruit/veg Seasonality (HICES 2015-16)")+
  theme_ipsum()

eth_reach_food_group %>% 
  dplyr::filter(food_group == "other_veg") %>% 
  left_join(distinct(eth_hices1516_hh_info, .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = month, y = quantity_afe)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,500))+
  xlab("Month")+
  ylab("Cereal consumed (g)")+
  labs(title = "Ethiopia 'Other vegetables' Seasonality (HICES 2015-16)")+
  theme_ipsum()



eth_hices1516_hh_info %>% 
  left_join(eth_hices1516_vehicle_cons, by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  dplyr::filter(food_item == "Wheat flour") %>% 
  group_by(sex_head, res)%>% 
  dplyr::summarise(mean = mean(purchased_100g)) 


eth_hices1516_hh_info %>% 
  left_join(eth_hices1516_vehicle_cons, by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  dplyr::filter(food_item == "Edible oil") %>% 
  group_by( res) %>% 
  dplyr::summarise(mean = mean(purchased_100g)) 

# India ------------------------------------------------------------------------
unique(ind_nss1112_all_items$hhid)
ind_reach_food_group <- 
  ind_nss1112_all_items %>%
  dplyr::filter(!is.na(quantity_g)) %>%
  dplyr::select(-c(food_group)) %>% 
  left_join(nsso_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other.veg,other.fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  # mutate(quantity_afe = quantity_g) %>% 
  dplyr::summarise(
    quantity_g = sum(quantity_g, na.rm = T)
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() 

#filter out Odisha state (added by mistake from previous analysis)
ind_nss1112_hh_info <- ind_nss1112_hh_info %>% 
  dplyr::filter(!is.na(month))


ind_nss1112_hh_info <- ind_nss1112_hh_info %>% 
  mutate(month = ordered(case_when(    
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March",
    month == 4 ~ "April",
    month == 5 ~ "May",
    month == 6 ~ "June",
    month == 7 ~ "July",
    month == 8 ~ "August",
    month == 9 ~ "September",
    month == 10 ~ "October",
    month == 11 ~ "November",
    month == 12 ~ "December"),
    levels = c("January","February","March",
               "April","May","June","July","August",
               "September","October","November","December")
    ))

ind_reach_food_group %>% 
  dplyr::filter(food_group == "green_veg") %>% 
  left_join(distinct(ind_nss1112_hh_info , .keep_all = T), by = "hhid") %>% 
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,100))


ind_reach_food_group %>% 
  dplyr::filter(food_group == "green_veg") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>% 
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,100))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India Leafy Green Vegetables Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()


ind_reach_food_group %>% 
  dplyr::filter(food_group == "cereals") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>% 
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,1000))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India Cereal Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()

ind_reach_food_group %>% 
  dplyr::filter(food_group == "pulses") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  # dplyr::filter(!is.na(month)) %>% 
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,100))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India Cereal Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()

ind_reach_food_group %>% 
  dplyr::filter(food_group == "vita_fruit_veg") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>%
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,100))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India Vitamin A rich fruit/veg Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()


ind_reach_food_group %>% 
  dplyr::filter(food_group == "other.veg") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>%
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,200))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India 'Other vegetables' Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()


ind_reach_food_group %>% 
  dplyr::filter(food_group == "other.veg") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>%
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,200))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India 'Other vegetables' Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()


ind_reach_food_group %>% 
  dplyr::filter(food_group == "asf") %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  dplyr::filter(!is.na(month)) %>%
  ggplot(aes(x = factor(month), y = quantity_g)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0,30))+
  xlab("Month")+
  ylab("Quantity consumed (g)")+
  labs(title = "India ASF Seasonality",
       subtitle = "(NSSO 2011-12)")+
  theme_ipsum()

# fortification vehicles


ind_reach_food_group %>% 
  dplyr::filter(food_group == "asf") %>%
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  # dplyr::filter(food_item == "Rice") %>%
  dplyr::mutate(month = factor(month)) %>% 
  ggplot(aes(x = sex_head, y = quantity_g)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,30))+
  facet_wrap(facets = vars(res)) +
  theme_ipsum()


ind_nss1112_hh_info %>% 
  left_join(ind_nss1112_vehicle_cons, by = "hhid") %>% 
  # unique(eth_hices1516_vehicle_cons$food_item)
  dplyr::filter(food_item == "Rice") %>% 
  dplyr::mutate(month = factor(month)) %>% 
  ggplot(aes(x = sex_head, y = purchased_100g)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,30))+
  facet_wrap(facets = vars(res))


################################################################################
# investigate the head of household level

ind_nss1112 %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  group_by(educ_head) %>% 
  summarise(med = median(energy_kcal))

ind_nss1112 %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  group_by(sep_quintile) %>% 
  summarise(med = median(energy_kcal), n())



# lowest quintile reports the lowest intake of caorie 

# sex head of household
ind_nss1112 %>% 
  left_join(distinct(ind_nss1112_hh_info, .keep_all = T), by = "hhid") %>% 
  group_by(sex_head, res) %>% 
  summarise(med = median(energy_kcal),
            n = n())

eth_hices1516_hh_info
eth_hices1516 %>% 
  left_join(distinct(eth_hices1516_hh_info , .keep_all = T), by = "hhid") %>% 
  group_by(sex_head, res) %>% 
  summarise(med = median(energy_kcal),
            n =n())

# women report a higher intake of calories - is this a bias??
# sex of household makes a big difference in terms of apparent intake 


## look at education level of head of house

ind_reach_food_group %>% 
  left_join(distinct(ind_nss1112_hh_info , .keep_all = T), by = "hhid") %>% 
  filter(food_group == "other.veg" &
           quantity_g<2000) %>% 
  ggplot(aes(x = quantity_g, y =  factor(educ_head), fill = factor(educ_head)))+
  geom_density_ridges(stat = "binline", show.legend = FALSE) 

eth_reach_food_group %>% 
  left_join(distinct(eth_hices1516_hh_info , .keep_all = T), by = "hhid") %>% 
  filter(food_group == "asf" &
           quantity_afe<2000) %>% 
  ggplot(aes(x = quantity_afe, y =  factor(educ_head), fill = factor(educ_head)))+
  geom_density_ridges(stat = "binline", show.legend = FALSE) 


# education of head of houshold makes very little differece





# what are the numbers of 


################################################################################

## food group analysis/survey design

eth_food_item_grouped <- eth_hices1516_all_items %>% 
  # dplyr::filter(quantity_g>0) %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group)


# count the number of items chosen per group
eth_food_item_grouped %>% 
  mutate(consumed = ifelse(quantity_g>0, 1,0)) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  dplyr::summarise(quantity = sum(quantity_g),
                   number_items = sum(consumed)) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = factor(number_items), y = quantity))+
  geom_boxplot(outlier.shape = NA)+
  ylim(0,2500)+
  facet_wrap(facets = vars(food_group))
  

ind_food_item_groupedp <- 
  ind_nss1112_all_items %>%
  dplyr::filter(!is.na(quantity_g)) %>%
  dplyr::select(-c(food_group)) %>% 
  left_join(nsso_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other.veg,other.fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) 

ind_food_item_groupedp %>%  
  mutate(consumed = ifelse(quantity_g>0, 1,0)) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  dplyr::summarise(quantity = sum(quantity_g),
                   number_items = sum(consumed)) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = factor(number_items), y = quantity))+
  geom_boxplot(outlier.shape = NA)+
  ylim(0,2500)+
  facet_wrap(facets = vars(food_group))


#### Overall intake ############################################################

## Ethiopia 

ethiopia_energy <- hices1516 %>% 
  select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
         vitb12_mcg) %>% 
  filter(energy_kcal<5000) %>% 
  mutate(survey = "hices") %>% 
  # bind_rows(
  #   ess1819 %>% 
  #     select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
  #            vitb12_mcg) %>% 
  #     filter(energy_kcal<5000) %>% 
  #     mutate(survey = "ess") 
  # ) %>% 
  bind_rows(
    fcs11_women %>% 
      mutate(hhid = paste0(CLUSTER,HHNO,SUBJECT),
             survey = "fcs") %>% 
      select(hhid, ENERGY_KCAL_sum,
             VITA_sum,
             IRON_MG_sum,
             ZINC_MG_sum, 
             FOL_sum,
             VITB12_sum,
             survey) %>% 
      rename(energy_kcal = ENERGY_KCAL_sum,
             vita_rae_mcg = VITA_sum,
             fe_mg =IRON_MG_sum,
             zn_mg = ZINC_MG_sum,
             folate_mcg = FOL_sum,
             vitb12_mcg = VITB12_sum) 
  ) 




ethiopia_energy %>% 
  ggplot(aes(x = energy_kcal, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported energy intake",
    x = "Energy (kcal)"
  )

ethiopia_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "Ethiopia reported vitamin A intake",
    x = "Vitmain A RAE (mcg)"
  )


ethiopia_energy %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported iron intake",
    x = "Iron (mg)"
  )

ethiopia_energy %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported zinc intake",
    x = "Zinc (mg)"
  )

ethiopia_energy %>% 
  filter(folate_mcg<1200) %>% 
  ggplot(aes(x = folate_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported folate intake",
    x = "Folate (mcg)"
  )

ethiopia_energy %>% 
  filter(folate_mcg<5) %>% 
  ggplot(aes(x = vitb12_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported vitamin B12 intake",
    x = "Vitamin B12 (mcg)"
  )


### we see some clear lower intake values in ethiopia for the individual level
### recall than the household consumption


## India

india_energy <- nsso1112 %>% 
  select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
         vitb12_mcg) %>% 
  mutate(survey = "nsso") %>% 
  bind_rows(
    nnmb_women %>% 
      select(SUBJECT,
             ENERGY_kcal,
             VITA_RAE_mcg,
             IRON_mg,
             ZINC_mg,
             FOLDFE_mcg,
             VITB12_mcg) %>% 
      rename(
        hhid = SUBJECT,
        energy_kcal = ENERGY_kcal,
        vita_rae_mcg = VITA_RAE_mcg,
        fe_mg = IRON_mg,
        zn_mg = ZINC_mg,
        folate_mcg = FOLDFE_mcg,
        vitb12_mcg = VITB12_mcg
      ) %>% 
      mutate(survey = "nnmb") 
  )

india_energy %>% 
  filter(energy_kcal<5000) %>% 
  ggplot(aes(x = energy_kcal, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported energy intake",
    x = "Energy (kcal)"
  )

india_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported vitamin A intake",
    x = "Vitmain A RAE (mcg)"
  )

india_energy %>% 
  filter(fe_mg<50) %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported iron intake",
    x = "Iron (mg)"
  )


india_energy %>% 
  filter(zn_mg<25) %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported zinc intake",
    x = "Zinc (mg)"
  )

india_energy %>% 
  filter(vitb12_mcg<5) %>%
  ggplot(aes(x = vitb12_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported vitamin B12 intake",
    x = "Vitmain B12 (mcg)"
  )

india_energy %>% 
  filter(folate_mcg<1000) %>%
  ggplot(aes(x = folate_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported folate intake",
    x = "Folate (mcg)"
  )



## look at coverage of items----------------------------------------------------

# rice - India

nsso1112_all_items %>% 
  filter(item_code == 101 |
           item_code == 102 |
           item_code == 106
  ) %>% 
  select(hhid,
         item_code,
         quantity_g) %>% 
  mutate(consumed_rice = ifelse(quantity_g > 0, 1, 0)) %>% 
  ungroup() %>%
  left_join(nsso1112_hh_info, by = "hhid") %>% 
  group_by(hhid) %>% 
  summarise(consumed_rice = max(consumed_rice)) %>% 
  ungroup() %>% 
  # summarise(consumed_rice) %>% 
  # srvyr::as_survey_design(weights = survey_wgt) %>% 
  # dplyr::group_by(adm1) %>% 
  dplyr::summarise(consumed_rice = sum(consumed_rice)/n())

nnmb_all_items %>% 
  filter(FOODEX2_INGR_CODE == "A001D#F10.A07XK") %>% 
  mutate(consumed_rice = ifelse(FOOD_AMOUNT_REPORTED>0, 1,0)) %>% 
  left_join(nnmb_subject %>% select(SUBJECT, ADM1_NAME), 
            by = "SUBJECT") %>% 
  group_by(ADM1_NAME) %>% 
  summarise(rice_percentage = sum(consumed_rice)/n())

# wheat flour - Ethiopia

fcs11_adults %>% 
  mutate(
    subject_id = paste0(CLUSTER,HHNO, SUBJECT)
  ) %>% 
  group_by(subject_id) %>% 
  select(subject_id, WHEAT_G_sum,REGION) %>% 
  ungroup() %>% 
  mutate(wheat = ifelse(WHEAT_G_sum>0,1,0)) %>% 
  # group_by(REGION) %>% 
  summarise(coverage = sum(wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

hices1516_all_items %>% 
  filter(grepl("wheat",item_code)) %>% 
  group_by(hhid) %>% 
  summarise(total_wheat = sum(quantity_g)) %>% 
  mutate(consumed_wheat = ifelse(total_wheat>0,1,0)) %>% 
  summarise(coverage = sum(consumed_wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

# edible oil - Ethiopia
# are butter/other fats fortifiable?

fcs11_adults %>% 
  mutate(
    subject_id = paste0(CLUSTER,HHNO, SUBJECT)
  ) %>% 
  group_by(subject_id) %>% 
  select(subject_id, OIL_G_sum,REGION) %>% 
  ungroup() %>% 
  mutate(oil = ifelse(OIL_G_sum>0,1,0)) %>% 
  # group_by(REGION) %>% 
  summarise(oil_consumed = sum(oil)/n())

hices1516_all_items %>% 
  filter(grepl("Edible oil",item_code)) %>% 
  group_by(hhid) %>% 
  summarise(total_oil = sum(quantity_g)) %>% 
  mutate(consumed_oil = ifelse(total_oil>0,1,0)) %>% 
  summarise(coverage = sum(consumed_oil)/n(),
            se = sqrt(coverage*(1-coverage)/n()))


ess1819_all_items %>% 
  filter(grepl("Wheat", item_name)) %>% 
  group_by(hhid) %>% 
  summarise(total_wheat = sum(quantity_g)) %>% 
  mutate(consumed_wheat = ifelse(total_wheat>0,1,0)) %>% 
  summarise(coverage = sum(consumed_wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

ess1819_all_items %>% 
  filter(grepl("Oil", item_name)) %>% 
  group_by(hhid) %>% 
  summarise(total_oil = sum(quantity_g)) %>% 
  mutate(consumed_oil = ifelse(total_oil>0,1,0)) %>% 
  summarise(coverage = sum(consumed_oil)/n(),
            se = sqrt(coverage*(1-coverage)/n()))


###### food group analyses #### 
# defining it based on MDD-W because of Vit A food groups and ASF


sum(hices_mdd_w$green_veg,na.rm = T)
sum(hices_mdd_w$vita_fruit_veg,na.rm = T)
sum(hices_mdd_w$cereals,na.rm = T)

sum(fcs_mdd_w$green_veg,na.rm = T)
sum(fcs_mdd_w$vita_fruit_veg,na.rm = T)
sum(fcs_mdd_w$cereals,na.rm = T)


sum(nsso_mdd_w$green_veg,na.rm = T)
sum(nsso_mdd_w$vita_fruit_veg,na.rm = T)
sum(nsso_mdd_w$cereals,na.rm = T)

sum(nnmb_mdd_w$green_veg,na.rm = T)
sum(nnmb_mdd_w$vita_fruit_veg,na.rm = T)
sum(nnmb_mdd_w$cereals,na.rm = T)

#leafy green veg - ETH
hices_green_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "hices")
# %>% 
# ggplot(aes(x = quantity_consumed))+ 
# geom_histogram()+
# ggtitle("HICES leafy greens") +
# xlab("Quantity (g)")+
# xlim(-100,1500)

nsso_green_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "nsso")



nnmb_green_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(green_veg == 1) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>% 
  mutate(survey = "nnmb")


bind_rows(nsso_green_quantity,nnmb_green_quantity) %>% 
  filter(quantity_consumed<100) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Leafy greens consumed",
       x = "Quantity of food group consumed (g)")



nsso_cereals_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(cereals == 1 & quantity_g>0) %>% 
  
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "nsso")



nnmb_cereals_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(cereals == 1) %>% 
  filter(cereals == 1 & FOOD_AMOUNT_REPORTED>0) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>% 
  mutate(survey = "nnmb")


bind_rows(nnmb_cereals_quantity,nsso_cereals_quantity) %>% 
  filter(quantity_consumed<1000) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Cereals consumed",
       x = "Quantity of food group consumed (g)")


nsso_vita_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<300) %>% 
  mutate(survey = "nsso")



nnmb_vita_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>%
  filter(quantity_consumed<300) %>% 
  mutate(survey = "nnmb")


bind_rows(nsso_vita_quantity,nnmb_vita_quantity) %>% 
  filter(quantity_consumed<100) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Vitamin A-rich fruits/vegetables consumed",
       x = "Quantity of food group consumed (g)")



fcs_women_greens <- fcs11_women %>% 
  mutate(
    hhid = paste0(CLUSTER,HHNO, SUBJECT) 
  ) %>% 
  select(hhid) %>% 
  left_join(
    fcs11 %>% 
      mutate(hhid = paste0(CLUSTER,HHNO, SUBJECT))
  ) %>% 
  left_join(fcs_mdd_w %>% 
              select(-SHRT_DESC) %>% 
              distinct(CODE, .keep_all = TRUE), by = "CODE") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  mutate(survey = "fcs")
# %>% 
# ggplot(aes(x = quantity_consumed))+ 
# geom_histogram()+
# ggtitle("FSS leafy greens") +
# xlab("Quantity (g)") +
# xlim(-100,1500)


bind_rows(fcs_women_greens,hices_green_quantity) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Leafy greens consumed",
       x = "Quantity of food group consumed (g)")


hices_cereal_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(cereals == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<3000) %>% 
  mutate(survey = "hices")


fcs_cereal_quantity <- fcs11_women %>% 
  mutate(
    hhid = paste0(CLUSTER,HHNO, SUBJECT) 
  ) %>% 
  select(hhid) %>% 
  left_join(
    fcs11 %>% 
      mutate(hhid = paste0(CLUSTER,HHNO, SUBJECT))
  ) %>% 
  left_join(fcs_mdd_w %>% 
              select(-SHRT_DESC) %>% 
              distinct(CODE, .keep_all = TRUE), by = "CODE") %>% 
  filter(cereals == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  mutate(survey = "fcs")

bind_rows(fcs_cereal_quantity,hices_cereal_quantity) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Cereals, roots and tubers reported",
       x = "Quantity of food group consumed (g)")


fcs_women_vita <- fcs11_women %>% 
  mutate(
    hhid = paste0(CLUSTER,HHNO, SUBJECT) 
  ) %>% 
  select(hhid) %>% 
  left_join(
    fcs11 %>% 
      mutate(hhid = paste0(CLUSTER,HHNO, SUBJECT))
  ) %>% 
  left_join(fcs_mdd_w %>% 
              select(-SHRT_DESC) %>% 
              distinct(CODE, .keep_all = TRUE), by = "CODE") %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  filter(quantity_consumed<200) %>% 
  mutate(survey = "fcs")

hices_vita_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<200) %>% 
  mutate(survey = "hices")


bind_rows(hices_vita_quantity,fcs_women_vita) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Vitamin A rich fruits/vegetables",
       x = "Quantity of food group consumed (g)")





#############################
#
# Plan
#
#############################
# We need to separate out some variables to look at 
# - single person recalling consumption for the entire household, (look at hh of house, education level, sex, age (potentially))
# - limited capture of food consumed outside of the home, 
# - variance coefficients in 24HR, 
# - intra household distribution in HCES


eth_micronutrient_food_groups <- hices1516_all_items %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(across(-c(afe,item_code,item_name,quantity_g,value,quantity_100g),
                ~.x*quantity_100g/afe)) %>% 
  dplyr::summarise(
    dplyr::across(
      -c(afe,item_code,item_name,quantity_g,value,quantity_100g),
      ~sum(.x, na.rm = TRUE)
    )
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() %>% 
  # dplyr::filter(
  #   energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]
  # ) %>% 
  dplyr::group_by(
    food_group
  ) %>% 
  dplyr::summarise(
    dplyr::across(
      -c(hhid),
      ~mean(.)
    )
  ) 

micronutrients <- colnames(eth_micronutrient_food_groups)
micronutrients <- micronutrients[c(2:6,8:10,15)]

mn_fg_plots <- list()
for(item in micronutrients){
  print(item)
  p1 <-  eth_micronutrient_food_groups %>% 
    ggplot(aes(area = !!sym(item), 
               fill = 
                 stringr::str_to_title(
                   gsub("_"," ",food_group)),
               # stringr::str_split_i(food_group,
               #                      "\\_",
               #                      1)), 
               label = stringr::str_to_title(sub("_"," ",food_group)))
           # stringr::str_to_title(
           #   stringr::str_split_i(food_group,
           #                        "\\_",
           #                        2)))
    )+
    geom_treemap() +
    geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                       grow = FALSE, size = 12)+
    labs(title = gsub("_"," ",item),
         # stringr::str_to_title(stringr::str_split_i(item,
         # "\\_",
         # 1)),
         
    )+
    scale_fill_brewer(palette = "Set3")+
    # guides(fill=guide_legend())+
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm'))+
    guides(fill = guide_legend(title="Food group",label.position = "bottom"))
  # theme(legend.direction = "horizontal", legend.position = "bottom")+
  # guides(fill = "none")+
  theme_ipsum()
  mn_fg_plots[[item]] <- p1
}
ggpubr::ggarrange(plotlist = mn_fg_plots, common.legend = TRUE)



#-------------------------------------------------------------------------------
#create reach and quantity estiamtes for each household and food group
reach_food_group <- hices1516_all_items %>% 
  # dplyr::filter(quantity_g>0) %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(quantity_afe = quantity_g) %>% 
  dplyr::summarise(
    qunatity_afe = sum(quantity_afe, na.rm = T)
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() 



reach_food_group %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  dplyr::filter(food_group == "cereals") %>% 
  dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
  dplyr::group_by(
    food_group, res, res_quintile
  ) %>% 
  dplyr::summarise(
    median_reach = median(qunatity_afe),
    reach = sum(reached == "Yes")/n(),
    n()
  ) 

