### quick analysis for bias paper





source(here::here("all_base_models/scripts/base_model_functions.R"))


#read in base models

nga1819 <- apparent_intake("nga1819")
nga1819_all_items <- full_item_list("nga1819")

nsso1112<- apparent_intake("nsso1112")
nsso1112_all_items <- full_item_list("nsso1112")
nsso1112_hh_info <- household_data("nsso1112")

ess1819 <- apparent_intake("ess1819")
ess1819_all_items <- full_item_list("ess1819")

mwi1516 <- apparent_intake("mwi1516")
nsso1112_all_items <-  full_item_list("nsso1112")

hices1516 <- apparent_intake("hices1516")
hices1516 <- hices1516 %>% 
  left_join(hices_hh_info, by = "hhid")

hices1516_all_items <- full_item_list("hices1516")




## read in a time stamp --------------------------------------------------------

nsso1112_all_items %>% 
  filter(item_code == 101 | item_code == 101) %>% 
  select(hhid,
         item_code,
         quantity_g) %>% 
  mutate(consumed_rice = ifelse(quantity_g > 0, 1, 0)) %>% 
  ungroup() %>% 
  left_join(nsso1112_hh_info, by = "hhid") %>% 
  # summarise(consumed_rice) %>% 
  srvyr::as_survey_design(weights = survey_wgt) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(consumed_rice = sum(consumed_rice/n() ))

  

         