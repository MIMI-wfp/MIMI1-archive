
rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign","radiant")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))




###############################################################################

nga_lss1819_all_items <- full_item_list("nga_lss1819")
nga_lss1819_hh_info <- household_data("nga_lss1819")

nga_lss1819_all_items %>% 
  filter(grepl("flour", item_name ))

all_rice_reach <- nga_lss1819_all_items %>% 
  filter(item_code %in% c(13,14)) %>% 
  mutate(rice = ifelse(quantity_g > 0,1,0)) %>% 
  group_by(hhid) %>% 
  summarise(all_rice = sum(rice),
            all_rice_quantity_g = sum(quantity_g)) %>% 
  mutate(all_rice = ifelse(all_rice>1,1,all_rice))


all_rice_reach %>% 
  summarise((
    median(all_rice_quantity_g,na.rm = T)
  ))

local_rice_reach <- nga_lss1819_all_items %>% 
  filter(item_code %in% c(13)) %>% 
  mutate(rice = ifelse(quantity_g > 0,1,0)) %>% 
  group_by(hhid) %>% 
  summarise(local_rice = sum(rice),
            local_rice_quantity_g = sum(quantity_g)) %>% 
  mutate(local_rice = ifelse(local_rice>1,1,local_rice))

imported_rice_reach <- nga_lss1819_all_items %>% 
  filter(item_code %in% c(14)) %>% 
  mutate(rice = ifelse(quantity_g > 0,1,0)) %>% 
  group_by(hhid) %>% 
  summarise(imported_rice = sum(rice),
            imported_rice_quantity_g = sum(quantity_g)) %>% 
  mutate(imported_rice = ifelse(imported_rice>1,1,imported_rice))


all_rice_reach_tot <- all_rice_reach %>% 
  left_join(local_rice_reach, by = "hhid") %>% 
  left_join(imported_rice_reach, by = "hhid")

write.csv(all_rice_reach_tot, here::here("training/nigeria/all_rice_reach.csv"))


table(all_rice_reach$rice)


sum(is.na(all_rice_reach$rice))

all_rice_reach %>% 
  summarise(sum(rice,na.rm = TRUE)/n())


all_rice_reach %>% 
  ggplot(aes(x = quantity_g))+
  geom_histogram()


## Just Kebbi state

nga_lss1819_hh_info %>% 
  filter(adm1 == 21) %>% 
  left_join(all_rice_reach,by = "hhid") %>% 
  as_survey_design(id = hhid, weights = survey_wgt) %>% 
  srvyr::summarise(survey_mean(rice==1, proportion = TRUE))


### Bread in diet --------------------------------------------------------------

nga_lss1819_all_items %>% 
  filter(item_code %in% c(25,26,27)) %>%
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  summarise(mean(quantity_g, na.rm = TRUE))

distinct(nga_lss1819_all_items,item_name,item_code)

x <- nga_lss1819_all_items %>% 
     filter(item_code %in% c(25,26,27)) %>%
     group_by(hhid) %>% 
     summarise(quantity_g = sum(quantity_g)) %>% 
      ungroup() 

quantile(x$quantity_g,probs  = seq(0,1,0.25),na.rm = T)
                   