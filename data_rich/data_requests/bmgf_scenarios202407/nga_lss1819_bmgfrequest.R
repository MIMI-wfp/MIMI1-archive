################################################################################
#
# BMGF Request: 
#   Modelling the potential contributions of rice, wheat flour, and bouillon
#   fortification to meet thiamine and riboflavin requirements in Nigeria
#
# (Non-reproducible) R Code
# Part 1: Preparation of analysis data frame 
#
################################################################################

setwd(here::here())

nga_cons<-read.csv(here::here("/data/current/nga_lss1819_food_consumption.csv"))
nga_fct<-read.csv(here::here("/data/current/nga_lss1819_fct.csv")
nga_hh<-read.csv(here::here("/data/current/nga_lss1819_hh_info.csv"))
nga_fort<-read.csv(here::here("/data/current/nga_lss1819_fort.csv"))
nga_fort<-read.csv(here::here("/data/current/nga_lss1819_fort.csv"))
nga_zone<-read.csv("/Users/kevintang/Downloads/NGA_2018_LSS_v01_M_CSV/Household/secta_cover.csv")
nga_zone <- nga_zone %>% select(hhid, zone)
                   
#Thia and Ribo Apparent Intake base case
nga_fct <- nga_fct %>% select(item_code, item_name, thia_mg, ribo_mg)


nga_base <- nga_cons %>% 
  left_join(nga_fct, by = "item_code") %>% 
  mutate(thia_item = if_else(!is.na(thia_mg), quantity_100g * thia_mg, 0),
         ribo_item = if_else(!is.na(ribo_mg), quantity_100g * ribo_mg, 0)) %>% 
  group_by(hhid) %>% 
  summarise(thia_supply = sum(thia_item),
            ribo_supply = sum(ribo_item)) %>% 
  left_join(nga_hh, by = "hhid") %>% 
  summarise(hhid = hhid,
            thia_ai = thia_supply/afe,
            ribo_ai = ribo_supply/afe)

#Thia and ribo contributions (wheat flour, rice)
nga_fort_contr <- nga_fort %>% 
  mutate(thia_wheatflour = wheatflour_100g * 3.5/10,
         ribo_wheatflour = wheatflour_100g * 4.1/10,
         thia_rice = rice_100g * 4.3/10,
         ribo_rice = rice_100g * 4.1/10) %>% 
  select(hhid, thia_wheatflour, ribo_wheatflour, thia_rice, ribo_rice)

#Thia and ribo contributions (bouillon, assuming 15% CODEX NRV)
nga_fort_contr <- nga_fort_contr %>% 
  left_join(nga_zone, by = "hhid")

nga_fort_contr <- nga_fort_contr %>% 
  mutate(thia_bouillon = case_when(zone == 1 ~ 1.2*0.15 * 4.8,
                                   zone == 2 ~ 1.2*0.15 * 8.5,
                                   zone == 3 ~ 1.2*0.15 * 9.1,
                                   zone == 4 ~ 1.2*0.15 * 3.9,
                                   zone == 5 ~ 1.2*0.15 * 4.8,
                                   zone == 6 ~ 1.2*0.15 * 2.7),
         ribo_bouillon = case_when(zone == 1 ~ 1.2*0.15 * 4.8,
                                   zone == 2 ~ 1.2*0.15 * 8.5,
                                   zone == 3 ~ 1.2*0.15 * 9.1,
                                   zone == 4 ~ 1.2*0.15 * 3.9,
                                   zone == 5 ~ 1.2*0.15 * 4.8,
                                   zone == 6 ~ 1.2*0.15 * 2.7))

#Thia and ribo contributions (all)
nga_fort_contr[is.na(nga_fort_contr)] <- 0

nga_fort_contr <- nga_fort_contr %>% 
  mutate(thia_allvehicles = thia_wheatflour + thia_rice + thia_bouillon,
         ribo_allvehicles = ribo_wheatflour + ribo_rice + ribo_bouillon)

#Combine variables into analytical data frame
nga_bmgfrequest <- nga_base %>% 
  left_join(nga_fort_contr, by = "hhid") %>% 
  left_join(nga_hh, by = "hhid") %>% 
  select(-X)

write_csv(nga_bmgfrequest, here::here("/data/current/nga_lss1819_bmgfrequest.csv"))
