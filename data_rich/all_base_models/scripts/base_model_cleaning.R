### make all countries with the same structure
### MIMI
### 12-12-2023


# LAST UPDATED BY MO OSMAN ON 20-04-24, TO UPDATE NGA FOOD COMPOSITION TABLES
# Updated to include Rupi's updates to food matching on India FCT.



library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(haven)
library(ggplot2)
library(readxl)




path_to_data <- "../MIMI_data/"
path_to_save <-here::here("data_rich/all_base_models/data/current//")
# path_to_save <-  here::here("data_rich/India/data/processed/lsff/")



setwd(here::here())

source("data_rich/all_base_models/scripts/base_model_functions.R")

# nsso #########################################################################

nsso_food_consumption <- read.csv(here::here("data_rich","India", "data", "processed", "extra_states","india_daily_consumption.csv"))
# nsso_food_consumption <- read_csv("~/Documents/MIMI/code/data_rich/India/data/processed/extra_states/india_daily_consumption.csv")


nsso_food_consumption <- 
  nsso_food_consumption %>% 
  rename(
    hhid = HHID,
    item_code = Item_Code,
    quantity_g = Total_Consumption_Quantity,
    value = Total_Consumption_Value,
    food_group = hdds_groups
  ) %>% 
  select(hhid, item_code, quantity_g, value, food_group) %>% 
  mutate(quantity_100g = quantity_g/100)


nsso_fct <- read_xlsx(paste0(path_to_data, "India/India_NSSO_2012/nsso_fct_20240513.xlsx"), sheet = 1)
# nsso_fct <- read.csv("~/code/data_rich/India/data/processed/extra_states/matched_fct.csv")
nsso_fct <- nsso_fct %>%
  rename(item_Name1 = item_name) %>% 
  select(-ends_with(
    c(
      "_id",
      "_name",
      "_source"
    )
    
  )) %>% 
  
  rename(
    # item_code = Item_Code,
    item_name = item_Name1,
    vita_rae_mcg = vita_mcg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mcg,
    fe_mg = iron_mg,
    ca_mg = calcium_mg,
    zn_mg = zinc_mg,
    # na_mg = sodium_mg,
    thia_mg = vitb1_mg,
    ribo_mg = vitb2_mg,
    niac_mg = vitb3_mg,
  ) 

nsso_basics <- read.csv(here::here("data_rich/India/data/raw/block_1_2_identification.csv"))


nsso_include_hh <- nsso_food_consumption %>% 
  inner_join(nsso_fct %>% select(item_code, energy_kcal), by = 'item_code') %>% 
  mutate(energy_kcal = energy_kcal*quantity_100g) %>% 
  group_by(hhid) %>% 
  summarise(energy_kcal = sum(energy_kcal, na.rm = T)) %>% 
  ungroup() %>% 
  #keep the three states
  right_join(nsso_basics %>% filter(State_code %in% c(9,10,22)) %>% 
               select(HHID), by = c('hhid' = 'HHID')) %>% 
  filter(energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]) 


nsso_food_consumption <- nsso_food_consumption %>% 
  filter(hhid %in% nsso_include_hh$hhid)


write_csv(nsso_fct, paste0(path_to_save,"ind_nss1112_fct.csv"))



write_csv(nsso_food_consumption, paste0(path_to_save,"ind_nss1112_food_consumption.csv"))

# 
# nsso_fct <- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_matched_fct.csv"))







# rm(nsso_afe)
rm(nsso_food_consumption)
rm(nsso_fct)
# rm(nsso_afe)


# Ethiopia #####################################################################
# HICES ------------------------------------------------------------------------

hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))


hices_food_consumption <- hices_food_consumption %>% 
  rename(
    item_code = ITEMC,
    quantity_g = QUANTITY,
    value = VALUE,
    food_group = TOP4
  ) %>% 
  select(hhid, item_code, quantity_g, value, food_group) %>% 
  mutate(quantity_g = quantity_g/365,
         value = value/365) %>% 
  mutate(quantity_100g = quantity_g/100)

write_csv(hices_food_consumption, paste0(path_to_save,"eth_hices1516_food_consumption.csv"))

# fct 

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

write_csv(hices_fct, paste0(path_to_save,"eth_hices1516_fct.csv"))



rm(hices_fct)
rm(hices_food_consumption)
# rm(hices_afe)

# ESS --------------------------------------------------------------------------

ess_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_food_cons.csv"))

ess_food_consumption
ess_food_consumption <- ess_food_consumption %>% 
  filter(g_d_nep>0) %>% 
  rename(
    hhid = HHID,
    item_code = fcode,
    quantity_g = g_d_nep,
    quantity_100g = g100_d_nep,
    # value = VALUE,
    food_group = foodgroup_hhdd_ext
  ) %>% 
  select(hhid, item_code, quantity_g,quantity_100g, food_group) 


write_csv(ess_food_consumption, paste0(path_to_save,"eth_ess1819_food_consumption.csv"))

# fct 

ess_fct <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_fct_full_complete.csv"))

names(ess_fct)

ess_fct <- ess_fct %>% 
  select(fcode,
         ref_fooditem,
         energy_in_kcal,
         vitamina_in_rae_in_mcg,
         thiamin_in_mg,
         riboflavin_in_mg,
         niacin_in_mg,
         pantothenate_in_mg,
         # vitamind_in_mcg,
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
    item_code = fcode,
    item_name = ref_fooditem,
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamin_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitb5_mg = pantothenate_in_mg,
    folate_mcg = folate_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    se_mcg = se_in_mcg,
    zn_mg = zn_in_mg
  )

write_csv(ess_fct, paste0(path_to_save,"eth_ess1819_fct.csv"))



rm(ess_fct)
rm(ess_food_consumption)
# rm(ess_afe)

# MWI ##########################################################################

mwi_base_model<- read.csv(paste0(path_to_data, "mwi/hh_mod_g_final.csv"))

mwi_food_consumption <- mwi_base_model %>% 
  select(
     item_code,
     HHID,
     g100_d_nep,
     food.group
     ) %>% 
  rename(
    hhid = HHID,
    quantity_100g = g100_d_nep,
    food_group = food.group
  ) %>% 
  mutate(
    quantity_g = quantity_100g*100
  ) %>% 
  filter(
    quantity_g>0
  )


write_csv(mwi_food_consumption, paste0(path_to_save,"mwi_ihs1617_food_consumption.csv"))


mwi_fct <- mwi_base_model %>% 
  select(-c(
    X,
    HHID,
    g100_d_nep,
    food.group
  )) %>% 

  distinct(item_code, .keep_all = TRUE) %>% 
  rename(
    energy_kcal = energy.kcal,
    vita_rae_mcg = vitarae.mcg,
    thia_mg = thia.mg,
    ribo_mg = ribo.mg,
    niac_mg = niac.mg,
    vitb6_mg = vitb6.mg,
    folate_mcg = fol.mcg, 
    vitb12_mcg = vb12.mcg,
    se_mcg = se.mcg,
    vitc_mg = vc.mg, 
    ca_mg = ca.mg, 
    fe_mg = fe.mg,
    zn_mg = zn.mg
  ) %>% 
  select(
    item_code,
    energy_kcal,
    vita_rae_mcg,
    thia_mg,
    ribo_mg,
    niac_mg,
    vitb6_mg,
    folate_mcg, 
    vitb12_mcg,
    se_mcg,
    vitc_mg, 
    ca_mg, 
    fe_mg,
    zn_mg
  ) %>% 
  mutate(item_name = item_code)
names(mwi_fct)

# fct 


write_csv(mwi_fct, paste0(path_to_save,"mwi_ihs1617_fct.csv"))


rm(mwi_fct)
rm(mwi_food_consumption)

# nga ##########################################################################

nga_food_consumption<- read.csv(paste0(path_to_data, "nga/sect6b_food_cons_final.csv"))

nga_lss1_estimates <- haven::read_dta(paste0(path_to_data,"nga/NGA_LSS1819_estimates.dta"))


nga_afe <- nga_lss1_estimates %>% 
  dplyr::select(hhid,
         hhafe) %>% 
  rename(afe = hhafe)


nga_food_consumption <- nga_food_consumption %>% 
  select(
    hhid,
    item_code,
    g100_d_nep,
    food_group
  ) %>% 
  rename(
    quantity_100g = g100_d_nep,
  ) %>% 
  mutate(
    quantity_g = quantity_100g*100,
    item_code = item_code
  ) %>% 
  filter(
    quantity_g>0
  )

summary(nga_food_consumption)

write_csv(nga_food_consumption, paste0(path_to_save,"nga_lss1819_food_consumption.csv"))


nga_fct <- read.csv(paste0(path_to_data, "nga/fct_nga_v4.0_full.csv"))
nga_item_names <- readxl::read_xlsx(paste0(path_to_data,"nga/NGA_LSS1819_fctmatch.xlsx"), sheet = "Nutrient values", skip = 1)

nga_fct <- nga_fct %>% 
  select(
   item_code,
   ref_fooditem,
   energy_in_kcal,
   vitamina_in_rae_in_mcg,
   thiamine_in_mg,
   riboflavin_in_mg,
   niacin_equivalent_in_mg,
   vitaminb6_in_mg,
   vitamind_in_mcg,
   folate_total_in_mcg,
   vitaminb12_in_mcg,
   vitaminc_in_mg,
   ca_in_mg,
   fe_in_mg,
   zn_in_mg
  ) %>% 
  rename(
    item_name = ref_fooditem,
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamine_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_equivalent_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitd_mcg = vitamind_in_mcg,
    folate_mcg = folate_total_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    zn_mg = zn_in_mg
  ) %>% 
  filter(!is.na(item_code)) %>% 
  mutate(item_code = as.integer(item_code))


# fct 

as_tibble(nga_fct)

# THIS IS THE OLD FCT WHICH WILL NOW BE REPLACED BY KATIE ADAMS FCT:
# write_csv(nga_fct, paste0(path_to_save,"nga_lss1819_fct.csv"))

rm(nga_fct)

#-------------------------------------------------------------------------------

# CREATE ALTERNATIVE NIGERIA FCT BASED ON KATIE ADAMS' FOOD COMPOSITION MATCHES: 

# Read in the food composition table: 
KA_nga_fct <- read_excel("data_rich/all_base_models/data/KA_nga_lss1819_fct/Nigeria nutrient values v6.xlsx",
                         sheet = "Nutrient values", skip = 1)

# Filter for the relevant columns: 
KA_nga_fct <- KA_nga_fct %>% 
  dplyr::select(c(1, 2, 3, 17, 33, 38, 39, 40, 43, 36, 44, 46, 47, 24, 25, 30))

# Rename columns based on their index position: 
new_col_names <- c("zone", "item_code", "item_name", "energy_kcal", "vita_rae_mcg",
                   "thia_mg", "ribo_mg", "niac_mg", "vitb6_mg", "vitd_mcg",
                   "folate_mcg", "vitb12_mcg", "vitc_mg", "ca_mg", "fe_mg",
                   "zn_mg")

colnames(KA_nga_fct) <- new_col_names

# Additional pre-processing of fct: 
KA_nga_fct <- KA_nga_fct %>% 
  mutate(item_code = as.integer(item_code)) %>% 
  filter(!is.na(item_code)) 

as.tibble(KA_nga_fct)

# Write this alternative fct to csv: 
write_csv(KA_nga_fct, paste0(path_to_save,"current/nga_lss1819_fct.csv"))


rm(nga_food_consumption)

################################################################################
##                                                                            ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
##                       household information                                ##
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################



# nsso #########################################################################

# nsso_basics <- read.csv("~/code/data_rich/India/data/raw/block_1_2_identification.csv")
# 
# nsso_household_information <- read.csv("~/code/data_rich/India/data/processed/extra_states/household_char.csv")
# nsso_demographics <- read.csv("~/code/data_rich/India/data/processed/demographics.csv")
# nsso_expenditure <- read.csv("~/code/data_rich/India/data/raw/block_12_consumer_expenditure.csv")
# nsso_afe<- read.csv(here::here("data_rich","India", "data", "processed", "extra_states","india_afe.csv"))
#extra states
nsso_basics <- read.csv(here::here("data_rich/India/data/raw/block_1_2_identification.csv"))

nsso_household_information <- read.csv("~/MIMI_mac/code/data_rich/India/data/processed/extra_states/household_char.csv")
nsso_demographics <- read.csv("~/MIMI_mac/code/data_rich/India/data/processed/extra_states/demographics.csv")
nsso_expenditure <- read.csv(here::here("data_rich/India/data/raw/block_12_consumer_expenditure.csv"))
nsso_afe<- read.csv("~/MIMI_mac/code/data_rich/India/data/processed/extra_states/india_afe.csv")




nsso_afe <- nsso_afe %>%
  rename(hhid = HHID) %>%
  select(hhid,
         afe)


nsso_household_information <- nsso_household_information %>%
  inner_join(nsso_demographics, by = "HHID") %>%
  filter(Relation == "Self") %>%
  mutate(sex_head = Sex,
         age_head = Age,
         educ_head = Education,
         res = HH_Type_code) %>%
   select(HHID,
          res,
          sex_head,
          age_head,
          educ_head,
          Combined_multiplier) %>%
   rename(hhid = HHID,
          survey_wgt = Combined_multiplier)


nsso_basics <- nsso_basics %>%
  select(HHID,
         Date_of_Survey,
         State_code,
         District_code) %>%
  mutate(year = paste0("20",
                       str_sub(Date_of_Survey, -2,-1)),
         month = str_sub(Date_of_Survey, -4,-3)) %>%
  rename(hhid = HHID,
         adm1 = State_code,
         adm2 = District_code
         )

# NEED SEP QUNINTILES -- MAYBE CALCULATE MYSLEF?

nsso_expenditure <- nsso_expenditure %>%
  rename(hhid = HHID) %>%
  filter(Srl_no == 49) %>%
  left_join(nsso_afe, by = "hhid") %>%
  mutate(expenditure = Value) %>%
  ungroup() %>%
  select(hhid, expenditure)
# %>%
#   mutate(sep_quintile = ntile(expenditure, 5)) %>%
#   group_by(urbrur) %>%
#   mutate(ur_quintile = ntile(expenditure, 5)) %>%
#   ungroup()

nsso_household_information <-  nsso_household_information %>%
  right_join(nsso_basics %>% filter(adm1 %in% c(9,10,22))
            , by = "hhid") %>%
  left_join(nsso_expenditure, by = "hhid") %>%
  left_join(nsso_afe, by= "hhid") %>%
  mutate(sep_quintile = ntile(expenditure, 5)) %>%
  group_by(res) %>%
  mutate(res_quintile = ntile(expenditure, 5)) %>%
  ungroup()
  # select(-c(expenditure))

# summary(nsso_household_information)
nsso_household_information

write_csv(nsso_household_information, paste0(path_to_save,"ind_nss1112_hh_info.csv"))

rm(nsso_household_information)
rm(nsso_basics)
rm(nsso_demographics)


# hices -----------------------------------------------------------------------

hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))
hices_rur_quintiles <- read.csv(here::here("ethiopia/data/urb_rur_quintiles.csv"))




hices_afe <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_afe.csv"))


hices_hh_info <- hices_food_consumption %>% 
  select(hhid,
         CQ11, 
         CQ12,
         MONTH,
         UR,
         SEX_Head,
         Age_Head,
         EDUC_Head,
         WGT) %>% 
  rename(
    adm1 = CQ11,
    adm2 = CQ12,
    month = MONTH,
    res = UR,
    sex_head = SEX_Head,
    age_head = Age_Head,
    educ_head = EDUC_Head,
    survey_wgt = WGT
  ) %>% 
  mutate(adm2 = paste0(adm1,"_",adm2)) %>% 
  group_by(hhid) %>% 
  slice(1) %>% 
  left_join(
    distinct(hices_rur_quintiles,.keep_all = T), by = "hhid"
  ) %>% 
  select(-c(UR, EXPCC)) %>% 
  rename(sep_quintile = quintile,
         res_quintile = ur_quintile) %>% 
  left_join(hices_afe, by = "hhid")

write.csv(hices_hh_info, paste0(path_to_save, "eth_hices1516_hh_info.csv"))      

# ESS --------------------------------------------------------------------------

#  adm1 = saq01, adm2 = saq02, urbrur = saq14, hhid = household_id, date = InterviewStart
# head of household == 

ess1 <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect_cover_hh_w4.csv"))
ess2_roster <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect1_hh_w4.csv"))
ess3_educ <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect2_hh_w4.csv"))
ess_qunit<- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/cons_agg_w4.csv"))

ess_afe <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_hme.csv"))
ess_afe <- ess_afe %>% 
  select(HHID,afe) %>% 
  rename(hhid = HHID)


ess_hh_info <- ess1 %>% 
  select(
    household_id,
    saq14,
    pw_w4,
    saq01,
    saq02,
    InterviewStart
    ) %>% 
  left_join(
    ess2_roster %>% 
      select(
        household_id,
        s1q01,
        s1q02,
        s1q03a
      ),
    by = "household_id"
    ) %>% 
  filter(
    s1q01 == "1. Head"
  ) %>% 
  rename(
    hhid = household_id,
    res = saq14,
    adm1 = saq01,
    adm2 = saq02,
    sex_head = s1q02,
    age_head = s1q03a,
    survey_wgt = pw_w4
  ) %>% 
  left_join(
    ess3_educ %>% 
      select(household_id, 
             individual_id,
             s2q06
             ) %>% 
      filter(individual_id == 1) %>% 
      rename(
        hhid = household_id,
        educ_head = s2q06) %>% 
      mutate(educ_head = ifelse(is.na(educ_head),"None", educ_head)),
    by = "hhid"
  ) %>% 
  mutate(
    year =  str_sub(InterviewStart, 1,4),
    month = str_sub(InterviewStart, 6,7),
    sex_head = str_sub(sex_head, 4, -1)
  ) %>% 
  left_join(ess_qunit %>% rename(hhid = household_id) %>% 
              select(hhid, spat_totcons_aeq,cons_quint),
            by = "hhid") %>% 
  mutate(sep_quintile = ntile(spat_totcons_aeq, 5)) %>% 
  group_by(res) %>% 
  mutate(res_quintile = ntile(spat_totcons_aeq, 5)) %>% 
  ungroup() %>% 
  select(-c(
    individual_id,
    InterviewStart,
    s1q01,
    spat_totcons_aeq,
    cons_quint
  )) %>% 
  left_join(ess_afe, by = "hhid")



  

write.csv(ess_hh_info, paste0(path_to_save, "eth_ess1819_hh_info.csv"))

## need SEP as well

# mwi --------------------------------------------------------------------------


mwi1_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_a_filt.csv"))
mwi2_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_b.csv"))
mwi3_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_c.csv"))
mwi_sep_qunit <- read.csv("~/Documents/MIMI/MIMI_data/mwi/MWI_2016_IHS-IV_v04_M_CSV/consumption_aggregate/ihs4 consumption aggregate.csv")

mwi_afe<- read.csv(paste0(path_to_data, "mwi/mwi_afe.csv"))
mwi_afe <- mwi_afe %>% 
  rename(hhid = HHID)



mwi_hh_info <- mwi1_hh %>% 
  select(
    HHID, 
    case_id,
    region,
    district,
    reside,
    interviewDate,
    hh_wgt) %>% 
  left_join(
    mwi2_hh %>% 
      select(
        HHID,
        PID,
        hh_b03,#sex
        hh_b04,#relationship
        hh_b06b
        
      ),
    by = "HHID"
  ) %>% 
  left_join(mwi3_hh %>% 
              select(HHID,
                     PID,
                     hh_c09),
            by = c("HHID", "PID")
            ) %>% 
  rename(
    hhid = HHID,
    adm1 = region,
    adm2 = district,
    res = reside,
    survey_wgt = hh_wgt
    
  ) %>% 
  filter(hh_b04 == 1) %>% 
  mutate(
    age_head = as.numeric(str_sub(interviewDate, 1,4)) - as.numeric(hh_b06b),
    sex_head = ifelse(hh_b03 == 1, "Male", "Female"),
    educ_head = hh_c09,
    res = ifelse(res == 1, "Urban", "Rural"),
    year = str_sub(interviewDate, 1,4),
    month = str_sub(interviewDate, 6,7)
  ) %>% 
  left_join(mwi_sep_qunit,
            by = "case_id") %>% 
  mutate(sep_quintile = ntile(rexpaggpc, 5)) %>% 
  group_by(res) %>% 
  mutate(res_quintile = ntile(rexpaggpc, 5)) %>% 
  select(
    hhid,
    adm1,
    adm2,
    res,
    age_head,
    sex_head,
    educ_head,
    sep_quintile,
    res_quintile,
    year, 
    month,
    survey_wgt
  ) %>% 
  left_join(mwi_afe, by = "hhid")

# need sep!

write.csv(mwi_hh_info, paste0(path_to_save, "mwi_ihs1617_hh_info.csv"))
rm(mwi_hh_info)
rm(mwi1_hh)
rm(mwi2_hh)
rm(mwi3_hh)

# nga --------------------------------------------------------------------------

nga_hh1 <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/secta_cover.csv"))
nga_roster <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/sect1_roster.csv"))
nga_edu <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/sect2_education.csv"))
nga_consumption <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/totcons.csv"))

nga_lss1_estimates <- haven::read_dta(paste0(path_to_data,"nga/NGA_LSS1819_estimates.dta"))


nga_afe <- nga_lss1_estimates %>% 
  select(hhid,
         hhafe) %>% 
  rename(afe = hhafe)

nga_hh_info <- nga_hh1 %>% 
  select(hhid,
         sector,
         state,
         lga,
         InterviewStart,
         wt_final) %>% 
  mutate(
    month =format(as.Date(InterviewStart, "%m/%d/%Y %H:%M:%S"),"%m"),
    year =format(as.Date(InterviewStart, "%m/%d/%Y %H:%M:%S"),"%Y")
  ) %>% 
  select(-InterviewStart) %>% 
  left_join(
    nga_roster %>% 
      select(hhid, 
             indiv,
             s01q02,
             s01q03,
             s01q04a) %>% 
      left_join(nga_edu %>% 
                  select(hhid,
                         indiv,
                         s02q07),
                  by = c("hhid","indiv")
                ) %>% 
      filter(s01q03 == 1),
    by = "hhid"
  ) %>% 
  mutate(
    res = ifelse(sector == 1,"Urban", "Rural"),
    sex_head = ifelse(s01q02 == 1, "Male", "Female"),
  ) %>% 
  rename(age_head = s01q04a,
         educ_head = s02q07,
         survey_wgt = wt_final,
         adm1 = state,
         adm2 = lga) %>% 
  left_join(nga_consumption %>% 
              dplyr::select(hhid, totcons_adj),
            by = "hhid")  %>% 
  rename(total_consumption = totcons_adj) %>% 
  mutate(sep_quintile = ntile(total_consumption, 5)) %>% 
  group_by(res) %>% 
  mutate(res_quintile = ntile(total_consumption, 5)) %>% 
  ungroup() %>% 
  select(
    hhid,
    adm1,
    adm2,
    res,
    sep_quintile,
    res_quintile,
    age_head,
    sex_head,
    educ_head,
    year, 
    month,
    survey_wgt
  ) %>% 
  left_join(nga_afe, by = "hhid") %>% 
  filter(hhid %in% unique(food_consumption$hhid))




# Add in enumeration areas for Nigeria LSS from the "cover" module: 
nga_hh_info <- nga_hh_info %>% 
  left_join(read_csv("../MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/Household/secta_cover.csv") %>% 
              dplyr::select("hhid", "ea"), by = "hhid")


# For each household with a missing survey_wgt, find a household with the same ea
# that does have a survey_wgt, and use that survey_wgt to fill in the missing value:
for (i in which(is.na(nga_hh_info$survey_wgt))) {
  nga_hh_info$survey_wgt[i] <- nga_hh_info$survey_wgt[which(nga_hh_info$ea == nga_hh_info$ea[i] & !is.na(nga_hh_info$survey_wgt))][1]
}

sum(is.na(nga_hh_info$survey_wgt) == TRUE)

write.csv(nga_hh_info, paste0(path_to_save, "nga_lss1819_hh_info.csv"))
rm(nga_edu)
rm(nga_hh1)
rm(nga_hh_info)
rm(nga_roster)


### create machine learning targets as one file
mimi_targets <- target_creation()


write_csv(mimi_targets, paste0(path_to_save, "mimi_targets.csv"))

rm(list = ls())
