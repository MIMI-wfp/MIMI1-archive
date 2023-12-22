### make all countries with the same structure
### MIMI
### 12-12-2023

library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(haven)

path_to_data <- "~/Documents/MIMI/MIMI_data/"
path_to_save <- here::here("all_base_models/data/")

# nsso #########################################################################

nsso_food_consumption <- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_daily_consumption.csv"))

nsso_food_consumption <- nsso_food_consumption %>% 
  rename(
    hhid = HHID,
    item_code = Item_Code,
    quantity_g = Total_Consumption_Quantity,
    value = Total_Consumption_Value,
    food_group = hdds_groups
  ) %>% 
  select(hhid, item_code, quantity_g, value, food_group) %>% 
  mutate(quantity_100g = quantity_g/100)


write_csv(nsso_food_consumption, paste0(path_to_save,"nsso1112_food_consumption.csv"))


nsso_fct <- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_matched_fct.csv"))

nsso_fct <- nsso_fct %>%  
  
  rename(
    item_code = Item_Code,
    vita_rae_mcg = vita_mg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mg,
    fe_mg = iron_mg,
    ca_mg = calcium_mg,
    zn_mg = zinc_mg,
    na_mg = sodium_mg,
    thia_mg = vitb1_mg,
    ribo_mg = vitb2_mg,
    niac_mg = vitb3_mg,
  ) 


write_csv(nsso_fct, paste0(path_to_save,"nsso1112_fct.csv"))

nsso_afe<- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_afe.csv")) 

nsso_afe <- nsso_afe %>% 
  rename(hhid = HHID) %>% 
  select(hhid,
         afe)

write.csv(nsso_afe, paste0(path_to_save,"nsso1112_afe.csv" ))

rm(nsso_afe)
rm(nsso_basics)
rm(nsso_food_consumption)
rm(nsso_fct)
# rm(nsso_afe)


# Ethiopia #####################################################################
# HICES ------------------------------------------------------------------------

hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))

hices_food_consumption
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

write_csv(hices_food_consumption, paste0(path_to_save,"hices1516_food_consumption.csv"))

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

write_csv(hices_fct, paste0(path_to_save,"hices1516_fct.csv"))

hices_afe <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_afe.csv"))
write_csv(hices_afe, paste0(path_to_save,"hices1516_afe.csv"))

rm(hices_fct)
rm(hices_food_consumption)
rm(hices_afe)
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


write_csv(ess_food_consumption, paste0(path_to_save,"ess1819_food_consumption.csv"))

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

write_csv(ess_fct, paste0(path_to_save,"ess1819_fct.csv"))


ess_afe <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_hme.csv"))
ess_afe <- ess_afe %>% 
  select(HHID,afe) %>% 
  rename(hhid = HHID)

write_csv(ess_afe, paste0(path_to_save,"ess1819_afe.csv"))

rm(ess_fct)
rm(ess_food_consumption)
rm(ess_afe)

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


write_csv(mwi_food_consumption, paste0(path_to_save,"mwi1516_food_consumption.csv"))


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


write_csv(mwi_fct, paste0(path_to_save,"mwi1516_fct.csv"))

mwi_afe<- read.csv(paste0(path_to_data, "mwi/mwi_afe.csv"))
mwi_afe <- mwi_afe %>% 
  rename(hhid = HHID)

write_csv(mwi_afe, paste0(path_to_save,"mwi1516_afe.csv"))


rm(mwi_fct)
rm(mwi_food_consumption)

# nga ##########################################################################

nga_food_consumption<- read.csv(paste0(path_to_data, "nga/sect6b_food_cons_final.csv"))

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
as_tibble(nga_food_consumption)

write_csv(nga_food_consumption, paste0(path_to_save,"nga1819_food_consumption.csv"))


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
write_csv(nga_fct, paste0(path_to_save,"nga1819_fct.csv"))

nga_lss1_estimates <- haven::read_dta(paste0(path_to_data,"nga/NGA_LSS1819_estimates.dta"))


nga_afe <- nga_lss1_estimates %>% 
  select(hhid,
         hhafe) %>% 
  rename(afe = hhafe)

write_csv(nga_afe, paste0(path_to_save,"nga1819_afe.csv"))

rm(nga_fct)
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
nsso_basics <- read.csv("India_analysis/data/raw/block_1_2_identification.csv")

nsso_household_information <- read.csv("India_analysis/data/processed/household_char.csv")
nsso_demographics <- read.csv("India_analysis/data/processed/demographics.csv")
nsso_expenditure <- read.csv("India_analysis/data/raw/block_12_consumer_expenditure.csv")

nsso_household_information <- nsso_household_information %>% 
  inner_join(nsso_demographics, by = "HHID") %>% 
  filter(Relation == "Self") %>% 
  mutate(sex_head = Sex,
         age_head = Age,
         educ_head = Education,
         urbrur = HH_Type_code) %>% 
   select(HHID,
          urbrur,
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
  group_by(hhid) %>% 
  summarise(Value = sum(Value)) %>% 
  left_join(nsso_afe, by = "hhid") %>% 
  mutate(expenditure = Value/(afe*30)) %>% 
  ungroup() 
# %>% 
#   mutate(sep_quintile = ntile(expenditure, 5)) %>% 
#   group_by(urbrur) %>% 
#   mutate(ur_quintile = ntile(expenditure, 5)) %>% 
#   ungroup() 

nsso_household_information <- nsso_household_information %>% 
  left_join(nsso_basics, by = "hhid") %>% 
  left_join(nsso_expenditure, by = "hhid") %>% 
  mutate(sep_quintile = ntile(expenditure, 5)) %>% 
  group_by(urbrur) %>%
  mutate(ur_quintile = ntile(expenditure, 5)) %>%
  ungroup() %>% 
  select(-c(Value, expenditure,afe))

write_csv(nsso_household_information, paste0(path_to_save,"nsso1112_hh_info.csv"))

rm(nsso_household_information)
rm(nsso_basics)
rm(nsso_demographics)


# hices -----------------------------------------------------------------------

hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))
hices_rur_quintiles <- read.csv(here::here("ethiopia/data/urb_rur_quintiles.csv"))

unique(hices_food_consumption$CQ11)

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
    urbrur = UR,
    sex_head = SEX_Head,
    age_head = Age_Head,
    educ_head = EDUC_Head,
    survey_wgt = WGT
  ) %>% 
  group_by(hhid) %>% 
  slice(1) %>% 
  left_join(
    hices_rur_quintiles, by = "hhid"
  ) %>% 
  select(-c(UR, EXPCC)) %>% 
  rename(seq_quintile = quintile)

write.csv(hices_hh_info, paste0(path_to_save, "hices1516_hh_info.csv"))      

# ESS --------------------------------------------------------------------------

#  adm1 = saq01, adm2 = saq02, urbrur = saq14, hhid = household_id, date = InterviewStart
# head of household == 

ess1 <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect_cover_hh_w4.csv"))
ess2_roster <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect1_hh_w4.csv"))
ess3_educ <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect2_hh_w4.csv"))
ess_qunit<- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/cons_agg_w4.csv"))


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
    urbrur = saq14,
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
  group_by(urbrur) %>% 
  mutate(ur_quintile = ntile(spat_totcons_aeq, 5)) %>% 
  ungroup() %>% 
  select(-c(
    individual_id,
    InterviewStart,
    s1q01,
    spat_totcons_aeq,
    cons_quint
  ))
  

write.csv(ess_hh_info, paste0(path_to_save, "ess1819_hh_info.csv"))

## need SEP as well

# mwi --------------------------------------------------------------------------


mwi1_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_a_filt.csv"))
mwi2_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_b.csv"))
mwi3_hh <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_c.csv"))
mwi_sep_qunit <- read.csv("~/Documents/MIMI/MIMI_data/mwi/MWI_2016_IHS-IV_v04_M_CSV/consumption_aggregate/ihs4 consumption aggregate.csv")

mwi_hh_info <- mwi1_hh %>% 
  select(
    case_id, 
    region,
    district,
    reside,
    interviewDate,
    hh_wgt) %>% 
  left_join(
    mwi2_hh %>% 
      select(
        case_id,
        PID,
        hh_b03,#sex
        hh_b04,#relationship
        hh_b06b
        
      ),
    by = "case_id"
  ) %>% 
  left_join(mwi3_hh %>% 
              select(case_id,
                     PID,
                     hh_c09),
            by = c("case_id", "PID")
            ) %>% 
  rename(
    hhid = case_id,
    adm1 = region,
    adm2 = district,
    urbrur = reside,
    survey_wgt = hh_wgt
    
  ) %>% 
  filter(hh_b04 == 1) %>% 
  mutate(
    age_head = as.numeric(str_sub(interviewDate, 1,4)) - as.numeric(hh_b06b),
    sex_head = ifelse(hh_b03 == 1, "Male", "Female"),
    educ_head = hh_c09,
    urbrur = ifelse(urbrur == 1, "Urban", "Rural"),
    year = str_sub(interviewDate, 1,4),
    month = str_sub(interviewDate, 6,7)
  ) %>% 
  left_join(mwi_sep_qunit %>%  rename(hhid =case_id),
            by = "hhid") %>% 
  mutate(sep_quintile = ntile(rexpaggpc, 5)) %>% 
  group_by(urbrur) %>% 
  mutate(ur_quintile = ntile(rexpaggpc, 5)) %>% 
  select(
    hhid,
    adm1,
    adm2,
    urbrur,
    age_head,
    sex_head,
    educ_head,
    sep_quintile,
    ur_quintile,
    year, 
    month,
    survey_wgt
  )

# need sep!

write.csv(mwi_hh_info, paste0(path_to_save, "mwi1516_hh_info.csv"))
rm(mwi_hh_info)
rm(mwi1_hh)
rm(mwi2_hh)
rm(mwi3_hh)

# nga --------------------------------------------------------------------------

nga_hh1 <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/secta_cover.csv"))
nga_roster <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/sect1_roster.csv"))
nga_edu <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/sect2_education.csv"))
nga_consumption <- read.csv(paste0(path_to_data, "nga/NGA_2018_LSS_v01_M_CSV/household/totcons.csv"))

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
    urbrur = ifelse(sector == 1,"Urban", "Rural"),
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
  group_by(urbrur) %>% 
  mutate(ur_quintile = ntile(total_consumption, 5)) %>% 
  ungroup() %>% 
  select(
    hhid,
    adm1,
    adm2,
    urbrur,
    sep_quintile,
    ur_quintile,
    age_head,
    sex_head,
    educ_head,
    year, 
    month,
    survey_wgt
  ) 


write.csv(nga_hh_info, paste0(path_to_save, "nga1819_hh_info.csv"))
rm(nga_edu)
rm(nga_hh1)
rm(nga_hh_info)
rm(nga_roster)


