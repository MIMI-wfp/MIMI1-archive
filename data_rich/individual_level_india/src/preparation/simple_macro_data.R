# Script for creating SAS files for simpleMacro running
# takes in the output from simplemacro algorithm and puts 
# all the states and mns back together into a usable dataset


setwd("~/Documents/LSHTM/WFP_project/MIMI")
path_to_script <- "scripts/data_extraction/"
path_to_data <- "../IND_00062/"
source(paste0(path_to_script,"functions.R"))
source(paste0(path_to_script,"data_loading.R"))
library(boot)
library(gt)
## creates bigger titles for plots
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16)) 

#make a data set for the intake of all MNs
all_mn <- vit_a_population %>% ungroup() %>% select(c("SUBJECT", 
                                        "sum_VITA_RAE_mcg",
                                        "ROUND", 
                                        "HOUSEHOLD",
                                        "SEX",
                                        "PREG_LACT",
                                        "AGE_YEAR",
                                        "AGE_GROUP",
                                        "WEEK_DAY",
                                        "ADM1_NAME",
                                        "ADM2_NAME")) %>% 
  left_join(folate_population%>% ungroup()  %>% select(c("SUBJECT", 
                                           "sum_FOLDFE_mcg",
                                           "ROUND", 
                                           "HOUSEHOLD",
                                           "SEX",
                                           "PREG_LACT",
                                           "AGE_YEAR",
                                           "AGE_GROUP",
                                           "WEEK_DAY",
                                           "ADM1_NAME",
                                           "ADM2_NAME")), by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "PREG_LACT",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "WEEK_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(iron_population %>% ungroup() %>% select(c("SUBJECT", 
                                         "sum_IRON_mg",
                                         "ROUND", 
                                         "HOUSEHOLD",
                                         "SEX",
                                         "PREG_LACT",
                                         "AGE_YEAR",
                                         "AGE_GROUP",
                                         "WEEK_DAY",
                                         "ADM1_NAME",
                                         "ADM2_NAME")), by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "PREG_LACT",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "WEEK_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(zinc_population%>% ungroup() %>% select(c("SUBJECT", 
                                        "sum_ZINC_mg",
                                        "ROUND", 
                                        "HOUSEHOLD",
                                        "SEX",
                                        "PREG_LACT",
                                        "AGE_YEAR",
                                        "AGE_GROUP",
                                        "WEEK_DAY",
                                        "ADM1_NAME",
                                        "ADM2_NAME")), by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "PREG_LACT",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "WEEK_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(vit_b12_population %>% ungroup()  %>% select(c("SUBJECT", 
                                            "sum_VITB12_mcg",
                                            "ROUND", 
                                            "HOUSEHOLD",
                                            "SEX",
                                            "PREG_LACT",
                                            "AGE_YEAR",
                                            "AGE_GROUP",
                                            "WEEK_DAY",
                                            "ADM1_NAME",
                                            "ADM2_NAME")), by = c("SUBJECT", 
                                    "ROUND", 
                                    "HOUSEHOLD",
                                    "SEX",
                                    "PREG_LACT",
                                    "AGE_YEAR",
                                    "AGE_GROUP",
                                    "WEEK_DAY",
                                    "ADM1_NAME",
                                    "ADM2_NAME"))

all_mn %>% filter(ADM1_NAME == "Madhya_Pradesh" & SEX == "Male") %>% 
  ggplot(aes(x = sum_IRON_mg)) +
  geom_histogram()

create_state <- function(micronutrient, state_name) {
  #function to create datasets of adult men and women, split at adm2 level
  # by state, suitable for simpleMacro reading in..
  va_state_men <- micronutrient %>%
    ungroup() %>%
    filter(AGE_YEAR >= 18 & SEX == "Male") %>%
    filter(ADM1_NAME == state_name) %>%
    select(-c(ROUND, AGE_GROUP, ADM1_NAME)) %>%
    drop_na(ADM2_NAME) %>% 
    mutate(SEX = factor(ifelse(SEX == "Male", 1, 2))) %>%
    mutate(WEEKEND = ifelse(WEEK_DAY == 1 | WEEK_DAY == 7, 1, 0)) %>%
    mutate(
      adm2_ = as.numeric(factor(ADM2_NAME)),
      ADM2_NAME = paste0("adm2_", as.numeric(factor(ADM2_NAME)))
    ) %>%
    
    pivot_wider(
      names_from = ADM2_NAME,
      values_from = ADM2_NAME,
      values_fill = 0,
      values_fn = function(x) 1
    )
  
  va_state_women <- micronutrient %>%
    ungroup() %>%
    filter(AGE_YEAR >= 18 & SEX == "Female") %>%
    mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
    filter(PREG_LACT<=1) %>% 
    filter(ADM1_NAME == state_name) %>%
    select(-c(ROUND, AGE_GROUP, ADM1_NAME)) %>%
    drop_na(ADM2_NAME) %>% 
    mutate(SEX = factor(ifelse(SEX == "Male", 1, 2))) %>%
    mutate(WEEKEND = ifelse(WEEK_DAY == 1 | WEEK_DAY == 7, 1, 0)) %>%
    mutate(
      adm2_ = as.numeric(factor(ADM2_NAME)),
      ADM2_NAME = paste0("adm2_", as.numeric(factor(ADM2_NAME)))
    ) %>%
    
    pivot_wider(
      names_from = ADM2_NAME,
      values_from = ADM2_NAME,
      values_fill = 0,
      values_fn = function(x) 1
    ) 
  
  write_csv(va_state_men, paste0("outputs/SAS_data/va_", state_name, "_men_SAS.csv"),append = FALSE)
  write_csv(va_state_women, paste0("outputs/SAS_data/va_", state_name, "_women_SAS.csv"),append = FALSE)
  
}

# unique(vit_a_population$ADM1_NAME)
#create the dataframes

boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector])
}

va_Maharashtra <- create_state(all_mn,"Maharashtra")
va_West_Bengal <- create_state(all_mn,"West_Bengal")
va_Gujarat <- create_state(all_mn,"Gujarat")
va_Orissa <- create_state(all_mn,"Orissa")
va_Tamil_Nadu <- create_state(all_mn,"Tamil_Nadu")
va_Andhra_Pradesh <- create_state(all_mn,"Andhra_Pradesh")
va_Madhya_Pradesh <- create_state(all_mn,"Madhya_Pradesh" )
va_Kerala <- create_state(all_mn,"Kerala")
va_Karnataka<- create_state(all_mn,"Karnataka")
va_Uttar_Pradesh<- create_state(all_mn,"Uttar_Pradesh")

#energy
en_Maharashtra <- create_state(energy_population,"Maharashtra")
en_West_Bengal <- create_state(energy_population,"West_Bengal")
en_Gujarat <- create_state(energy_population,"Gujarat")
en_Orissa <- create_state(energy_population,"Orissa")
en_Tamil_Nadu <- create_state(energy_population,"Tamil_Nadu")
en_Andhra_Pradesh <- create_state(energy_population,"Andhra_Pradesh")
en_Madhya_Pradesh <- create_state(energy_population,"Madhya_Pradesh" )
en_Kerala <- create_state(energy_population,"Kerala")
en_Karnataka<- create_state(energy_population,"Karnataka")
en_Uttar_Pradesh<- create_state(energy_population,"Uttar_Pradesh")

# names(va_West_Bengal)
# sum(va_$sum_VITA_RAE_mcg == 0)
# sum(va_West_Bengal$WEEKEND)
# sum(va_Uttar_Pradesh$WEEKEND)
# 
# va_Maharashtra <- vit_a_population %>% 
#     ungroup() %>% 
#     filter(AGE_YEAR>=18) %>% 
#     filter(ADM1_NAME == "Maharashtra") %>% 
#     select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
#     mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
#     mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
#     mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
#            ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
#     pivot_wider(names_from = ADM2_NAME, 
#                 values_from = ADM2_NAME,
#                 values_fill = 0,
#                 values_fn = function(x) 1) 
#   
#   names(vitamin_a_SAS)
#     
#   write_csv(va_Maharashtra,"outputs/SAS_data/va_Maharashtra_SAS.csv")
# 
# va_Maharashtra <- state_sas(Maharashtra)
# 
# all_mn_men <- all_mn %>% 
#   ungroup() %>% 
#   filter(AGE_YEAR>=18 & SEX == "Male") %>% 
#   select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
#   # mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
#   mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
#   mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
#          ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
#   drop_na(ADM2_NAME) %>%
#   pivot_wider(names_from = ADM2_NAME, 
#               values_from = ADM2_NAME,
#               values_fill = 0,
#               values_fn = function(x) 1) 
# #str_replace(ADM2_NAME, " ", "_")
# write_csv(all_mn_men,"outputs/SAS_data/men_SAS.csv")
# 
# iron_SAS <- iron_population %>% 
#   ungroup() %>% 
#   filter(AGE_YEAR>=18) %>% 
#   select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
#   mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
#   mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
#   mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
#          ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
#   pivot_wider(names_from = ADM2_NAME, 
#               values_from = ADM2_NAME,
#               values_fill = 0,
#               values_fn = function(x) 1) 
# 
# write_csv(iron_SAS,"outputs/SAS_data/iron_SAS.csv")
# 
# zinc_SAS <- zinc_population %>% 
#   ungroup() %>% 
#   filter(AGE_YEAR>=18) %>% 
#   select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
#   mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
#   mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
#   mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
#          ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
#   pivot_wider(names_from = ADM2_NAME, 
#               values_from = ADM2_NAME,
#               values_fill = 0,
#               values_fn = function(x) 1) 
# 
# write_csv(zinc_SAS,"outputs/SAS_data/zinc_SAS.csv")


#####-----------------------------------------------------------

# read in the output data

# vita
path_to_data_sas <- "../data/usual_intake/"
AP_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_AP_va.xlsx"))
GU_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_GU_va.xlsx"))
KA_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_KA_va.xlsx"))
KE_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_KE_va.xlsx"))
MP_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_MP_va.xlsx"))
MA_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_MA_va.xlsx"))
OR_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_OR_va.xlsx"))
TN_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_TN_va.xlsx"))
UP_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_UP_va.xlsx"))
# WB_va_all <- read_excel(paste0(path_to_data_sas, "vit_a/all/final_WB_va.xlsx"))

#folate

AP_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_AP_fo.xlsx"))
GU_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_GU_fo.xlsx"))
KA_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_KA_fo.xlsx"))
KE_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_KE_fo.xlsx"))
MP_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_MP_fo.xlsx"))
MA_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_MA_fo.xlsx"))
OR_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_OR_fo.xlsx"))
TN_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_TN_fo.xlsx"))
UP_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_UP_fo.xlsx"))
WB_fo_all <- read_excel(paste0(path_to_data_sas, "folate/all/_final_WB_fo.xlsx"))

#iron

AP_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_AP_ir.xlsx"))
GU_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_GU_ir.xlsx"))
KA_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_KA_ir.xlsx"))
KE_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_KE_ir.xlsx"))
MP_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_MP_ir.xlsx"))
MA_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_MA_ir.xlsx"))
OR_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_OR_ir.xlsx"))
TN_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_TN_ir.xlsx"))
UP_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_UP_ir.xlsx"))
WB_ir_all <- read_excel(paste0(path_to_data_sas, "iron/all/_final_WB_ir.xlsx"))

#zinc

AP_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_AP_zn.xlsx"))
GU_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_GU_zn.xlsx"))
KA_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_KA_zn.xlsx"))
KE_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_KE_zn.xlsx"))
MP_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_MP_zn.xlsx"))
MA_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_MA_zn.xlsx"))
OR_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_OR_zn.xlsx"))
TN_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_TN_zn.xlsx"))
UP_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_UP_zn.xlsx"))
WB_zn_all <- read_excel(paste0(path_to_data_sas, "zinc/all/_final_WB_zn.xlsx"))



# create maps from numeric to name of adm2
adm2_name_map <- function(state_name){
  #maps the previous numeric factor back to the adm2 name for each state
AP_dict <-   vit_a_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(ADM1_NAME == state_name) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
         ADM2_NAME = factor(ADM2_NAME)) %>% 
  select(adm2_, ADM2_NAME) %>% 
  distinct(ADM2_NAME, adm2_)
}

MA_dict <- adm2_name_map("Maharashtra")
WB_dict <- adm2_name_map("West_Bengal")
GU_dict <- adm2_name_map("Gujarat")
OR_dict <- adm2_name_map("Orissa")
TN_dict <- adm2_name_map("Tamil_Nadu")
AP_dict <- adm2_name_map("Andhra_Pradesh")
MP_dict <- adm2_name_map("Madhya_Pradesh" )
KE_dict <- adm2_name_map("Kerala")
KA_dict<- adm2_name_map("Karnataka")
UP_dict<- adm2_name_map("Uttar_Pradesh")

convert_to_name <- function(df, dict){
  #converts the adm2 name from the SAS output and changes it back into name
  df %>% 
    filter(adm2_ != "Overall") %>%
    mutate(adm2_ = as.numeric(adm2_)) %>% 
    left_join(dict, by = "adm2_") %>% 
    select(!adm2_)
}

# convert adm2 number to adm2 name
AP_va_all <- convert_to_name(AP_va_all, AP_dict)
GU_va_all <- convert_to_name(GU_va_all, GU_dict)
KA_va_all <- convert_to_name(KA_va_all, KA_dict)
KE_va_all <- convert_to_name(KE_va_all, KE_dict)
MA_va_all <- convert_to_name(MA_va_all, MA_dict)
MP_va_all <- convert_to_name(MP_va_all, MP_dict)
OR_va_all <- convert_to_name(OR_va_all, OR_dict)
TN_va_all <- convert_to_name(TN_va_all, TN_dict)
UP_va_all <- convert_to_name(UP_va_all, UP_dict)

#combine all for a sinngle data frame
va_all <- bind_rows(AP_va_all, UP_va_all, GU_va_all, KA_va_all, KE_va_all, MA_va_all, MP_va_all, OR_va_all, TN_va_all)
# WB_va_all <- WB_va_all %>% 
#   filter(adm2_ != "Overall") %>% 
#   mutate(adm2_ = as.numeric(adm2_)) %>% 
#   left_join(WB_dict, by = "adm2_") 

AP_fo_all <- convert_to_name(AP_fo_all, AP_dict)
GU_fo_all <- convert_to_name(GU_fo_all, GU_dict)
KA_fo_all <- convert_to_name(KA_fo_all, KA_dict)
KE_fo_all <- convert_to_name(KE_fo_all, KE_dict)
MA_fo_all <- convert_to_name(MA_fo_all, MA_dict)
MP_fo_all <- convert_to_name(MP_fo_all, MP_dict)
OR_fo_all <- convert_to_name(OR_fo_all, OR_dict)
TN_fo_all <- convert_to_name(TN_fo_all, TN_dict)
UP_fo_all <- convert_to_name(UP_fo_all, UP_dict)
WB_fo_all <- convert_to_name(WB_fo_all, WB_dict)

fo_all <- bind_rows(AP_fo_all, UP_fo_all, GU_fo_all, KA_fo_all, KE_fo_all, MA_fo_all, MP_fo_all, OR_fo_all, TN_fo_all, WB_fo_all)


AP_ir_all <- convert_to_name(AP_ir_all, AP_dict)
GU_ir_all <- convert_to_name(GU_ir_all, GU_dict)
KA_ir_all <- convert_to_name(KA_ir_all, KA_dict)
KE_ir_all <- convert_to_name(KE_ir_all, KE_dict)
MA_ir_all <- convert_to_name(MA_ir_all, MA_dict)
MP_ir_all <- convert_to_name(MP_ir_all, MP_dict)
OR_ir_all <- convert_to_name(OR_ir_all, OR_dict)
TN_ir_all <- convert_to_name(TN_ir_all, TN_dict)
UP_ir_all <- convert_to_name(UP_ir_all, UP_dict)
WB_ir_all <- convert_to_name(WB_ir_all, WB_dict)

ir_all <- bind_rows(AP_ir_all, UP_ir_all, GU_ir_all, KA_ir_all, KE_ir_all, MA_ir_all, MP_ir_all, OR_ir_all, TN_ir_all, WB_ir_all)


AP_zn_all <- convert_to_name(AP_zn_all, AP_dict)
GU_zn_all <- convert_to_name(GU_zn_all, GU_dict)
KA_zn_all <- convert_to_name(KA_zn_all, KA_dict)
KE_zn_all <- convert_to_name(KE_zn_all, KE_dict)
MA_zn_all <- convert_to_name(MA_zn_all, MA_dict)
MP_zn_all <- convert_to_name(MP_zn_all, MP_dict)
OR_zn_all <- convert_to_name(OR_zn_all, OR_dict)
TN_zn_all <- convert_to_name(TN_zn_all, TN_dict)
UP_zn_all <- convert_to_name(UP_zn_all, UP_dict)
WB_zn_all <- convert_to_name(WB_zn_all, WB_dict)

zn_all<- bind_rows(AP_zn_all, UP_zn_all, GU_zn_all, KA_zn_all, KE_zn_all, MA_zn_all, MP_zn_all, OR_zn_all, TN_zn_all, WB_zn_all)

####### Men ##########

AP_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_AP_va_men.xlsx"))
GU_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_GU_va_men.xlsx"))
KA_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_KA_va_men.xlsx"))
KE_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_KE_va_men.xlsx"))
MP_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_MP_va_men.xlsx"))
MA_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_MA_va_men.xlsx"))
OR_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_OR_va_men.xlsx"))
TN_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_TN_va_men.xlsx"))
UP_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_UP_va_men.xlsx"))
WB_va_men <- read_excel(paste0(path_to_data_sas, "vit_a/men/_final_WB_va_men.xlsx"))

AP_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_AP_fo_men.xlsx"))
GU_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_GU_fo_men.xlsx"))
KA_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_KA_fo_men.xlsx"))
KE_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_KE_fo_men.xlsx"))
MP_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_MP_fo_men.xlsx"))
MA_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_MA_fo_men.xlsx"))
OR_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_OR_fo_men.xlsx"))
TN_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_TN_fo_men.xlsx"))
UP_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_UP_fo_men.xlsx"))
WB_fo_men <- read_excel(paste0(path_to_data_sas, "folate/men/_final_WB_fo_men.xlsx"))

AP_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_AP_ir_men.xlsx"))
GU_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_GU_ir_men.xlsx"))
KA_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_KA_ir_men.xlsx"))
KE_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_KE_ir_men.xlsx"))
MP_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_MP_ir_men.xlsx"))
MA_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_MA_ir_men.xlsx"))
OR_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_OR_ir_men.xlsx"))
TN_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_TN_ir_men.xlsx"))
UP_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_UP_ir_men.xlsx"))
WB_ir_men <- read_excel(paste0(path_to_data_sas, "iron/men/_final_WB_ir_men.xlsx"))

AP_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_AP_zn_men.xlsx"))
# AP_zn_men <- AP_zn_men%>% rename(note = SS)
GU_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_GU_zn_men.xlsx"))
KA_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_KA_zn_men.xlsx"))
KE_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_KE_zn_men.xlsx"))
MP_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_MP_zn_men.xlsx"))
MA_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_MA_zn_men.xlsx"))

OR_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_OR_zn_men.xlsx"))
TN_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_TN_zn_men.xlsx"))
UP_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_UP_zn_men.xlsx"))
WB_zn_men <- read_excel(paste0(path_to_data_sas, "zinc/men/_final_WB_zn_men.xlsx"))

#vitb12
GU_vb_men <- read_excel(paste0(path_to_data_sas, "vit_b12/men/_final_GU_vb_men.xlsx"))
#convert to anem

AP_va_men_st <-AP_va_men %>% filter(adm2_ == "Overall")
GU_va_men_st <-GU_va_men%>% filter(adm2_ == "Overall")
KA_va_men_st <-KA_va_men%>% filter(adm2_ == "Overall")
KE_va_men_st <-KE_va_men %>% filter(adm2_ == "Overall")
MA_va_men_st <- MA_va_men %>% filter(adm2_ == "Overall")
MP_va_men_st <- MP_va_men %>% filter(adm2_ == "Overall")
OR_va_men_st <- OR_va_men%>% filter(adm2_ == "Overall")
TN_va_men_st <- TN_va_men%>% filter(adm2_ == "Overall")
UP_va_men_st <- UP_va_men%>% filter(adm2_ == "Overall")
WB_va_men_st <- WB_va_men%>% filter(adm2_ == "Overall")

va_state_men <- bind_rows(AP_va_men_st,GU_va_men_st,KA_va_men_st, KE_va_men_st, MA_va_men_st,MP_va_men_st,OR_va_men_st, TN_va_men_st, UP_va_men_st,WB_va_men_st)

AP_va_men <- convert_to_name(AP_va_men, AP_dict)
GU_va_men <- convert_to_name(GU_va_men, GU_dict)
KA_va_men <- convert_to_name(KA_va_men, KA_dict)
KE_va_men <- convert_to_name(KE_va_men, KE_dict)
MA_va_men <- convert_to_name(MA_va_men, MA_dict)
MP_va_men <- convert_to_name(MP_va_men, MP_dict)
OR_va_men <- convert_to_name(OR_va_men, OR_dict)
TN_va_men <- convert_to_name(TN_va_men, TN_dict)
UP_va_men <- convert_to_name(UP_va_men, UP_dict)
WB_va_men <- convert_to_name(WB_va_men, WB_dict)
#combine all for a sinngle data frame
va_men <- bind_rows(AP_va_men, UP_va_men, GU_va_men, KA_va_men, KE_va_men, MA_va_men, MP_va_men, OR_va_men, TN_va_men, WB_va_men)
# WB_va_all <- WB_va_all %>% 
#   filter(adm2_ != "Overall") %>% 
#   mutate(adm2_ = as.numeric(adm2_)) %>% 
#   left_join(WB_dict, by = "adm2_") 


va_state_men %>% 
  select(!c("adm2_", "N", "No_replicates")) %>% 
  mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
         note = ifelse(note == "UP", "Uttar Pradesh",note),
         note = ifelse(note == "WB", "West Bengal",note)) %>% 
  rename(mean_men = mean,
         inad_men = inadequate_percent,
         median_men = median,
         p25_men = P_25,
         p75_men = P_75
         ) %>% 
  left_join((va_state_women %>% 
                select(!c("adm2_", "N", "No_replicates")) %>% 
                mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
                      note = ifelse(note == "UP", "Uttar Pradesh",note),
                      note = ifelse(note == "WB", "West Bengal",note)) %>% 
                rename(mean_women = mean,
                       inad_women = inadequate_percent,
                       median_women = median,
                       p25_women = P_25,
                       p75_women = P_75
                ) )) %>% 
  mutate(across(2:11, round, 2)) %>% 
  gt() |>
  tab_header(title = "Vitamin A - Usual intake",
             subtitle = paste0("All usual intake values in ", "\U03BC","g retinol activity equivalents"))|>
  tab_spanner(
    label = paste0("Men (EAR = 460", "\U03BC","g)"),
    columns = c(
      mean_men,
      inad_men,
      median_men,
       p25_men,
       p75_men
    )
  )|>
  tab_spanner(
    label =  paste0("Women (EAR = 390", "\U03BC","g)"),
    columns = c(
      mean_women,
      inad_women,
      median_women,
      p25_women,
      p75_women
    )
  ) |>
  cols_label(
    note = "State",
    mean_men = "Mean",
    inad_men = "Inadequate %",
    median_men = "Median",
    p25_men = "25%",
    p75_men = "75%",
    mean_women = "Mean",
    inad_women = "Inadequate %",
    median_women = "Median",
    p25_women = "25%",
    p75_women = "75%"
  )

AP_fo_men_st <-AP_fo_men %>% filter(adm2_ == "Overall")
GU_fo_men_st <-GU_fo_men%>% filter(adm2_ == "Overall")
KA_fo_men_st <-KA_fo_men%>% filter(adm2_ == "Overall")
KE_fo_men_st <-KE_fo_men %>% filter(adm2_ == "Overall")
KE_fo_men_st <- KE_fo_men_st %>% rename(note = S)
MA_fo_men_st <- MA_fo_men %>% filter(adm2_ == "Overall")
MP_fo_men_st <- MP_fo_men %>% filter(adm2_ == "Overall")
OR_fo_men_st <- OR_fo_men%>% filter(adm2_ == "Overall")
OR_fo_men_st <- OR_fo_men_st %>% rename(note = S)
TN_fo_men_st <- TN_fo_men%>% filter(adm2_ == "Overall")
UP_fo_men_st <- UP_fo_men%>% filter(adm2_ == "Overall")
WB_fo_men_st <- WB_fo_men%>% filter(adm2_ == "Overall")

fo_state_men <- bind_rows(AP_fo_men_st,GU_fo_men_st,KA_fo_men_st, KE_fo_men_st, MA_fo_men_st,MP_fo_men_st,OR_fo_men_st, TN_fo_men_st, UP_fo_men_st,WB_fo_men_st)


AP_fo_men <- convert_to_name(AP_fo_men, AP_dict)
GU_fo_men <- convert_to_name(GU_fo_men, GU_dict)
KA_fo_men <- convert_to_name(KA_fo_men, KA_dict)
KE_fo_men <- convert_to_name(KE_fo_men, KE_dict)
KE_fo_men <- KE_fo_men %>% rename(note = S)
MA_fo_men <- convert_to_name(MA_fo_men, MA_dict)
MP_fo_men <- convert_to_name(MP_fo_men, MP_dict)
OR_fo_men <- convert_to_name(OR_fo_men, OR_dict)
OR_fo_men <- OR_fo_men %>% rename(note = S)
TN_fo_men <- convert_to_name(TN_fo_men, TN_dict)
UP_fo_men <- convert_to_name(UP_fo_men, UP_dict)
WB_fo_men <- convert_to_name(WB_fo_men, WB_dict)

fo_men <- bind_rows(AP_fo_men, UP_fo_men, GU_fo_men, KA_fo_men, KE_fo_men, MA_fo_men, MP_fo_men, OR_fo_men, TN_fo_men, WB_fo_men)

AP_ir_men_st <-AP_ir_men %>% filter(adm2_ == "Overall")
GU_ir_men_st <-GU_ir_men%>% filter(adm2_ == "Overall")
KA_ir_men_st <-KA_ir_men%>% filter(adm2_ == "Overall")
KE_ir_men_st <-KE_ir_men %>% filter(adm2_ == "Overall")
MA_ir_men_st <- MA_ir_men %>% filter(adm2_ == "Overall")
MP_ir_men_st <- MP_ir_men %>% filter(adm2_ == "Overall")
OR_ir_men_st <- OR_ir_men%>% filter(adm2_ == "Overall")
TN_ir_men_st <- TN_ir_men%>% filter(adm2_ == "Overall")
UP_ir_men_st <- UP_ir_men%>% filter(adm2_ == "Overall")
WB_ir_men_st <- WB_ir_men%>% filter(adm2_ == "Overall")

ir_state_men <- bind_rows(AP_ir_men_st,GU_ir_men_st,KA_ir_men_st, KE_ir_men_st, MA_ir_men_st,MP_ir_men_st,OR_ir_men_st, TN_ir_men_st, UP_ir_men_st,WB_ir_men_st)


AP_ir_men <- convert_to_name(AP_ir_men, AP_dict)
GU_ir_men <- convert_to_name(GU_ir_men, GU_dict)
KA_ir_men <- convert_to_name(KA_ir_men, KA_dict)
KE_ir_men <- convert_to_name(KE_ir_men, KE_dict)
MA_ir_men <- convert_to_name(MA_ir_men, MA_dict)
MP_ir_men <- convert_to_name(MP_ir_men, MP_dict)
OR_ir_men <- convert_to_name(OR_ir_men, OR_dict)
TN_ir_men <- convert_to_name(TN_ir_men, TN_dict)
UP_ir_men <- convert_to_name(UP_ir_men, UP_dict)
WB_ir_men <- convert_to_name(WB_ir_men, WB_dict)

ir_men <- bind_rows(AP_ir_men, UP_ir_men, GU_ir_men, KA_ir_men, KE_ir_men, MA_ir_men, MP_ir_men, OR_ir_men, TN_ir_men, WB_ir_men)

AP_zn_men_st <-AP_zn_men %>% filter(adm2_ == "Overall")
GU_zn_men_st <-GU_zn_men%>% filter(adm2_ == "Overall")
KA_zn_men_st <-KA_zn_men%>% filter(adm2_ == "Overall")
KE_zn_men_st <-KE_zn_men %>% filter(adm2_ == "Overall")
MA_zn_men_st <- MA_zn_men %>% filter(adm2_ == "Overall")
MP_zn_men_st <- MP_zn_men %>% filter(adm2_ == "Overall")
OR_zn_men_st <- OR_zn_men%>% filter(adm2_ == "Overall")
TN_zn_men_st <- TN_zn_men%>% filter(adm2_ == "Overall")
UP_zn_men_st <- UP_zn_men%>% filter(adm2_ == "Overall")
WB_zn_men_st <- WB_zn_men%>% filter(adm2_ == "Overall")

zn_state_men <- bind_rows(AP_zn_men_st,GU_zn_men_st,KA_zn_men_st, KE_zn_men_st, MA_zn_men_st,MP_zn_men_st,OR_zn_men_st, TN_zn_men_st, UP_zn_men_st,WB_zn_men_st)


AP_zn_men <- convert_to_name(AP_zn_men, AP_dict)
GU_zn_men <- convert_to_name(GU_zn_men, GU_dict)
KA_zn_men <- convert_to_name(KA_zn_men, KA_dict)
KE_zn_men <- convert_to_name(KE_zn_men, KE_dict)
MA_zn_men <- convert_to_name(MA_zn_men, MA_dict)
MP_zn_men <- convert_to_name(MP_zn_men, MP_dict)
OR_zn_men <- convert_to_name(OR_zn_men, OR_dict)
TN_zn_men <- convert_to_name(TN_zn_men, TN_dict)
UP_zn_men <- convert_to_name(UP_zn_men, UP_dict)
WB_zn_men <- convert_to_name(WB_zn_men, WB_dict)

zn_men<- bind_rows(AP_zn_men, UP_zn_men, GU_zn_men, KA_zn_men, KE_zn_men, MA_zn_men, MP_zn_men, OR_zn_men, TN_zn_men, WB_zn_men)




vb12_men <- convert_to_name(GU_vb_men, GU_dict)
############ WOMEN ################

AP_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_AP_va_women.xlsx"))
GU_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_GU_va_women.xlsx"))
KA_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_KA_va_women.xlsx"))
KE_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_KE_va_women.xlsx"))
MP_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_MP_va_women.xlsx"))
MA_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_MA_va_women.xlsx"))
OR_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_OR_va_women.xlsx"))
TN_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_TN_va_women.xlsx"))
UP_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_UP_va_women.xlsx"))
# WB_va_women <- read_excel(paste0(path_to_data_sas, "vit_a/women/_final_WB_va_women.xlsx"))

AP_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_AP_fo_women.xlsx"))
GU_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_GU_fo_women.xlsx"))
KA_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_KA_fo_women.xlsx"))
KE_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_KE_fo_women.xlsx"))
MP_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_MP_fo_women.xlsx"))
MA_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_MA_fo_women.xlsx"))
OR_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_OR_fo_women.xlsx"))
TN_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_TN_fo_women.xlsx"))
UP_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_UP_fo_women.xlsx"))
# UP_fo_women <- UP_fo_women %>% rename(note = s)
WB_fo_women <- read_excel(paste0(path_to_data_sas, "folate/women/_final_WB_fo_women.xlsx"))

AP_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_AP_ir_women.xlsx"))
GU_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_GU_ir_women.xlsx"))
KA_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_KA_ir_women.xlsx"))
KE_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_KE_ir_women.xlsx"))
MP_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_MP_ir_women.xlsx"))
MA_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_MA_ir_women.xlsx"))
OR_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_OR_ir_women.xlsx"))
TN_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_TN_ir_women.xlsx"))
UP_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_UP_ir_women.xlsx"))
WB_ir_women <- read_excel(paste0(path_to_data_sas, "iron/women/_final_WB_ir_women.xlsx"))

AP_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_AP_zn_women.xlsx"))
GU_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_GU_zn_women.xlsx"))
KA_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_KA_zn_women.xlsx"))
KE_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_KE_zn_women.xlsx"))
MP_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_MP_zn_women.xlsx"))
MA_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_MA_zn_women.xlsx"))
OR_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_OR_zn_women.xlsx"))
TN_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_TN_zn_women.xlsx"))
UP_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_UP_zn_women.xlsx"))
WB_zn_women <- read_excel(paste0(path_to_data_sas, "zinc/women/_final_WB_zn_women.xlsx"))

#vitb12
GU_vb_women <- read_excel(paste0(path_to_data_sas, "vit_b12/women/_final_GU_vb_women.xlsx"))
#convert to anem

AP_va_women_st <-AP_va_women %>% filter(adm2_ == "Overall")
GU_va_women_st <-GU_va_women%>% filter(adm2_ == "Overall")
KA_va_women_st <-KA_va_women%>% filter(adm2_ == "Overall")
KE_va_women_st <-KE_va_women %>% filter(adm2_ == "Overall")
MA_va_women_st <- MA_va_women %>% filter(adm2_ == "Overall")
MP_va_women_st <- MP_va_women %>% filter(adm2_ == "Overall")
OR_va_women_st <- OR_va_women%>% filter(adm2_ == "Overall")
TN_va_women_st <- TN_va_women%>% filter(adm2_ == "Overall")
UP_va_women_st <- UP_va_women%>% filter(adm2_ == "Overall")
# WB_va_women_st <- WB_va_women%>% filter(adm2_ == "Overall")

va_state_women <- bind_rows(AP_va_women_st,GU_va_women_st,KA_va_women_st, KE_va_women_st, MA_va_women_st,MP_va_women_st,OR_va_women_st, TN_va_women_st, UP_va_women_st)#,WB_va_men_st)



AP_va_women <- convert_to_name(AP_va_women, AP_dict)
# AP_va_women <- AP_va_women%>% rename(note = s)
GU_va_women <- convert_to_name(GU_va_women, GU_dict)
KA_va_women <- convert_to_name(KA_va_women, KA_dict)
KE_va_women <- convert_to_name(KE_va_women, KE_dict)
MA_va_women <- convert_to_name(MA_va_women, MA_dict)
MP_va_women <- convert_to_name(MP_va_women, MP_dict)
OR_va_women <- convert_to_name(OR_va_women, OR_dict)
TN_va_women <- convert_to_name(TN_va_women, TN_dict)
UP_va_women <- convert_to_name(UP_va_women, UP_dict)
# WB_va_women <- convert_to_name(WB_va_women, WB_dict)
#combine all for a sinngle data frame
va_women <- bind_rows(AP_va_women, UP_va_women, GU_va_women, KA_va_women, KE_va_women, MA_va_women, MP_va_women, OR_va_women, TN_va_women)
# WB_va_all <- WB_va_all %>% 
#   filter(adm2_ != "Overall") %>% 
#   mutate(adm2_ = as.numeric(adm2_)) %>% 
#   left_join(WB_dict, by = "adm2_") 

AP_fo_women_st <-AP_fo_women %>% filter(adm2_ == "Overall")
GU_fo_women_st <-GU_fo_women%>% filter(adm2_ == "Overall")
KA_fo_women_st <-KA_fo_women%>% filter(adm2_ == "Overall")
KE_fo_women_st <-KE_fo_women %>% filter(adm2_ == "Overall")
MA_fo_women_st <- MA_fo_women %>% filter(adm2_ == "Overall")
MP_fo_women_st <- MP_fo_women %>% filter(adm2_ == "Overall")
OR_fo_women_st <- OR_fo_women%>% filter(adm2_ == "Overall")
TN_fo_women_st <- TN_fo_women%>% filter(adm2_ == "Overall")
UP_fo_women_st <- UP_fo_women%>% filter(adm2_ == "Overall")
WB_fo_women_st <- WB_fo_women%>% filter(adm2_ == "Overall")

fo_state_women <- bind_rows(AP_fo_women_st,GU_fo_women_st,KA_fo_women_st, KE_fo_women_st, MA_fo_women_st,MP_fo_women_st,OR_fo_women_st, TN_fo_women_st, UP_fo_women_st,WB_fo_men_st)



AP_fo_women <- convert_to_name(AP_fo_women, AP_dict)
GU_fo_women <- convert_to_name(GU_fo_women, GU_dict)
KA_fo_women <- convert_to_name(KA_fo_women, KA_dict)
KE_fo_women <- convert_to_name(KE_fo_women, KE_dict)
MA_fo_women <- convert_to_name(MA_fo_women, MA_dict)
MP_fo_women <- convert_to_name(MP_fo_women, MP_dict)
OR_fo_women <- convert_to_name(OR_fo_women, OR_dict)
TN_fo_women <- convert_to_name(TN_fo_women, TN_dict)
UP_fo_women <- convert_to_name(UP_fo_women, UP_dict)
WB_fo_women <- convert_to_name(WB_fo_women, WB_dict)

fo_women <- bind_rows(AP_fo_women, UP_fo_women, GU_fo_women, KA_fo_women, KE_fo_women, MA_fo_women, MP_fo_women, OR_fo_women, TN_fo_women, WB_fo_women)


AP_ir_women_st <-AP_ir_women %>% filter(adm2_ == "Overall")
GU_ir_women_st <-GU_ir_women%>% filter(adm2_ == "Overall")
KA_ir_women_st <-KA_ir_women%>% filter(adm2_ == "Overall")
KE_ir_women_st <-KE_ir_women %>% filter(adm2_ == "Overall")
MA_ir_women_st <- MA_ir_women %>% filter(adm2_ == "Overall")
MP_ir_women_st <- MP_ir_women %>% filter(adm2_ == "Overall")
OR_ir_women_st <- OR_ir_women%>% filter(adm2_ == "Overall")
TN_ir_women_st <- TN_ir_women%>% filter(adm2_ == "Overall")
UP_ir_women_st <- UP_ir_women%>% filter(adm2_ == "Overall")
WB_ir_women_st <- WB_ir_women%>% filter(adm2_ == "Overall")

ir_state_women <- bind_rows(AP_ir_women_st,GU_ir_women_st,KA_ir_women_st, KE_ir_women_st, MA_ir_women_st,MP_ir_women_st,OR_ir_women_st, TN_ir_women_st, UP_ir_women_st,WB_ir_men_st)





AP_ir_women <- convert_to_name(AP_ir_women, AP_dict)
GU_ir_women <- convert_to_name(GU_ir_women, GU_dict)
KA_ir_women <- convert_to_name(KA_ir_women, KA_dict)
KE_ir_women <- convert_to_name(KE_ir_women, KE_dict)
MA_ir_women <- convert_to_name(MA_ir_women, MA_dict)
MP_ir_women <- convert_to_name(MP_ir_women, MP_dict)
OR_ir_women <- convert_to_name(OR_ir_women, OR_dict)
TN_ir_women <- convert_to_name(TN_ir_women, TN_dict)
UP_ir_women <- convert_to_name(UP_ir_women, UP_dict)
WB_ir_women <- convert_to_name(WB_ir_women, WB_dict)

ir_women <- bind_rows(AP_ir_women, UP_ir_women, GU_ir_women, KA_ir_women, KE_ir_women, MA_ir_women, MP_ir_women, OR_ir_women, TN_ir_women, WB_ir_women)


AP_zn_women_st <-AP_zn_women %>% filter(adm2_ == "Overall")
GU_zn_women_st <-GU_zn_women%>% filter(adm2_ == "Overall")
KA_zn_women_st <-KA_zn_women%>% filter(adm2_ == "Overall")
KE_zn_women_st <-KE_zn_women %>% filter(adm2_ == "Overall")
MA_zn_women_st <- MA_zn_women %>% filter(adm2_ == "Overall")
MP_zn_women_st <- MP_zn_women %>% filter(adm2_ == "Overall")
OR_zn_women_st <- OR_zn_women%>% filter(adm2_ == "Overall")
TN_zn_women_st <- TN_zn_women%>% filter(adm2_ == "Overall")
UP_zn_women_st <- UP_zn_women%>% filter(adm2_ == "Overall")
WB_zn_women_st <- WB_zn_women%>% filter(adm2_ == "Overall")

zn_state_women <- bind_rows(AP_zn_women_st,GU_zn_women_st,KA_zn_women_st, KE_zn_women_st, MA_zn_women_st,MP_zn_women_st,OR_zn_women_st, TN_zn_women_st, UP_zn_women_st,WB_zn_men_st)




AP_zn_women <- convert_to_name(AP_zn_women, AP_dict)
GU_zn_women <- convert_to_name(GU_zn_women, GU_dict)
KA_zn_women <- convert_to_name(KA_zn_women, KA_dict)
KE_zn_women <- convert_to_name(KE_zn_women, KE_dict)
MA_zn_women <- convert_to_name(MA_zn_women, MA_dict)
MP_zn_women <- convert_to_name(MP_zn_women, MP_dict)
OR_zn_women <- convert_to_name(OR_zn_women, OR_dict)
TN_zn_women <- convert_to_name(TN_zn_women, TN_dict)
UP_zn_women <- convert_to_name(UP_zn_women, UP_dict)
WB_zn_women <- convert_to_name(WB_zn_women, WB_dict)

zn_women<- bind_rows(AP_zn_women, UP_zn_women, GU_zn_women, KA_zn_women, KE_zn_women, MA_zn_women, MP_zn_women, OR_zn_women, TN_zn_women, WB_zn_women)

va_state_men %>% 
  select(!c("adm2_", "N", "No_replicates")) %>% 
  mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
         note = ifelse(note == "UP", "Uttar Pradesh",note),
         note = ifelse(note == "WB", "West Bengal",note)) %>% 
  rename(mean_men = mean,
         inad_men = inadequate_percent,
         median_men = median,
         p25_men = P_25,
         p75_men = P_75
  ) %>% 
  left_join((va_state_women %>% 
               select(!c("adm2_", "N", "No_replicates")) %>% 
               mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
                      note = ifelse(note == "UP", "Uttar Pradesh",note),
                      note = ifelse(note == "WB", "West Bengal",note)) %>% 
               rename(mean_women = mean,
                      inad_women = inadequate_percent,
                      median_women = median,
                      p25_women = P_25,
                      p75_women = P_75
               ) )) %>% 
  mutate(across(2:11, round, 2)) %>% 
  gt() |>
  tab_header(title = "Vitamin A - Usual intake",
             subtitle = paste0("All usual intake values in ", "\U03BC","g retinol activity equivalents"))|>
  tab_spanner(
    label = paste0("Men (EAR = 460", "\U03BC","g)"),
    columns = c(
      mean_men,
      inad_men,
      median_men,
      p25_men,
      p75_men
    )
  )|>
  tab_spanner(
    label =  paste0("Women (EAR = 390", "\U03BC","g)"),
    columns = c(
      mean_women,
      inad_women,
      median_women,
      p25_women,
      p75_women
    )
  ) |>
  cols_label(
    note = "State",
    mean_men = "Mean",
    inad_men = "Inadequate %",
    median_men = "Median",
    p25_men = "25%",
    p75_men = "75%",
    mean_women = "Mean",
    inad_women = "Inadequate %",
    median_women = "Median",
    p25_women = "25%",
    p75_women = "75%"
  )


ir_state_men %>% 
  select(!c("adm2_", "N", "No_replicates")) %>% 
  mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
         note = ifelse(note == "UP", "Uttar Pradesh",note),
         note = ifelse(note == "WB", "West Bengal",note)) %>% 
  rename(mean_men = mean,
         inad_men = inadequate_percent,
         median_men = median,
         p25_men = P_25,
         p75_men = P_75
  ) %>% 
  left_join((ir_state_women %>% 
               select(!c("adm2_", "N", "No_replicates")) %>% 
               mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
                      note = ifelse(note == "UP", "Uttar Pradesh",note),
                      note = ifelse(note == "WB", "West Bengal",note)) %>% 
               rename(mean_women = mean,
                      inad_women = inadequate_percent,
                      median_women = median,
                      p25_women = P_25,
                      p75_women = P_75
               ) )) %>% 
  mutate(across(2:11, round, 2)) %>% 
  gt() |>
  tab_header(title = "Iron - Usual intake",
             subtitle = paste0("All usual intake values in mg"))|>
  tab_spanner(
    label = paste0("Men (EAR = 11 mg)"),
    columns = c(
      mean_men,
      inad_men,
      median_men,
      p25_men,
      p75_men
    )
  )|>
  tab_spanner(
    label =  paste0("Women (EAR = 15 mg)"),
    columns = c(
      mean_women,
      inad_women,
      median_women,
      p25_women,
      p75_women
    )
  ) |>
  cols_label(
    note = "State",
    mean_men = "Mean",
    inad_men = "Inadequate %",
    median_men = "Median",
    p25_men = "25%",
    p75_men = "75%",
    mean_women = "Mean",
    inad_women = "Inadequate %",
    median_women = "Median",
    p25_women = "25%",
    p75_women = "75%"
  )


fo_state_men %>% 
  select(!c("adm2_", "N", "No_replicates")) %>% 
  mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
         note = ifelse(note == "UP", "Uttar Pradesh",note),
         note = ifelse(note == "WB", "West Bengal",note)) %>% 
  rename(mean_men = mean,
         inad_men = inadequate_percent,
         median_men = median,
         p25_men = P_25,
         p75_men = P_75
  ) %>% 
  left_join((fo_state_women %>% 
               select(!c("adm2_", "N", "No_replicates")) %>% 
               mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
                      note = ifelse(note == "UP", "Uttar Pradesh",note),
                      note = ifelse(note == "WB", "West Bengal",note)) %>% 
               rename(mean_women = mean,
                      inad_women = inadequate_percent,
                      median_women = median,
                      p25_women = P_25,
                      p75_women = P_75
               ) )) %>% 
  mutate(across(2:11, round, 2)) %>% 
  gt() |>
  tab_header(title = "Folate - Usual intake",
             subtitle = paste0("All usual intake values in ", "\U03BC","g"))|>
  tab_spanner(
    label = paste0("Men (EAR = 250", "\U03BC","g)"),
    columns = c(
      mean_men,
      inad_men,
      median_men,
      p25_men,
      p75_men
    )
  )|>
  tab_spanner(
    label =  paste0("Women (EAR = 250", "\U03BC","g)"),
    columns = c(
      mean_women,
      inad_women,
      median_women,
      p25_women,
      p75_women
    )
  ) |>
  cols_label(
    note = "State",
    mean_men = "Mean",
    inad_men = "Inadequate %",
    median_men = "Median",
    p25_men = "25%",
    p75_men = "75%",
    mean_women = "Mean",
    inad_women = "Inadequate %",
    median_women = "Median",
    p25_women = "25%",
    p75_women = "75%"
  )

zn_state_men %>% 
  select(!c("adm2_", "N", "No_replicates")) %>% 
  mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
         note = ifelse(note == "UP", "Uttar Pradesh",note),
         note = ifelse(note == "WB", "West Bengal",note)) %>% 
  rename(mean_men = mean,
         inad_men = inadequate_percent,
         median_men = median,
         p25_men = P_25,
         p75_men = P_75
  ) %>% 
  left_join((zn_state_women %>% 
               select(!c("adm2_", "N", "No_replicates")) %>% 
               mutate(note = ifelse(note == "AP", "Andhra Pradesh",note),
                      note = ifelse(note == "UP", "Uttar Pradesh",note),
                      note = ifelse(note == "WB", "West Bengal",note)) %>% 
               rename(mean_women = mean,
                      inad_women = inadequate_percent,
                      median_women = median,
                      p25_women = P_25,
                      p75_women = P_75
               ) )) %>% 
  mutate(across(2:11, round, 2)) %>% 
  gt() |>
  tab_header(title = "Zinc - Usual intake",
             subtitle = paste0("All usual intake values in mg"))|>
  tab_spanner(
    label = paste0("Men (EAR = 14 mg)"),
    columns = c(
      mean_men,
      inad_men,
      median_men,
      p25_men,
      p75_men
    )
  )|>
  tab_spanner(
    label =  paste0("Women (EAR = 11 mg)"),
    columns = c(
      mean_women,
      inad_women,
      median_women,
      p25_women,
      p75_women
    )
  ) |>
  cols_label(
    note = "State",
    mean_men = "Mean",
    inad_men = "Inadequate %",
    median_men = "Median",
    p25_men = "25%",
    p75_men = "75%",
    mean_women = "Mean",
    inad_women = "Inadequate %",
    median_women = "Median",
    p25_women = "25%",
    p75_women = "75%"
  )



vb12_women <- convert_to_name(GU_vb_women, GU_dict)
###### convert to spatial data

india_adm2 <- st_read(paste0(path_to_data, "shape_files/original_country/clean_india_adm2.shp"))
india_adm2 <- india_adm2 %>% 
  ms_simplify(keep  =0.1, keep_shapes = T, snap = T) %>% 
  rename("ADM2_NAME" = shapeName)

va_all_sp <- va_all %>% 
  left_join(india_adm2,by = "ADM2_NAME")

fo_all_sp<- fo_all %>% 
  left_join(india_adm2 ,by = "ADM2_NAME")

ir_all_sp<- ir_all %>% 
  left_join(india_adm2 ,by = "ADM2_NAME")

zn_all_sp<- zn_all %>% 
  left_join(india_adm2 ,by = "ADM2_NAME")

va_men_sp <- va_men %>% 
  left_join((india_adm2),by = "ADM2_NAME")

fo_men_sp<- fo_men %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

ir_men_sp<- ir_men %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

zn_men_sp<- zn_men %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

va_women_sp<- va_women %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

fo_women_sp<- fo_women %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

ir_women_sp<- ir_women %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

zn_women_sp<- zn_women %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

vb12_women_sp <- vb12_women %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

vb12_men_sp <- vb12_men %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")

# write the shape files

st_write(va_all_sp, paste0(path_to_data, "shape_files/usual_intake/va_all.shp"), append = TRUE)
st_write(ir_all_sp, paste0(path_to_data, "shape_files/usual_intake/ir_all.shp"), append = TRUE)
st_write(fo_all_sp, paste0(path_to_data, "shape_files/usual_intake/fo_all.shp"), append = TRUE)
st_write(zn_all_sp, paste0(path_to_data, "shape_files/usual_intake/zn_all.shp"), append = TRUE)
st_write(va_men_sp, paste0(path_to_data, "shape_files/usual_intake/va_men.shp"), append = TRUE)
st_write(fo_men_sp, paste0(path_to_data, "shape_files/usual_intake/fo_men.shp"), append = TRUE)
st_write(ir_men_sp, paste0(path_to_data, "shape_files/usual_intake/ir_men.shp"), append = TRUE)
st_write(zn_men_sp, paste0(path_to_data, "shape_files/usual_intake/zn_men.shp"), append = TRUE)
st_write(va_women_sp, paste0(path_to_data, "shape_files/usual_intake/va_women.shp"), append = TRUE)
st_write(fo_women_sp, paste0(path_to_data, "shape_files/usual_intake/fo_women.shp"), append = TRUE)
st_write(ir_women_sp, paste0(path_to_data, "shape_files/usual_intake/ir_women.shp"), append = TRUE)
st_write(zn_women_sp, paste0(path_to_data, "shape_files/usual_intake/zn_women.shp"), append = TRUE)
st_write(vb12_women_sp, paste0(path_to_data, "shape_files/usual_intake/vb_women.shp"), append = TRUE)
st_write(vb12_men_sp, paste0(path_to_data, "shape_files/usual_intake/vb_men.shp"), append = TRUE)


#calculating usual difference
ir_usual_hist <-ir_women %>% 
  rename(usual_women = mean) %>% 
  select(usual_women, ADM2_NAME) %>% 
  inner_join((ir_men %>% 
               rename(usual_men = mean) %>% 
               select(usual_men, ADM2_NAME)
              ), by = "ADM2_NAME") %>% 
  mutate(diff = usual_men - usual_women) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, alpha = 1, position = 'dodge',
                  bins= 45) +
  theme_ipsum() +
  annotate("segment", x = 0, y = 40, xend = 5, yend = 40,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text",x = 0, y = 45, label = "Men have higher intake" )+
  labs(title = "Iron",
       x = "Usual intake difference (mg)", 
       y = "count")+ 
  xlim(-20,20)+
  My_Theme

#calculating usual difference
fo_usual_hist <- fo_women %>% 
  rename(usual_women = mean) %>% 
  select(usual_women, ADM2_NAME) %>% 
  inner_join((fo_men %>% 
                rename(usual_men = mean) %>% 
                select(usual_men, ADM2_NAME)
  ), by = "ADM2_NAME") %>% 
  mutate(diff = usual_men - usual_women) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, alpha = 1, position = 'dodge',
                  bins = 30) +
  theme_ipsum() +
  labs(title = "Folate",
       x = "Usual intake difference (mcg)", 
       y = "count")+ 
  annotate("segment", x = 0, y = 45, xend = 50, yend = 45,
  arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text",x = 0, y = 49, label = "Men have higher intake" )+
  xlim(-250,250)+
  My_Theme

va_usual_hist <- va_women %>% 
  rename(usual_women = mean) %>% 
  select(usual_women, ADM2_NAME) %>% 
  inner_join((va_men %>% 
                rename(usual_men = mean) %>% 
                select(usual_men, ADM2_NAME)
  ), by = "ADM2_NAME") %>% 
  mutate(diff = usual_men - usual_women) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, alpha = 1, position = 'dodge') +
  theme_ipsum() +
  annotate("segment", x = 0, y = 27, xend = 40, yend = 27,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text",x = 0, y = 30, label = "Men have higher intake" )+
  labs(title = "Vitamin A",
       x = "Usual intake difference (RAE mcg)", 
       y = "count")+ 
  xlim(-200,200)+
  My_Theme

zn_usual_hist <- zn_women %>% 
  rename(usual_women = mean) %>% 
  select(usual_women, ADM2_NAME) %>% 
  inner_join((zn_men %>% 
                rename(usual_men = mean) %>% 
                select(usual_men, ADM2_NAME)
  ), by = "ADM2_NAME") %>% 
  mutate(diff = usual_men - usual_women) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, alpha = 1, position = 'dodge') +
  theme_ipsum() +
  annotate("segment", x = 0, y = 40, xend = 2.5, yend = 40,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text",x = 0, y = 45, label = "Men have higher intake" )+
  labs(title = "Zinc",
       x = "Usual intake difference (mg)", 
       y = "count")   + 
  
  xlim(-10,10)+
  My_Theme          
  
vb_usual_hist <- vb12_women %>% 
  rename(usual_women = mean) %>% 
  select(usual_women, ADM2_NAME) %>% 
  inner_join((vb12_men %>% 
                rename(usual_men = mean) %>% 
                select(usual_men, ADM2_NAME)
  ), by = "ADM2_NAME") %>% 
  mutate(diff = usual_men - usual_women) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Vitamin B12",
       x = "Usual intake difference (mcg)", 
       y = "count") + 
  My_Theme

figure1 <- ggarrange(va_usual_hist, fo_usual_hist,  ir_usual_hist, zn_usual_hist,  
          ncol = 2, nrow = 2)

annotate_figure(figure1,
                top = text_grob("Difference in usual intake (Men-Women) at ADM2 level", color = "#404080", face = "bold", size = 14),
                bottom = text_grob("Population level usual intake calculated at ADM2 level ", color = "#69b3a2",
                                   hjust = 1, x = 1, face = "italic", size = 10),

                
                
)

#### differene in usual intake map 


ir_usual_sp <-ir_women %>% 
  rename(inad_women = inadequate_percent) %>% 
  select(inad_women, ADM2_NAME) %>% 
  inner_join((ir_men %>% 
                rename(inad_men = inadequate_percent) %>% 
                select(inad_men, ADM2_NAME)
  ), by = "ADM2_NAME")  %>%
  mutate(diff = inad_women - inad_men) %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")
st_write(ir_usual_sp, paste0(path_to_data, "shape_files/usual_intake/ir_usual_diff.shp"), append = TRUE)

fo_usual_sp <-fo_women %>% 
  rename(inad_women = inadequate_percent) %>% 
  select(inad_women, ADM2_NAME) %>% 
  inner_join((fo_men %>% 
                rename(inad_men = inadequate_percent) %>% 
                select(inad_men, ADM2_NAME)
  ), by = "ADM2_NAME")  %>%
  mutate(diff = inad_women - inad_men) %>% 
  left_join((india_adm2 
             ),by = "ADM2_NAME")
  
st_write(fo_usual_sp, paste0(path_to_data, "shape_files/usual_intake/fo_usual_diff.shp"), append = TRUE)

va_usual_sp <-va_women %>% 
  rename(inad_women = inadequate_percent) %>% 
  select(inad_women, ADM2_NAME) %>% 
  inner_join((va_men %>% 
                rename(inad_men = inadequate_percent) %>% 
                select(inad_men, ADM2_NAME)
  ), by = "ADM2_NAME")  %>%
  mutate(diff = inad_women - inad_men) %>% 
  left_join((india_adm2),by = "ADM2_NAME")
st_write(va_usual_sp, paste0(path_to_data, "shape_files/usual_intake/va_usual_diff.shp"), append = TRUE)

zn_usual_sp <-zn_women %>% 
  rename(inad_women = inadequate_percent) %>% 
  select(inad_women, ADM2_NAME) %>% 
  inner_join((zn_men %>% 
                rename(inad_men = inadequate_percent) %>% 
                select(inad_men, ADM2_NAME)
  ), by = "ADM2_NAME")  %>%
  mutate(diff = inad_women - inad_men) %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")
st_write(zn_usual_sp, paste0(path_to_data, "shape_files/usual_intake/zn_usual_diff.shp"), append = TRUE)
  
vb12_usual_sp <-vb12_women %>% 
  rename(inad_women = inadequate_percent) %>% 
  select(inad_women, ADM2_NAME) %>% 
  inner_join((vb12_men %>% 
                rename(inad_men = inadequate_percent) %>% 
                select(inad_men, ADM2_NAME)
  ), by = "ADM2_NAME")  %>%
  mutate(diff = inad_women - inad_men) %>% 
  left_join((india_adm2 ),by = "ADM2_NAME")
st_write(vb12_usual_sp, paste0(path_to_data, "shape_files/usual_intake/vb12_usual_diff.shp"), append = TRUE)



with(vit_a_household, t.test(SUM_MALE, SUM_FEMALE)) #not a significant difference 
with(folate_household, t.test(SUM_MALE, SUM_FEMALE)) 
with(iron_household, t.test(SUM_MALE, SUM_FEMALE))
with(zinc_household, t.test(SUM_MALE, SUM_FEMALE))


path_to_datasets <- "datasets/simple_macro_output/"

#save datasets

save(vb12_women,  file = paste0(path_to_datasets, "vb12_women.RData"))
save(va_women,file = paste0(path_to_datasets, "va_women.RData"))
save(fo_women,file = paste0(path_to_datasets, "fo_women.RData"))
save(ir_women,file = paste0(path_to_datasets, "ir_women.RData"))
save(zn_women, file = paste0(path_to_datasets, "zn_women.RData"))



save(vb12_men,  file = paste0(path_to_datasets, "vb12_men.RData"))
save(va_men,file = paste0(path_to_datasets, "va_men.RData"))
save(fo_men,file = paste0(path_to_datasets, "fo_men.RData"))
save(ir_men,file = paste0(path_to_datasets, "ir_men.RData"))
save(zn_men, file = paste0(path_to_datasets, "zn_men.RData"))



#--------------------------------

# Energy


AP_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_AP_en_women.xlsx"))
GU_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_GU_en_women.xlsx"))
GU_en_women <- GU_en_women %>% rename(note = S)
KA_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_KA_en_women.xlsx"))
KE_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_KE_en_women.xlsx"))
MP_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_MP_en_women.xlsx"))
MA_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_MA_en_women.xlsx"))
OR_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_OR_en_women.xlsx"))
TN_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_TN_en_women.xlsx"))
UP_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_UP_en_women.xlsx"))
WB_en_women <- read_excel(paste0(path_to_data_sas, "energy/_final_WB_en_women.xlsx"))


#convert to anem

AP_en_women <- convert_to_name(AP_en_women, AP_dict)
# AP_va_women <- AP_va_women%>% rename(note = s)
GU_en_women <- convert_to_name(GU_en_women, GU_dict)
KA_en_women <- convert_to_name(KA_en_women, KA_dict)
KE_en_women <- convert_to_name(KE_en_women, KE_dict)
MA_en_women <- convert_to_name(MA_en_women, MA_dict)
MP_en_women <- convert_to_name(MP_en_women, MP_dict)
OR_en_women <- convert_to_name(OR_en_women, OR_dict)
TN_en_women <- convert_to_name(TN_en_women, TN_dict)
UP_en_women <- convert_to_name(UP_en_women, UP_dict)
WB_en_women <- convert_to_name(WB_en_women, WB_dict)
#combine all for a sinngle data frame
en_women <- bind_rows(AP_en_women, UP_en_women, GU_en_women, KA_en_women, KE_en_women, MA_en_women, MP_en_women, OR_en_women, TN_en_women, WB_en_women)


AP_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_AP_en_men.xlsx"))
GU_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_GU_en_men.xlsx"))
# GU_en_men <- GU_en_women %>% rename(note = S)
KA_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_KA_en_men.xlsx"))
KE_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_KE_en_men.xlsx"))
MP_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_MP_en_men.xlsx"))
MA_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_MA_en_men.xlsx"))
OR_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_OR_en_men.xlsx"))
TN_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_TN_en_men.xlsx"))
UP_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_UP_en_men.xlsx"))
UP_en_men <- UP_en_men %>% rename(note =  S)
WB_en_men <- read_excel(paste0(path_to_data_sas, "energy/_final_WB_en_men.xlsx"))


#convert to anem

AP_en_men <- convert_to_name(AP_en_men, AP_dict)
# AP_vwomen <- AP_va_women%>% rename(note = s)
GU_en_men <- convert_to_name(GU_en_men, GU_dict)
KA_en_men <- convert_to_name(KA_en_men, KA_dict)
KE_en_men <- convert_to_name(KE_en_men, KE_dict)
MA_en_men <- convert_to_name(MA_en_men, MA_dict)
MP_en_men <- convert_to_name(MP_en_men, MP_dict)
OR_en_men <- convert_to_name(OR_en_men, OR_dict)
TN_en_men <- convert_to_name(TN_en_men, TN_dict)
UP_en_men <- convert_to_name(UP_en_men, UP_dict)
WB_en_men <- convert_to_name(WB_en_men, WB_dict)
#combine all for a sinngle data frame
en_men <- bind_rows(AP_en_men, UP_en_men, GU_en_men, KA_en_men, KE_en_men, MA_en_men, MP_en_men, OR_en_men, TN_en_men, WB_en_men)

save(en_men, file = paste0(path_to_datasets, "en_men.RData"))
save(en_women, file = paste0(path_to_datasets, "en_women.RData"))
