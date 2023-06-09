#### Gabriel Battcock
#### Data extraction from NIN dataset to machine learning dataframe
####
#### Extracts data from the NIN dataset, calculates the difference in mean MN intake 
#### between the head male and head female of the household. This is matched with binary data
#### of food group intake for the DQQ, GDQD and MDD. 
setwd("~/Documents/LSHTM/WFP_project/MIMI")

path_to_script <- "scripts/data_extraction/"
path_to_data <- "../IND_00062/"
source(paste0(path_to_script,"functions.R"))

# read in all the data files 
consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))
food_dict <- read_excel(paste0(path_to_data, "food_groups/FoodEx2_Exposure_dict.xlsx"))
code_list <- read_csv(paste0(path_to_data, "code_lists.csv"))
DQQ <- read_csv(paste0(path_to_data, "food_groups/DQQ_library.csv"))
GDQS <- read_csv(paste0(path_to_data, "food_groups/GDQS_library.csv"))
MDD <- read_csv(paste0(path_to_data, "food_groups/MDD_library.csv"))
vb12_dict <- read_csv(paste0(path_to_data, "dictionaries/vb12_dict.csv"))

#shape files
india_adm2 <- st_read(paste0(path_to_data, "shape_files/original_country/clean_india_adm2.shp"))
india_adm2 <- india_adm2 %>% 
  ms_simplify(keep  =0.1, keep_shapes = T, snap = T)
plot(india_adm2$geometry)

  
###### create vb12 variable    - can change if not correct
# consumption <- consumption %>% inner_join(vb12_dict, by = "FOODEX2_INGR_CODE") %>%
#   mutate(VITB12_mcg = (vitaminb12_in_mg*FOOD_AMOUNT_REPORTED)/100)
# write_csv(consumption, paste0(path_to_data, "consumption_user.csv"))

###### Constant variables to be used ###############

# Using EAR from the NIN to calculate minimum requirements chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7231601/pdf/nmz096.pdf

vita_EAR_men_mcg <- 570
vita_EAR_women_mcg <- 490
folate_EAR_men_mcg  <-  250
folate_EAR_women_mcg <- 250
iron_EAR_men_mg <- 19.2
iron_EAR_women_mg <- 22.4
zinc_EAR_men_mg <- 11
zinc_EAR_women_mg <- 8.9
vitb12_EAR_men_mcg <- 2
vitb12_EAR_women_mcg <- 2

# upper limit taken from Chan. Public Health  <- https://www.hsph.harvard.edu/nutritionsource
iron_UL_mg <- 45
zinc_UL_mg <- 40
folate_UL_mcg <-  1000
vita_UL_mcg <- 3000

# create the initial datasets

#create a joined user and consumption
joined <- full_join(user, consumption, by = c("SUBJECT","ROUND")) 
#micronutrients
vitamin_A_calc <- DIFF_HEAD_OF_HOUSE(joined, VITA_RAE_mcg)
folate_calc <- DIFF_HEAD_OF_HOUSE(joined, FOLDFE_mcg)
iron_calc <- DIFF_HEAD_OF_HOUSE(joined, IRON_mg)
zinc_calc <- DIFF_HEAD_OF_HOUSE(joined, ZINC_mg)

joined$VITB12_mcg

#difference per household
vit_a_household <- DIFF_HOUSEHOLD(joined, VITA_RAE_mcg)
folate_household <- DIFF_HOUSEHOLD(joined, FOLDFE_mcg)
vit_b12_household <- DIFF_HOUSEHOLD(joined, VITB12_mcg)
iron_household <- DIFF_HOUSEHOLD(joined, IRON_mg)
zinc_household <- DIFF_HOUSEHOLD(joined, ZINC_mg)

# Population variables
vit_a_population <- MICRONUT_SUM(joined, VITA_RAE_mcg)
folate_population <- MICRONUT_SUM(joined, FOLDFE_mcg)
vit_b12_population <- MICRONUT_SUM(joined, VITB12_mcg)
iron_population <- MICRONUT_SUM(joined, IRON_mg)
zinc_population <- MICRONUT_SUM(joined, ZINC_mg)

################### fortification vehicles ###########################

##### overall calories

energy_population <- MICRONUT_SUM(joined,ENERGY_kcal)


#### rice difference

RICE_men <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Male") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  slice(1)

RICE_women <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED))%>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Female") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  slice(1)
  
## difference rice per household
RICE_HOUSEHOLD <- RICE_men %>% 
  ungroup() %>% 
  rename(RICE_men_g = sum_RICE_g) %>% 
  select(HOUSEHOLD, ADM2_NAME, RICE_men_g) %>% 
  full_join((RICE_women %>% ungroup() %>% 
               rename(RICE_women_g = sum_RICE_g
                      ) %>% 
               select(HOUSEHOLD, ADM2_NAME, RICE_women_g)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_rice_g = RICE_men_g - RICE_women_g) %>% 
  ungroup() %>% 
  group_by(ADM2_NAME) %>% 
  summarise(mean_rice_g = mean(diff_rice_g, na.rm = T)) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")


## oil data sets

OIL_men <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("OIL",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_OIL_g = sum(FOOD_AMOUNT_REPORTED))%>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Male") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  slice(1)



OIL_women <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("OIL",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_OIL_g = sum(FOOD_AMOUNT_REPORTED))%>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Female") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  slice(1)

OIL_HOUSEHOLD <- OIL_men %>% 
  ungroup() %>% 
  rename(OIL_men_g = sum_OIL_g) %>% 
  select(HOUSEHOLD, ADM2_NAME, OIL_men_g) %>% 
  left_join((OIL_women %>% ungroup() %>% 
               rename(OIL_women_g = sum_OIL_g
               ) %>% 
               select(HOUSEHOLD, ADM2_NAME, OIL_women_g)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_oil_g = OIL_men_g - OIL_women_g) %>% 
  ungroup() %>% 
  group_by(ADM2_NAME) %>% 
  summarise(mean_rice_g = mean(diff_oil_g, na.rm = T)) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")




# food lists
DQQ_list <- FOOD_GROUP_LIST(joined,DQQ)
DQQ_list <- DQQ_list %>%
  select(SUBJECT,where(~ n_distinct(.) > 1))
GDQS_list <- FOOD_GROUP_LIST(joined,GDQS)
GDQS_list <- GDQS_list %>% 
  select(SUBJECT,where(~ n_distinct(.) > 1))

MDD_list <- FOOD_GROUP_LIST(joined, MDD) 
MDD_list <- MDD_list %>% 
  select(SUBJECT,where(~ n_distinct(.) > 1))
# 



all_mn <- vit_a_population %>% 
  left_join(folate_population, by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "CONSUMPTION_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(iron_population, by = c("SUBJECT", 
                                    "ROUND", 
                                    "HOUSEHOLD",
                                    "SEX",
                                    "AGE_YEAR",
                                    "AGE_GROUP",
                                    "CONSUMPTION_DAY",
                                    "ADM1_NAME",
                                    "ADM2_NAME")) %>% 
  left_join(zinc_population, by = c("SUBJECT", 
                                    "ROUND", 
                                    "HOUSEHOLD",
                                    "SEX",
                                    "AGE_YEAR",
                                    "AGE_GROUP",
                                    "CONSUMPTION_DAY",
                                    "ADM1_NAME",
                                    "ADM2_NAME")) %>% 
  left_join(vit_b12_population, by = c("SUBJECT", 
                                       "ROUND", 
                                       "HOUSEHOLD",
                                       "SEX",
                                       "AGE_YEAR",
                                       "AGE_GROUP",
                                       "CONSUMPTION_DAY",
                                       "ADM1_NAME",
                                       "ADM2_NAME"))
# # usable data frames
# # DQQ
# DQQ_vit_a <- inner_join(vitamin_A_calc, DQQ_list, by = "SUBJECT")
# DQQ_folate <- inner_join(folate_calc, DQQ_list, by = "SUBJECT")
# DQQ_iron <- inner_join(iron_calc, DQQ_list, by = "SUBJECT")
# DQQ_zinc <- inner_join(zinc_calc, DQQ_list, by = "SUBJECT")
# 
# #GDQS
# GDQS_vit_a <- inner_join(vitamin_A_calc, GDQS_list, by = "SUBJECT")
# GDQS_folate <- inner_join(folate_calc, GDQS_list, by = "SUBJECT")
# GDQS_iron <- inner_join(iron_calc, GDQS_list, by = "SUBJECT")
# GDQS_zinc <- inner_join(zinc_calc, GDQS_list, by = "SUBJECT")
# 
# #MDD
# MDD_vit_a <- inner_join(vitamin_A_calc, MDD_list, by = "SUBJECT")
# MDD_folate <- inner_join(folate_calc, MDD_list, by = "SUBJECT")
# MDD_iron <- inner_join(iron_calc, MDD_list, by = "SUBJECT")
# MDD_zinc <- inner_join(zinc_calc, MDD_list, by = "SUBJECT")


# mean intake for all types of demographics within households
# how mean intakes interact between population groups
# estimating intake inadequacy for the different MNs
# 

rm(joined)