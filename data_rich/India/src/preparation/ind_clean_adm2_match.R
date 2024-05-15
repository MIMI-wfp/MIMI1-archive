#########################################
#        Simple clean                  #
#########################################

# Author: Gabriel Battcock
# Created: 10 October 23
# Last updated: 6 May 24
# Script does a simple clean of the NSSO 2011-12 data and matches the adm2 varaibles
# to the gadm shapefile


library(tidyr)
library(readr)
library(sf)
library(rmapshaper)
library(readxl)
library(here)

# Read in files ---------------------------------------------------------------------------

# choose whether we select all the states or just bmgf states
# path_to_file <- "./India/data/raw/"
# path_to_file <- "./India/data/raw/extra_states/"
# all states
path_to_file <- "~/Documents/MIMI/data_science_code/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012/"

# read in the data files
block_1_2_identification <- read_csv(paste0(path_to_file, "Identification of Sample Household - Block 1 and 2 - Level 1 -  68.csv"))
block_3_level_2_household_char <- read_csv(paste0(path_to_file, "Household Characteristics - Block 3 -  Level 2 -  68.csv"))
bloc_3_level_3 <- read_csv(paste0(path_to_file, "Household characteristics - Block 3 - Level 3.csv"))
block_4_demog <- read_csv(paste0(path_to_file, "Demographic and other particulars of household members - Block 4  - Level 4 - 68.csv"))
block_5_6_food_consumption <- read_csv(paste0(path_to_file, "Consumption of cereals-pulses- milk and milk products  during the last 30 days  - Block 5.1- 5.2- 6 - Level 5 - 68.csv"))
block_7_8_clothing_consumption <- read_csv(paste0(path_to_file, "Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.csv"))
block_9_edu_expenditure <- read_csv( paste0(path_to_file, "Expenditure on Education and Medical (institutional) goods and services -  Block 9 - Level 7 -  68.csv"))
block_10_misc_expenditure <- read_csv( paste0(path_to_file, "Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days. Block 10 - Level 8 -68.csv"))
block_11_construction_expenditure <- read_csv(paste0(path_to_file, "Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use-  Block 11 - Level 9 -  68.csv"))
block_12_consumer_expenditure <- read_csv(paste0(path_to_file, "Summary of Consumer Expenditure - Block 12 - Level 11 - 68.csv"))
block_13_yoga_ayurveda <- read_csv(paste0(path_to_file, "Information on Ayurveda, Yoga, Naturopathy, Unani, Siddha, Homeopathy(ASYUSH) - Block 13 - Level 10 - 68.csv"))


# Indian food composition table
# ifct <- read_xlsx("~/Documents/MIMI/MIMI_data/India/FCT/ifct_noduplicates_sentencecase_20231110.xlsx")
#read in the conversion from item code to ifct code
# item_code_dict <- read_xlsx(paste0(path_to_file,"food_codes_nsso_to_ifct.xlsx"),sheet = "food_codes")
# item_code_dict %>% dplyr::select(!c(NSS_CES_group)) %>% na.omit()


#shape files ----------------------------------------

# 
# 
# district <- st_read("~/Documents/LSHTM/WFP_project/data/shrug-pc11dist-poly-shp/district.shp")
# district <- district %>% ms_simplify(keep  = 0.1, keep_shapes = T, snap = T)
# plot(district$geometry)
# 
# shrug_adm2_names <- district %>%  sf::st_drop_geometry(data_all)
# write.csv(shrug_adm2_names, "/Users/gabrielbattcock/Documents/MIMI/code/data_rich/India/data/shrug_adm2.csv")
# # gadm2
# gadm_adm2 <- st_read(here::here("../MIMI_data/India/gadm41_IND_shp/gadm41_IND_2.shp"))
# gadm_adm2 <- gadm_adm2 %>%  ms_simplify(keep  = 0.1, keep_shapes = T, snap = T) 

# #gadm3
# gadm2_adm3 <- st_read(here::here("../MIMI_data/India/gadm41_IND_shp/gadm41_IND_3.shp"))
# delhi_gadm3 <- gadm2_adm3 %>% filter(NAME_1 =="NCT of Delhi")
# 
# # 
# x <- gadm_adm2 %>% sf::st_drop_geometry(data_all) %>% select(
#   NAME_1, NAME_2, GID_1, GID_2
# )
# 
# delhi_gadm2 = x %>% filter(NAME_1 =="NCT of Delhi")
# plot(delhi_gadm2 $geometry)
# 
# delhi_shp <-district %>% filter(pc11_s_id =="07")
# plot(delhi_shp$geometry)
# 
# plot(delhi_gadm3$geometry)

 # write.csv(x, "/Users/gabrielbattcock/Documents/MIMI/code/data_rich/India/data/shapefile_names.csv")


#################################################################################

# chat_gbt_match <- read.csv("/Users/gabrielbattcock/Documents/MIMI/code/data_rich/India/data/nsso_match_chatgbt.csv")


#state

# state <- st_read(here::here("../MIMI_data/India/gadm41_IND_shp/gadm41_IND_1.shp"))
# state <- state %>% ms_simplify(keep  = 0.1, keep_shapes = T, snap = T)
# 
# state <- state %>%   sf::st_drop_geometry(data_all)

# write.csv(state, "/Users/gabrielbattcock/Documents/MIMI/code/data_rich/India/data/gadm_adm1.csv")
# adm1_nsso_link <- read.csv("/Users/gabrielbattcock/Documents/MIMI/code/data_rich/India/data/gadm_adm1.csv")


## rematch with NSSO codes
# 
nsso_states <-  state %>%
  left_join(adm1_nsso_link, by =c("GID_1", "NAME_1")) %>%
  group_by(State_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  slice(1)
# 
# 
# 
# 
# nsso_states <- nsso_states %>% 
#   st_as_sf()  %>% ms_simplify(keep  = 0.1, keep_shapes = T, snap = T)
# 
# write_sf(nsso_states, here::here("data_rich/India/data/processed/state_shape.shp"))

plot(nsso_states$geometry)



# plot(state$geometry)
# plot(district$geometry)




# Collecting names of the districts from the NSSO data to match with names of shape file
# district_names <- block_1_2_identification %>% dplyr::select(State_code, District_code) %>%
#   dplyr::group_by(State_code, District_code) %>%
#   dplyr::summarise()


# write_csv(district_names, paste0(path_to_file, "district_names_unmatched.csv" ))
# read in the matched datat
# district_dict <- read_csv(paste0(path_to_file, "district_names.csv"))

# create a new csv with the 
# all_districts <- district_names %>% 
#   left_join(
#     
#   )

# bmgf_states <- district %>% mutate(pc11_s_id = as.numeric(pc11_s_id))
# plot(bmgf_states$geometry)

# 
# bmgf_states <- bmgf_states %>%
#   dplyr::select(State_code,District_code,District_name,pc11_d_id,geometry) %>%
#   filter(!is.na(State_code))
# x_name$d_name %in% bmgf_states$District_name

x_name <- district %>% dplyr::filter(pc11_s_id %in% c("09","10","22"))
# dplyr::anti_join(district %>% dplyr::filter(pc11_s_id == "09"), bmgf_states)
# write a shape file

# bmgf_states <- bmgf_states %>% dplyr::select(State_code,District_code,District_name,pc11_d_id, geometry)
 # write_sf(bmgf_states, "./India_analysis/data/processed/extra_states/district_shape.shp")
# write_sf(state, here::here("data_rich/India/data/processed/state_shape.shp"))

# household level --------------------------------------------------------------

household_characteristic_b3 <- 
  block_3_level_2_household_char %>% 
  dplyr::select(
    HHID, 
    HH_Size,
    HH_Type,
    HH_Type_code,
    Religion, NIC_2008,
    NCO_2004,
    Social_Group,
    whether_Land_owned,
    Type_of_land_owned,
    Land_owned,
    Land_Leased_in,
    Land_Leased_out,
    Land_total_possessed,
    otherwise_Land_possessed,
    During_july10_june11_irrigated,
    During_july10_june11_cultivated,
    Combined_multiplier,
    Subsample_multiplier,
    District_code
    ) %>% 
  dplyr::mutate(
    HH_Type_code = 
      factor(
        ifelse(HH_Type_code == 2 | HH_Type_code >20, "Urban", "Rural")
      ),
    Religion = 
      factor(
         dplyr::case_when(
           Religion == 1 ~ "Hinduism",
           Religion == 2 ~ "Islam",
           Religion == 3 ~ "Christianity",
           Religion == 4 ~ "Sikhism",
           Religion == 5 ~ "Jainism",
           Religion == 6 ~ "Buddhism",
           Religion == 7 ~ "Zoroastrianism",
           Religion == 9 ~ "Others"
         )
      ),
    Social_Group =
      factor(
        dplyr::case_when(
          Social_Group == 1 ~ "ST",
          Social_Group == 2 ~ "SC",
          Social_Group == 3 ~ "OBC",#Other backwards classes
          Social_Group == 9 ~ "Others"
        )
      ),
    whether_Land_owned = factor(ifelse(whether_Land_owned == 1, "Yes", 
                                       "No")),
    Type_of_land_owned =
      factor(
        dplyr::case_when(
          Type_of_land_owned == 1 ~ "Homestead only",
          Type_of_land_owned == 2 ~ "Homestead and other",
          Type_of_land_owned == 3 ~ "Other only"
        )
      ),
    
  )


# readr::write_csv(household_characteristic_b3, here::here("India_analysis",
#                                                  "data",
#                                                  "processed",
#                                                  "household_char.csv"))
readr::write_csv(household_characteristic_b3, here::here("data_rich", "India",
                                                         "data",
                                                         "processed",
                                                         "extra_states",
                                                         "household_char.csv"))

# Household demographics -------------------------------------------------------

demographics_b4 <- 
  block_4_demog %>% 
  dplyr::select(
    HHID,
    Relation,
    Sex,
    Age, 
    Marital_status,
    Education,
    Days_stayed_away,
    Meals_per_day,
    Meals_school,
    Meals_employer,
    Meals_payment,
    Meals_at_home
    ) %>% 
  dplyr::mutate(
    Relation = factor(
      dplyr::case_when(
        Relation == 1 ~ "Self",
        Relation == 2 ~ "Spouse",
        Relation == 3 ~ "Married child",
        Relation == 4 ~ "Spouse of child",
        Relation == 5 ~ "Unmarried child",
        Relation == 6 ~ "Grandchild",
        Relation == 7 ~ "Parent",
        Relation == 8 ~ "Sibling",
        Relation == 9 ~ "Servant"
      )
    ),
    Sex = factor(ifelse(Sex == 1, "Male", "Female")),
    Marital_status = 
      factor(
        dplyr::case_when(
          Marital_status == 1 ~ "never married",
          Marital_status == 2 ~ "currently married",
          Marital_status == 3 ~ "widowed",
          Marital_status == 4 ~ "divorced"
        )
      ),
    Education = factor(
      dplyr::case_when(
        as.numeric(Education) == 1 ~ "non literate",
        as.numeric(Education) == 2 ~ "literate w/o formal school",
        as.numeric(Education) == 3 ~ "through tlc",
        as.numeric(Education) == 4 ~ "others",
        as.numeric(Education) == 5 ~ "below primary",
        as.numeric(Education) == 6 ~ "Primary",
        as.numeric(Education) == 7 ~ "Middle",
        as.numeric(Education) == 8 ~ "Secondary",
        as.numeric(Education) == 10 ~ "Higher secondary",
        as.numeric(Education) == 11 ~ "Diploma/certificate",
        as.numeric(Education) == 12 ~ "Graduate",
        as.numeric(Education) == 13 ~ "Post graduate"
          
      )
    )
  )

# readr::write_csv(demographics_b4, here::here("India_analysis",
#                                              "data",
#                                              "processed",
#                                              
#                                              "demographics.csv"))


readr::write_csv(demographics_b4, here::here("data_rich", "India",
                                             "data",
                                             "processed",
                                             "extra_states",
                                             "demographics.csv"))


# Food composition -------------------------------------------------------------





consumption_b5 <- block_5_6_food_consumption %>%
    dplyr::select(HHID,
                  State_Code,
                  Item_Code,
                  Home_Produce_Quantity,
                  Home_Produce_Value,
                  Total_Consumption_Quantity,
                  Total_Consumption_Value)


# readr::write_csv(consumption_b5, here::here("India_analysis",
#                                              "data",
#                                              "processed",
#                                             
#                                              "consumption.csv"))


readr::write_csv(consumption_b5, here::here("data_rich", "India",
                                            "data",
                                            "processed",
                                            "extra_states",
                                            "consumption.csv"))


rm(list = ls())

