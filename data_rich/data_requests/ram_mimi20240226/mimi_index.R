################################################################################
##################### RAM DATA REQUEST - MIMI INDEX NGA ########################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 29-Feb-2024
# Last edited: 20-Mar-2024

# Addition of MIMI (simple) index, for display on RAM hungermap

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "readxl", "srvyr", "sf", "rmapshaper",
                 "tmap")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

########################
### PART 1: NIGERIA ####
########################

# READ IN REQUIRED DATA:

# Base apparent intake: 
nga_base_ai <- read_csv("data_rich/fortification_models/data/nga_lss1819_base_ai.csv")

# ADM1 apparent inadequacy: 
nga_ai_adm1 <- read_csv("data_rich/data_requests/ram_mimi20240226/nigeria/nga_ai_adm1.csv")

# ADM2 apparent inadequacy:
nga_ai_adm2 <- read_csv("data_rich/data_requests/ram_mimi20240226/nigeria/nga_ai_adm2.csv")

# Household and location data: 
nga_hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv")
household_locations <- read_csv("map_data/nga/new_shapefiles/household_locations.csv")
nga_admins <- read_excel("data_rich/data_requests/ram_mimi20240226/all_admins.xlsx",
                         sheet = "nga")


# nga_mimi <- read_csv("data_rich/data_requests/ram_mimi20240226/nga_mimi_20240227.csv")
# nga_cover <- read_csv("survey_data/nga/secta_cover.csv")
# wfp_ram <- read_excel("data_rich/data_requests/ram_mimi20240226/wfp_ram.xlsx")
# adm2_shp <- st_read("map_data/nga/new_shapefiles/nigeria_2")

#-------------------------------------------------------------------------------

# Calculate Nutrient Adequacy Ratio (NAR) for each micronutrient (household level): 
nga_base_ai <- nga_base_ai %>% 
  mutate(va_nar = nga_base_ai$vita_rae_mcg / 490,
         fo_nar = nga_base_ai$folate_mcg / 250,
         vb12_nar = nga_base_ai$vitb12_mcg / 2,
         fe_nar = nga_base_ai$fe_mg / 22.4, 
         zn_nar = nga_base_ai$zn_mg / 10.2) %>% 
  dplyr::select(hhid, va_nar, fo_nar, vb12_nar, fe_nar, zn_nar)

# If NAR > 1, then == 1, else NAR: 
nga_base_ai <- nga_base_ai %>% 
  mutate(across(va_nar:zn_nar, ~ifelse(. > 1, 1, .)))

# Calculate Mean adequacy ratio (MIMI simple): 
nga_base_ai <- nga_base_ai %>% 
  mutate(mimi_simple = rowMeans(across(va_nar:zn_nar), na.rm = T))

#-------------------------------------------------------------------------------

# Binarise MIMI simple, 1 if indadequate (< 0.75)

nga_base_ai <- nga_base_ai %>% 
  mutate(mimi_simple = ifelse(mimi_simple < 0.75, 1, 0))

#-------------------------------------------------------------------------------

# Left join ADM1, ADM2 and survey weight: 
nga_base_ai <- nga_base_ai %>% 
  dplyr::left_join(nga_hh_info %>% select(hhid, adm1, adm2, survey_wgt), by = "hhid")

#-------------------------------------------------------------------------------

# Filter to include only required columns: 
nga_base_ai <- nga_base_ai %>% 
  dplyr::select(hhid, mimi_simple, adm1, adm2, survey_wgt)

# Create a tbl_svy object:
nga_svy <- as_survey_design(nga_base_ai, 
                            weights = survey_wgt)

# Calculate survey weighted percentage of households with inadequate MIMI simple index:

# ADM1:
nga_mimi_adm1 <- nga_svy %>% 
  group_by(adm1) %>%
  summarise(mimi_simple = survey_mean(mimi_simple, 
                                      proportion = TRUE,
                                      na.rm = TRUE,
                                      vartype = NULL))

# ADM2: 
nga_mimi_adm2 <- nga_svy %>% 
  group_by(adm2) %>%
  summarise(mimi_simple = survey_mean(mimi_simple, 
                                      proportion = TRUE,
                                      na.rm = TRUE,
                                      vartype = NULL))

# Multiply values by 100 to get percentage:
nga_mimi_adm1 <- nga_mimi_adm1 %>% 
  mutate(mimi_simple = round((mimi_simple * 100), digits = 5))

nga_mimi_adm2 <- nga_mimi_adm2 %>%
  mutate(mimi_simple = round((mimi_simple * 100), digits = 5))

#-------------------------------------------------------------------------------

# Join MIMI index to apparent inadequacy dataframes: 
nga_ai_adm1 <- nga_ai_adm1 %>% 
  dplyr::left_join(nga_mimi_adm1, by = c("adm1" = "adm1"))

nga_ai_adm2 <- nga_ai_adm2 %>%
  dplyr::left_join(nga_mimi_adm2, by = c("adm2" = "adm2"))

# Remove objects that are no longer required: 
rm(list = c("nga_mimi_adm1", "nga_mimi_adm2", "nga_svy", "nga_base_ai"))

#-------------------------------------------------------------------------------

# Join HungerMap ADM1 names: 

# Get ADM1/state names: 
ADM1_names <- nga_hh_info %>% 
  dplyr::select(hhid, adm1) %>% 
  dplyr::left_join(household_locations %>% 
                     dplyr::select(hhid, state), by = "hhid") %>% 
  dplyr::select(adm1, state) %>%
  distinct()

# Hungermap ADM1 codes: 
ADM1_codes <- nga_admins %>% 
  dplyr::select(Code_adm1, Name_adm1) %>% 
  distinct() %>% 
  # convert all entries in name_adm1 to lowercase: 
  mutate(Name_adm1 = tolower(Name_adm1))

# Reovle spelling differences in ADM1_name: 
ADM1_names$state[ADM1_names$state == "fct"] <- "abuja"
ADM1_names$state[ADM1_names$state == "nasarawa"] <- "nassarawa"

# Join ADM1 names to ADM1 codes:
ADM1_codes <- ADM1_codes %>% 
  dplyr::left_join(ADM1_names, by = c("Name_adm1" = "state"))

rm(ADM1_names)

# Join ADM1 code to nga_ai_adm1: 
nga_ai_adm1 <- nga_ai_adm1 %>% 
  dplyr::left_join(ADM1_codes, by = "adm1") %>% 
  dplyr::select(iso, Code_adm1, va_ai, fo_ai, vb12_ai, fe_ai, zn_ai, mimi_simple) %>% 
  rename(Code = Code_adm1,
         fol_ai = fo_ai)

# Write as csv: 
write_csv(nga_ai_adm1, "data_rich/data_requests/ram_mimi20240226/nigeria/nga_mimi_adm1.csv")

# Clear environment: 
rm(list = ls())

# Note that decision has been taken to not display results aggregated at ADM2 
# level at present on the HungermapLIVE. We may choose to revisit this in future.

#-------------------------------------------------------------------------------

########################
### PART 2: ETHIOPIA ###
########################

# READ IN REQUIRED DATA

# Base apparent intake: 
eth_base_ai <- read_csv("data_rich/fortification_models/data/eth_hices1516_base_ai.csv")

# ADM1 apparent inadequacy: 
eth_ai_adm1 <- read_csv("data_rich/data_requests/ram_mimi20240226/ethiopia/eth_ai_adm1.csv")

# ADM2 apparent inadequacy:
eth_ai_adm2 <- read_csv("data_rich/data_requests/ram_mimi20240226/ethiopia/eth_ai_adm2.csv")

# Household and location data: 
eth_hh_info <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_hh_info.csv")
eth_admins <- read_csv("data_rich/data_requests/ram_mimi20240226/ethiopia/eth_ram_adm1.csv") %>% 
  dplyr::select(Code, Name)
# eth_admins <- read_excel("data_rich/data_requests/ram_mimi20240226/all_admins.xlsx",
#                          sheet = "eth2_merged")

#-------------------------------------------------------------------------------

# Calculate Nutrient Adequacy Ratio (NAR) for each micronutrient (household level):
eth_base_ai <- eth_base_ai %>% 
  mutate(va_nar = eth_base_ai$vita_rae_mcg / 490,
         fo_nar = eth_base_ai$folate_mcg / 250,
         vb12_nar = eth_base_ai$vitb12_mcg / 2,
         fe_nar = eth_base_ai$fe_mg / 22.4, 
         zn_nar = eth_base_ai$zn_mg / 10.2) %>% 
  dplyr::select(hhid, va_nar, fo_nar, vb12_nar, fe_nar, zn_nar)

# If NAR > 1, then == 1, else NAR: 
eth_base_ai <- eth_base_ai %>% 
  mutate(across(va_nar:zn_nar, ~ifelse(. > 1, 1, .)))

# Calculate Mean adequacy ratio (MIMI simple): 
eth_base_ai <- eth_base_ai %>% 
  mutate(mimi_simple = rowMeans(across(va_nar:zn_nar), na.rm = T))

# Binarise MIMI simple, 1 if inadequate (< 0.75): 
eth_base_ai <- eth_base_ai %>% 
  mutate(mimi_simple = ifelse(mimi_simple < 0.75, 1, 0))

#-------------------------------------------------------------------------------

# ESTIMATE PREVALENCE OF INADEQUACY AGGREGATED AT ADM1 LEVEL 

# Filter columns to include only mimi_simple, and left join location data and 
# survey weights: 
eth_base_ai <- eth_base_ai %>% 
  dplyr::select(hhid, mimi_simple) %>% 
  dplyr::left_join(eth_hh_info %>% 
                     dplyr::select(hhid, adm1, adm2, survey_wgt), by = "hhid")

# Create a tbl_svy object:
eth_svy <- as_survey_design(eth_base_ai, 
                            weights = survey_wgt)

# Calculate survey weighted percentage of household with inadequate MIMI simple
# index: 
eth_mimi_adm1 <- eth_svy %>% 
  group_by(adm1) %>% 
  summarise(mimi_simple = survey_mean(mimi_simple, 
                                      proportion = TRUE,
                                      na.rm = TRUE,
                                      vartype = NULL))

# Multiply values by 100 to get percentage: 
eth_mimi_adm1 <- eth_mimi_adm1 %>% 
  mutate(mimi_simple = round((mimi_simple * 100), digits = 5))

#-------------------------------------------------------------------------------

# JOIN MIMI INDEX TO APPARENT INADEQUACY DATAFRAME

eth_ai_adm1 <- eth_ai_adm1 %>% 
  dplyr::left_join(eth_mimi_adm1, by = "adm1")

# Remove objects that are not required further: 
rm(list = c("eth_mimi_adm1", "eth_svy", "eth_base_ai", "eth_ai_adm2"))

#-------------------------------------------------------------------------------

# JOIN HUNGERMAP ADM1 NAMES

# Resolve spelling differences: 
eth_ai_adm1$adm1[eth_ai_adm1$adm1 == "Benshangul"] <- "B. Gumuz"
eth_ai_adm1$adm1[eth_ai_adm1$adm1 == "Gambella"] <- "Gambela"
eth_ai_adm1$adm1[eth_ai_adm1$adm1 == "Oromiya"] <- "Oromia"
eth_ai_adm1$adm1[eth_ai_adm1$adm1 == "SNNP"] <- "SNNPR"

# Join code from eth_admins: 
eth_ai_adm1 <- eth_ai_adm1 %>% 
  dplyr::left_join(eth_admins, by = c("adm1" = "Name")) %>% 
  dplyr::select(iso, Code, va_ai, fo_ai, vb12_ai, fe_ai, zn_ai, mimi_simple) %>% 
  rename(fol_ai = fo_ai)

# Write csv: 
write_csv(eth_ai_adm1, "data_rich/data_requests/ram_mimi20240226/ethiopia/eth_mimi_adm1.csv")

rm(list = ls())

#-------------------------------------------------------------------------------

########################
##### PART 3: INDIA ####
########################

# READ IN REQUIRED DATA

# Base apparent intake: 
ind_base_ai <- read_csv("data_rich/fortification_models/data/ind_nss1112_base_ai.csv")

# ADM1 apparent inadequacy:
ind_ai_adm1 <- read_csv("data_rich/data_requests/ram_mimi20240226/india/ind_ai_adm1.csv")

# ADM2 apparent inadequacy:
ind_ai_adm2 <- read_csv("data_rich/data_requests/ram_mimi20240226/india/ind_ai_adm2.csv")

# Household and location data: 
ind_hh_info <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_hh_info.csv")
ind_admins <- read_excel("data_rich/data_requests/ram_mimi20240226/all_admins.xlsx",
                         sheet = "ind")

#-------------------------------------------------------------------------------

# India Harmonised Average Requirements (H-AR): 
india_har <- data.frame(vita_rae_mcg = 390, 
  thia_mg = 1.4,
  ribo_mg = 2.0,
  niac_mg = 12,
  vitb6_mg = 1.6,
  folate_mcg = 180,
  vitb12_mcg = 2,
  fe_mg = 15,
  ca_mg = 800,
  zn_mg = 11)

# Calculate Nutrient Adequacy Ratio (NAR) for each micronutrient (household level): 
ind_base_ai <- ind_base_ai %>% 
  mutate(va_nar = ind_base_ai$vita_rae_mcg / india_har$vita_rae_mcg, 
         fo_nar = ind_base_ai$folate_mcg / india_har$folate_mcg,
         vb12_nar = ind_base_ai$vitb12_mcg / india_har$vitb12_mcg,
         fe_nar = ind_base_ai$fe_mg / india_har$fe_mg,
         zn_nar = ind_base_ai$zn_mg / india_har$zn_mg) %>% 
  dplyr::select(hhid, va_nar, fo_nar, vb12_nar, fe_nar, zn_nar)

# If NAR > 1, then == 1, else NAR: 
ind_base_ai <- ind_base_ai %>% 
  mutate(across(va_nar:zn_nar, ~ifelse(. > 1, 1, .)))

# Calculate Mean adequacy ratio (MIMI simple index): 
ind_base_ai <- ind_base_ai %>% 
  mutate(mimi_simple = rowMeans(across(va_nar:zn_nar), na.rm = T))

# Binarise MIMI simple, 1 if inadequate (<0.75): 
ind_base_ai <- ind_base_ai %>% 
  mutate(mimi_simple = ifelse(mimi_simple < 0.75, 1, 0))

#-------------------------------------------------------------------------------

# ESTIMATE PREVALENCE OF INADEQUACY AGGREGATED AT ADM1 LEVEL

# Filter columns to include only mimi_simple, and left join location data and 
# survey weights: 
ind_base_ai <- ind_base_ai %>% 
  dplyr::select(hhid, mimi_simple) %>% 
  dplyr::left_join(ind_hh_info %>% 
                     dplyr::select(hhid, adm1, adm2, survey_wgt), by = "hhid")

# Create a tbl_svy object: 
ind_svy <- as_survey_design(ind_base_ai,
                            weights = survey_wgt)

# Calculate survey weighted percentage of households with inadequate MIMI simple
# index: 
ind_mimi_adm1 <- ind_svy %>% 
  group_by(adm1) %>% 
  summarise(mimi_simple = survey_mean(mimi_simple,
                                      proportion = TRUE, 
                                      na.rm = TRUE, 
                                      vartype = NULL))

# Multiply values by 100 to get percentages: 
ind_mimi_adm1 <- ind_mimi_adm1 %>% 
  mutate(mimi_simple = round((mimi_simple * 100), digits = 5))

#-------------------------------------------------------------------------------

# JOIN MIMI INDEX TO APPARENT INADEQUACY DATAFRAME

ind_ai_adm1 <- ind_ai_adm1 %>% 
  dplyr::left_join(ind_mimi_adm1, by = "adm1")

# Remove objects that are not required further: 
rm(list = c("ind_mimi_adm1", "ind_svy", "ind_base_ai", "ind_ai_adm2", "india_har"))

#-------------------------------------------------------------------------------

# Join HungerMap ADM1 NAMES AND CODES: 

# Firstly select distinct adm1 names and codes from "ind_admins":
ind_admins <- ind_admins %>% 
  dplyr::select(Code_adm1, Name_adm1) %>% 
  distinct()

# Name the adm1's in ind_ai_adm1 according to the National Sample Survey (NSS): 
ind_ai_adm1$adm1[ind_ai_adm1$adm1 == 9] <- "Uttar Pradesh"
ind_ai_adm1$adm1[ind_ai_adm1$adm1 == 10] <- "Bihar"
ind_ai_adm1$adm1[ind_ai_adm1$adm1 == 22] <- "Chhattisgarh"

# Left join the adm1 codes from india_admins: 
ind_ai_adm1 <- ind_ai_adm1 %>% 
  dplyr::left_join(ind_admins, by = c("adm1" = "Name_adm1")) %>% 
  dplyr::select(iso, Code_adm1, va_ai, fo_ai, vb12_ai, fe_ai, zn_ai, mimi_simple) %>%
  rename(Code = Code_adm1,
         fol_ai = fo_ai)

# Write csv: 
write_csv(ind_ai_adm1, "data_rich/data_requests/ram_mimi20240226/india/ind_mimi_adm1.csv")

# Remove all objects: 
rm(list = ls())

#-------------------------------------------------------------------------------

# BIND DATA FROM ALL COUNTRIES INTO A SINGLE DATAFRAME

# Read in data from individual countries:
india <- read_csv("data_rich/data_requests/ram_mimi20240226/india/ind_mimi_adm1.csv")
ethiopia <- read_csv("data_rich/data_requests/ram_mimi20240226/ethiopia/eth_mimi_adm1.csv")
nigeria <- read_csv("data_rich/data_requests/ram_mimi20240226/nigeria/nga_mimi_adm1.csv")

# Bind data from all countries:
hungermap_mimi <- bind_rows(india, ethiopia, nigeria)

# Round all values to 5 decimal places:
hungermap_mimi <- hungermap_mimi %>% 
  mutate(across(va_ai:zn_ai, ~round(., digits = 5))) %>% 
  rename(iso3 = iso)

# Write csv:
write_csv(hungermap_mimi, "data_rich/data_requests/ram_mimi20240226/hungermap_mimi20240321.csv")

rm(list = ls())


################################################################################
################################# END OF SCRIPT ################################
################################################################################

# Code below not used:

# wfp_ram <- wfp_ram %>% 
#   dplyr::select("lga", "wfp_admin2")
# 
# # Left join MIMI simple index with WFP ADM2 names:
# mimi_simple_adm2 <- mimi_simple_adm2 %>% 
#   dplyr::left_join(wfp_ram, by = "lga") %>% 
#   rename(admin2 = wfp_admin2)
# 
# #-------------------------------------------------------------------------------
# 
# # Tidy environment: 
# rm(list = c("nga_base_ai", "nga_cover", "nga_hh_info", "nga_svy", "nga_mimi"))
# 
# #-------------------------------------------------------------------------------
# 
# # JOIN HUNGERMAP ADM1 NAMES:
# 
# 
# #-------------------------------------------------------------------------------
# 
# # JOIN HUNGERMAP ADM2 NAMES: 
# 
# # Replace nga_mimi with a version that has LGA codes: 
# 
# nga_mimi_adm2 <- read_csv("data_rich/data_requests/ram_mimi20240226/nga_ai.csv") %>% 
#   rename(lga = adm2)
# 
# #-------------------------------------------------------------------------------
# 
# # Left join MIMI simple index: 
# nga_mimi <- nga_mimi %>% 
#   dplyr::left_join(mimi_simple, by = "lga")
# 
# #-------------------------------------------------------------------------------
# 
# # Now match RAM ADM2 codes to data-frame: 
# 
# # Need to read in cover csv: 
# cover <- read_csv("survey_data/nga/secta_cover.csv") %>% 
#   dplyr::select(hhid, lga) %>% 
#   rename(LGA = lga)
# 
# # Add LGA code to household_locations:
# household_locations <- household_locations %>% 
#   dplyr::left_join(cover, by = "hhid") 
# 
# # Rename variables for linkage: 
# household_locations <- household_locations %>% 
#   rename(lga_code = LGA)
# 
# wfp_ram <- wfp_ram %>% dplyr::select(lga, wfp_admin2) %>% 
#   rename(lga_code = lga)
# 
# # Add wfp_admin2 to household_locations:
# household_locations <- household_locations %>% 
#   dplyr::left_join(wfp_ram, by = "lga_code") 
# 
# # Left-join lga code to adm2_shp: 
# adm2_shp <- adm2_shp %>% 
#   dplyr::left_join(household_locations %>% 
#                      dplyr::select(lga, lga_code), by = "lga")
# 
# # Remove duplicates: 
# adm2_shp <- adm2_shp %>% 
#   distinct()
# 
# # Add a new empty column to nga_mimi called "Code":
# nga_mimi <- nga_mimi %>% 
#   mutate(Code = NA)
# 
# # Filter nga_mimi to show which entries have duplicate "admin2", showing all duplicates:
# nga_mimi_dup <- nga_mimi %>% 
#   filter(duplicated(admin2) | duplicated(admin2, fromLast = TRUE))
# 
# # And filter to show entries that are non-duplicates:
# nga_mimi_nodup <- nga_mimi %>% 
#   filter(!duplicated(admin2) & !duplicated(admin2, fromLast = TRUE))
# 
# # Convert nga_admins$geometry from a string variable to a polygon:
# nga_admins$geometry <- st_as_sfc(nga_admins$geometry)
# 
# # show Filter which nga_admins are duplicates:
# nga_admins_dup <- nga_admins %>% 
#   filter(duplicated(Name) | duplicated(Name, fromLast = TRUE))
# 
# #-------------------------------------------------------------------------------
# 
# # Plot the geometries for the duplicates to help fill in codes:
# 
# #OBI:
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 714], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[7], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[10], add = TRUE, col = "green")
# 
# # Manually fill in code: 
# nga_mimi_dup$Code[1] <- nga_admins_dup$Code[7]
# nga_mimi_dup$Code[8] <- nga_admins_dup$Code[10]
# 
# # IREPODUN: 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 1313], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[8], add = TRUE, col = "blue")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[2] <- nga_admins_dup$Code[8]
# 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 2309], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[5], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[3], add = TRUE, col = "green")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[5] <- nga_admins_dup$Code[5]
# nga_mimi_dup$Code[11] <- nga_admins_dup$Code[3]
# 
# # NASSARAWA: 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 1931], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[15], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[11], add = TRUE, col = "green")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[3] <- nga_admins_dup$Code[15]
# nga_mimi_dup$Code[7] <- nga_admins_dup$Code[11]
# 
# # BASSA: 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 2204], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[13], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[14], add = TRUE, col = "green")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[4] <- nga_admins_dup$Code[13]
# nga_mimi_dup$Code[13] <- nga_admins_dup$Code[14]
# 
# # SURULERE: 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 2420], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[9], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[12], add = TRUE, col = "green")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[6] <- nga_admins_dup$Code[9]
# nga_mimi_dup$Code[12] <- nga_admins_dup$Code[12]
# 
# # EGBADO: 
# plot(nga_admins$geometry)
# plot(adm2_shp$geometry[adm2_shp$lga_code == 2704], add = TRUE, col = "red")
# plot(nga_admins_dup$geometry[1], add = TRUE, col = "blue")
# plot(nga_admins_dup$geometry[2], add = TRUE, col = "green")
# 
# # Manually fill in code:
# nga_mimi_dup$Code[9] <- nga_admins_dup$Code[1]
# nga_mimi_dup$Code[10] <- nga_admins_dup$Code[2]
# 
# #-------------------------------------------------------------------------------
# 
# nga_mimi_nodup$Code <- NULL
# 
# # Left join Codes from nga_admins to nga_mimi_nodup: 
# nga_mimi_nodup <- nga_mimi_nodup %>% 
#   dplyr::left_join(nga_admins %>% 
#                      dplyr::select(Name, Code), by = c("admin2" = "Name"))
# 
# # Show duplicates by admin2:
# nga_mimi_duplicates <- nga_mimi_nodup %>% 
#   filter(duplicated(admin2) | duplicated(admin2, fromLast = TRUE))
# 
# plot(nga_admins$geometry)
# plot(nga_admins$geometry[nga_admins$Code == 1001751], add = TRUE, col = "red")
# plot(nga_admins$geometry[nga_admins$Code == 22917], add = TRUE, col = "blue")
# 
# #-------------------------------------------------------------------------------
# 
# # Append nga_mimi_nodup and nga_mimi_dup:
# nga_mimi <- rbind(nga_mimi_nodup, nga_mimi_dup)
# 
# # Remove unwanted objects: 
# rm(list = c("adm2_shp", "cover", "household_locations", "mimi_simple", 
#             "nga_admins_dup", "nga_mimi_dup", "nga_mimi_nodup", "wfp_ram"))
# 
# # From nga_mimi, remove the row where Code == 1001751:
# nga_mimi <- nga_mimi %>% 
#   filter(Code != 1001751)
# #-------------------------------------------------------------------------------
# 
# # Re-name Folate column to "fol_ai":
# 
# nga_mimi <- nga_mimi %>% 
#   rename(fol_ai = fo_ai)
# 
# codes <- nga_mimi %>% 
#   dplyr::select(lga, admin2, Code) 
# 
# codes <- codes %>% left_join(nga_admins %>% dplyr::select(Code, Name_adm1), 
#                              by = "Code")
# 
# codes <- codes %>% 
#   dplyr::select(Name_adm1, admin2, lga, Code) %>% 
#   rename(admin1 = Name_adm1)
# 
# nga_mimi <- nga_mimi %>% 
#   dplyr::select(iso, Code, va_ai, fol_ai, vb12_ai, fe_ai, zn_ai, mimi_simple) %>% 
#   rename(iso3 = iso)
# 
# #-------------------------------------------------------------------------------
# 
# # Write csv's: 
# write_csv(nga_mimi, "data_rich/data_requests/ram_mimi20240226/nga_mimi_20240306.csv")
# write_csv(codes, "data_rich/data_requests/ram_mimi20240226/admin2_ramcodes.csv")
# 
# rm(list = ls())




