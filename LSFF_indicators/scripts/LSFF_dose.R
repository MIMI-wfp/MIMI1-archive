################################################################################
############### SCRIPT FOR CREATING BINARY LSFF DOSE INDICATORS ################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 08-Feb-2024
# Last edited: 14-Feb-2024

# This script is for developing 3 possible options for a binary LSFF dose indicator.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

################################ NIGERIA #######################################

# READ DATA: 

# In the first instance, I will develop this indicator using data from the 
# NGA_LSS 2018-19 survey. Read in data:

vehicle_quantities <- read_csv("fortification_models/data/nga_lss1819_vehicle_quantities.csv")
base_ai <- read_csv("fortification_models/data/nga_lss1819_base_ai.csv")

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #1 #####
#############################

# Now create a second dose indicator, based on staple grain consumption delivering
# 80% of the H-AR for all of the 5 MNs:

# Firstly select staple grain quantities: 
vehicle_quantities <- vehicle_quantities %>% 
  dplyr::select("hhid", "rice_100g", "wheatflour_100g", "maizeflour_100g", 
                "staplegrain_100g")

# Transform all quantities to grams: 
vehicle_quantities <- vehicle_quantities %>% 
  dplyr::mutate_at(vars(-hhid), funs(. * 100)) %>% 
  rename(rice_g = rice_100g, wheatflour_g = wheatflour_100g, 
         maizeflour_g = maizeflour_100g, staplegrain_g = staplegrain_100g)

# Calculate how much of the intake objective was contributed by each vehicle:

rice_contribution <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae = rice_g / 261,
                folate = rice_g / 154,
                vitb12 = rice_g / 160,
                fe = rice_g / 448,
                zn = rice_g / 136) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

wheatflour_contribution <- vehicle_quantities %>%
  dplyr::mutate(vita_rae = wheatflour_g / 245,
                folate = wheatflour_g / 50,
                vitb12 = wheatflour_g / 94,
                fe = wheatflour_g / 299,
                zn = wheatflour_g / 148) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

maizeflour_contribution <- vehicle_quantities %>%
  dplyr::mutate(vita_rae = maizeflour_g / 245,
                folate = maizeflour_g / 83,
                vitb12 = maizeflour_g / 94,
                fe = maizeflour_g / 345,
                zn = maizeflour_g / 125) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

# Now, sum the contributions of each vehicle to get the total contribution:
staplegrain_contribution <- rice_contribution %>% 
  dplyr::left_join(wheatflour_contribution, by = "hhid") %>%
  dplyr::left_join(maizeflour_contribution, by = "hhid") %>% 
  dplyr::mutate(vita_rae = vita_rae.x + vita_rae.y + vita_rae,
                folate = folate.x + folate.y + folate,
                vitb12 = vitb12.x + vitb12.y + vitb12,
                fe = fe.x + fe.y + fe,
                zn = zn.x + zn.y + zn) %>% 
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn")

# Now binarise variables, if >1, then 1, else 0:
staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(vita_rae = ifelse(vita_rae > 1, 1, 0),
                folate = ifelse(folate > 1, 1, 0),
                vitb12 = ifelse(vitb12 > 1, 1, 0),
                fe = ifelse(fe > 1, 1, 0),
                zn = ifelse(zn > 1, 1, 0))

# Create dose 1 variable, if all MNs are met, then 1, else 0:
staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(dose1 = ifelse(rowSums(staplegrain_contribution[,2:6]) == 5, 1, 0))

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #2 #####
#############################

# This indicator is similar to the above, but only required the staple grain to 
# deliver 80% of the H-AR for 3 or more MNs:

staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(dose2 = ifelse(rowSums(staplegrain_contribution[,2:6]) >= 3, 1, 0))

#-------------------------------------------------------------------------------

# Create a data-frame to store these dose indicators:

nga_dose <- staplegrain_contribution %>% 
  dplyr::select("hhid", "dose1", "dose2")


# Remove objects not required further:
rm(list = c("rice_contribution", "wheatflour_contribution", "maizeflour_contribution", 
            "staplegrain_contribution"))

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #3 #####
#############################

# Re-read the vehicle quantities data:

vehicle_quantities <- read_csv("fortification_models/data/nga_lss1819_vehicle_quantities.csv")

# The first option is to create a binary indicator for LSFF dose based on:
# (quantity consumed of each vehicle * Theoretical intake under a fortification scenario) + base_ai
# If nutrient adequacy was inadequate under base_ai, and becomes adequate after fortification,
# then the binary indicator will be 1, otherwise 0.

# Rice fortification (data from nigeria fortification contents excel sheet): 

rice_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = ((rice_100g/10)*1500),
                thia_mg = ((rice_100g/10)*4.4),
                ribo_mg = ((rice_100g/10)*0),
                niac_mg = ((rice_100g/10)*44),
                vitb6_mg = ((rice_100g/10)*4.9),
                folate_mcg = ((rice_100g/10)*1200),
                vitb12_mcg = ((rice_100g/10)*10),
                fe_mg = ((rice_100g/10)*29),
                zn_mg = ((rice_100g/10)*47.4)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))


# Wheat flour fortification: 

wheatflour_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = ((wheatflour_100g/10)*1600),
                thia_mg = ((wheatflour_100g/10)*4.2),
                ribo_mg = ((wheatflour_100g/10)*4.25),
                niac_mg = ((wheatflour_100g/10)*38.25),
                vitb6_mg = ((wheatflour_100g/10)*4.8),
                folate_mcg = ((wheatflour_100g/10)*2080),
                vitb12_mcg = ((wheatflour_100g/10)*17),
                fe_mg = ((wheatflour_100g/10)*40),
                zn_mg = ((wheatflour_100g/10)*50)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))


# Maize flour fortification:

maizeflour_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = ((maizeflour_100g/10)*1600),
                thia_mg = ((maizeflour_100g/10)*4.2),
                ribo_mg = ((maizeflour_100g/10)*4.25),
                niac_mg = ((maizeflour_100g/10)*38.25),
                vitb6_mg = ((maizeflour_100g/10)*4.8),
                folate_mcg = ((maizeflour_100g/10)*2080),
                vitb12_mcg = ((maizeflour_100g/10)*17),
                fe_mg = ((maizeflour_100g/10)*40),
                zn_mg = ((maizeflour_100g/10)*50)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

# Overall fortication ai: 
# Add nutrient intake from fortifying rice, wheat flour and maize flour, to 
# nutrient intake in base_ai:

fortification_ai <- (base_ai %>% dplyr::select("hhid", "vita_rae_mcg", "thia_mg", 
                                               "ribo_mg", "niac_mg", "vitb6_mg", 
                                               "folate_mcg", "vitb12_mcg", "fe_mg", 
                                               "zn_mg")) %>% 
  dplyr::left_join(rice_fortified, by = "hhid") %>%
  dplyr::left_join(wheatflour_fortified, by = "hhid") %>%
  dplyr::left_join(maizeflour_fortified, by = "hhid") %>% 
  dplyr::mutate(vita_rae_mcg = vita_rae_mcg.x + vita_rae_mcg.y + vita_rae_mcg.x.x + vita_rae_mcg.y.y,
                thia_mg = thia_mg.x + thia_mg.y + thia_mg.x.x + thia_mg.y.y,
                ribo_mg = ribo_mg.x + ribo_mg.y + ribo_mg.x.x + ribo_mg.y.y,
                niac_mg = niac_mg.x + niac_mg.y + niac_mg.x.x + niac_mg.y.y,
                vitb6_mg = vitb6_mg.x + vitb6_mg.y + vitb6_mg.x.x + vitb6_mg.y.y,
                folate_mcg = folate_mcg.x + folate_mcg.y + folate_mcg.x.x + folate_mcg.y.y,
                vitb12_mcg = vitb12_mcg.x + vitb12_mcg.y + vitb12_mcg.x.x + vitb12_mcg.y.y,
                fe_mg = fe_mg.x + fe_mg.y + fe_mg.x.x + fe_mg.y.y,
                zn_mg = zn_mg.x + zn_mg.y + zn_mg.x.x + zn_mg.y.y) %>%
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg",
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

#-------------------------------------------------------------------------------

# For current analyses, focus on the 5 select micnonutrients:

base_ai <- base_ai %>% 
  dplyr::select("hhid", "vita_rae_mcg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

fortification_ai <- fortification_ai %>%
  dplyr::select("hhid", "vita_rae_mcg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

#-------------------------------------------------------------------------------

# Now, firstly calculate nutrient adequacy under a base scenario: 

base_ai <- base_ai %>% 
  dplyr::mutate(vita_rae_mcg = ifelse(vita_rae_mcg < 490, "Inadequate", "Adequate"),
                folate_mcg = ifelse(folate_mcg < 250, "Inadequate", "Adequate"),
                vitb12_mcg = ifelse(vitb12_mcg < 2, "Inadequate", "Adequate"),
                fe_mg = ifelse(fe_mg < 22.4, "Inadequate", "Adequate"),
                zn_mg = ifelse(zn_mg < 10.2, "Inadequate", "Adequate"))

# Create an additional variable for overall adequacy (adequacy of 3 or more MNs)
# Adequate == 1, Inadequate == 0:
base_ai <- base_ai %>% 
  mutate(nut_adeq = ifelse(rowSums(base_ai[,2:6] == "Adequate") >= 3, 
                           1, 0))


# Now, calculate nutrient adequacy under a fortification scenario:

fortification_ai <- fortification_ai %>% 
  dplyr::mutate(vita_rae_mcg = ifelse(vita_rae_mcg < 490, "Inadequate", "Adequate"),
                folate_mcg = ifelse(folate_mcg < 250, "Inadequate", "Adequate"),
                vitb12_mcg = ifelse(vitb12_mcg < 2, "Inadequate", "Adequate"),
                fe_mg = ifelse(fe_mg < 22.4, "Inadequate", "Adequate"),
                zn_mg = ifelse(zn_mg < 10.2, "Inadequate", "Adequate"))

# Create an additional variable for overall adequacy (adequacy of 3 or more MNs)
# Adequate == 1, Inadequate == 0:
fortification_ai <- fortification_ai %>% 
  mutate(nut_adeq = ifelse(rowSums(fortification_ai[,2:6] == "Adequate") >= 3, 
                           1, 0))

#-------------------------------------------------------------------------------

# Now, use this data to create dose variable: 

dose3 <- base_ai %>% 
  dplyr::select("hhid", "nut_adeq") %>%
  rename(base_adeq = nut_adeq) %>% 
  dplyr::left_join(fortification_ai %>% dplyr::select("hhid", "nut_adeq"), 
                   by = "hhid") %>%
  rename(fort_adeq = nut_adeq) %>% 
  dplyr::mutate(dose3 = ifelse(base_adeq == 0 & fort_adeq == 1, 1,
                               ifelse(base_adeq == 0 & fort_adeq == 0, 0, NA)))

#-------------------------------------------------------------------------------

# Add to nga_dose: 

nga_dose <- nga_dose %>% 
  dplyr::left_join(dose3 %>% dplyr::select("hhid", "base_adeq", "dose3"), 
                   by = "hhid")

# Remove objects not required further: 
rm(list = c("base_ai", "dose3", "fortification_ai", "maizeflour_fortified", "rice_fortified", 
            "wheatflour_fortified"))

#-------------------------------------------------------------------------------

# FINAL EDITS AND WRITE CSV: 

# Ensure that the denominator of the dose variable is households with inadequate 
# base intake:

nga_dose <- nga_dose %>% 
  dplyr::mutate(dose1 = ifelse(base_adeq == 0, dose1, NA),
                dose2 = ifelse(base_adeq == 0, dose2, NA))

# Export csv file with relevant variables for further analysis: 

nga_indicators <- nga_dose %>% 
  dplyr::select("hhid", "base_adeq", "dose1", "dose2", "dose3") %>% 
  dplyr::left_join(vehicle_quantities %>% dplyr::select("hhid", "staple_grain"), 
                   by = "hhid")

# Write to csv:
# write_csv(nga_indicators, "LSFF_indicators/data/nga_lss1819_indicators.csv")

rm(list = ls())

#-------------------------------------------------------------------------------

################################# ETHIOPIA #####################################

# READ DATA: 

vehicle_quantities <- read_csv("fortification_models/data/eth_hices1516_vehicle_quantities.csv")
base_ai <- read_csv("fortification_models/data/eth_hices1516_base_ai.csv")

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #1 #####
#############################

# Now create a second dose indicator, based on staple grain consumption delivering
# 80% of the H-AR for all of the 5 MNs:

# Firstly select staple grain quantities: 
vehicle_quantities <- vehicle_quantities %>% 
  dplyr::select("hhid", "rice_100g", "wheatflour_100g", "maizeflour_100g", 
                "staplegrain_100g")

# Transform all quantities to grams: 
vehicle_quantities <- vehicle_quantities %>% 
  dplyr::mutate_at(vars(-hhid), funs(. * 100)) %>% 
  rename(rice_g = rice_100g, wheatflour_g = wheatflour_100g, 
         maizeflour_g = maizeflour_100g, staplegrain_g = staplegrain_100g)

# Calculate how much of the intake objective was contributed by each vehicle:

rice_contribution <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae = rice_g / 261,
                folate = rice_g / 154,
                vitb12 = rice_g / 160,
                fe = rice_g / 448,
                zn = rice_g / 136) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

wheatflour_contribution <- vehicle_quantities %>%
  dplyr::mutate(vita_rae = NA,
                folate = wheatflour_g / 115,
                vitb12 = wheatflour_g / 94,
                fe = NA,
                zn = wheatflour_g / 74) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

maizeflour_contribution <- vehicle_quantities %>%
  dplyr::mutate(vita_rae = maizeflour_g / 490,
                folate = maizeflour_g / 250,
                vitb12 = maizeflour_g / 235,
                fe = maizeflour_g / 1195,
                zn = maizeflour_g / 204) %>%
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

# Now, sum the contributions of each vehicle to get the total contribution:
staplegrain_contribution <- rice_contribution %>% 
  dplyr::left_join(wheatflour_contribution, by = "hhid") %>%
  dplyr::left_join(maizeflour_contribution, by = "hhid") %>% 
  dplyr::mutate(vita_rae = vita_rae.x + vita_rae.y + vita_rae,
                folate = folate.x + folate.y + folate,
                vitb12 = vitb12.x + vitb12.y + vitb12,
                fe = fe.x + fe.y + fe,
                zn = zn.x + zn.y + zn) %>% 
  dplyr::select("hhid", "vita_rae", "folate", "vitb12", "fe", "zn")

# Now binarise variables, if >1, then 1, else 0:
staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(vita_rae = ifelse(vita_rae > 1, 1, 0),
                folate = ifelse(folate > 1, 1, 0),
                vitb12 = ifelse(vitb12 > 1, 1, 0),
                fe = ifelse(fe > 1, 1, 0),
                zn = ifelse(zn > 1, 1, 0))

# Create dose 1 variable, if all MNs are met, then 1, else 0:
staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(dose1 = ifelse(rowSums(staplegrain_contribution[,2:6]) == 5, 1, 0))

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #2 #####
#############################

# This indicator is similar to the above, but only required the staple grain to 
# deliver 80% of the H-AR for 3 or more MNs:

staplegrain_contribution <- staplegrain_contribution %>% 
  dplyr::mutate(dose2 = ifelse(rowSums(staplegrain_contribution[,2:6]) >= 3, 1, 0))

#-------------------------------------------------------------------------------

# Create a data-frame to store these dose indicators:

eth_dose <- staplegrain_contribution %>% 
  dplyr::select("hhid", "dose1", "dose2")


# Remove objects not required further:
rm(list = c("rice_contribution", "wheatflour_contribution", "maizeflour_contribution", 
            "staplegrain_contribution"))

#-------------------------------------------------------------------------------

#############################
##### DOSE INDICATOR #3 #####
#############################

# Re-read the vehicle quantities data:

vehicle_quantities <- read_csv("fortification_models/data/eth_hices1516_vehicle_quantities.csv")

# The first option is to create a binary indicator for LSFF dose based on:
# (quantity consumed of each vehicle * Theoretical intake under a fortification scenario) + base_ai
# If nutrient adequacy was inadequate under base_ai, and becomes adequate after fortification,
# then the binary indicator will be 1, otherwise 0.

# Rice fortification (data from Ethiopia fortification contents excel sheet): 

rice_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = ((rice_100g/10)*1500),
                thia_mg = ((rice_100g/10)*4.4),
                ribo_mg = 0,
                niac_mg = ((rice_100g/10)*45),
                vitb6_mg = ((rice_100g/10)*4.44),
                folate_mcg = ((rice_100g/10)*1200),
                vitb12_mcg = ((rice_100g/10)*10),
                fe_mg = ((rice_100g/10)*29),
                zn_mg = ((rice_100g/10)*46.8)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))


# Wheat flour fortification: 

wheatflour_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = 0,
                thia_mg = ((wheatflour_100g/10)*6.3),
                ribo_mg = ((wheatflour_100g/10)*5.1),
                niac_mg = ((wheatflour_100g/10)*42.5),
                vitb6_mg = ((wheatflour_100g/10)*4.8),
                folate_mcg = ((wheatflour_100g/10)*1600),
                vitb12_mcg = ((wheatflour_100g/10)*17),
                fe_mg = 0,
                zn_mg = ((wheatflour_100g/10)*80)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))


# Maize flour fortification:

maizeflour_fortified <- vehicle_quantities %>% 
  dplyr::mutate(vita_rae_mcg = ((maizeflour_100g/10)*800),
                thia_mg = ((maizeflour_100g/10)*2.45),
                ribo_mg = ((maizeflour_100g/10)*1.87),
                niac_mg = ((maizeflour_100g/10)*22.95),
                vitb6_mg = 0,
                folate_mcg = ((maizeflour_100g/10)*632),
                vitb12_mcg = ((maizeflour_100g/10)*6.8),
                fe_mg = ((maizeflour_100g/10)*4),
                zn_mg = ((maizeflour_100g/10)*0)) %>% 
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg", 
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

# Overall fortication ai: 
# Add nutrient intake from fortifying rice, wheat flour and maize flour, to 
# nutrient intake in base_ai:

fortification_ai <- (base_ai %>% dplyr::select("hhid", "vita_rae_mcg", "thia_mg", 
                                               "ribo_mg", "niac_mg", "vitb6_mg", 
                                               "folate_mcg", "vitb12_mcg", "fe_mg", 
                                               "zn_mg")) %>% 
  dplyr::left_join(rice_fortified, by = "hhid") %>%
  dplyr::left_join(wheatflour_fortified, by = "hhid") %>%
  dplyr::left_join(maizeflour_fortified, by = "hhid") %>% 
  dplyr::mutate(vita_rae_mcg = vita_rae_mcg.x + vita_rae_mcg.y + vita_rae_mcg.x.x + vita_rae_mcg.y.y,
                thia_mg = thia_mg.x + thia_mg.y + thia_mg.x.x + thia_mg.y.y,
                ribo_mg = ribo_mg.x + ribo_mg.y + ribo_mg.x.x + ribo_mg.y.y,
                niac_mg = niac_mg.x + niac_mg.y + niac_mg.x.x + niac_mg.y.y,
                vitb6_mg = vitb6_mg.x + vitb6_mg.y + vitb6_mg.x.x + vitb6_mg.y.y,
                folate_mcg = folate_mcg.x + folate_mcg.y + folate_mcg.x.x + folate_mcg.y.y,
                vitb12_mcg = vitb12_mcg.x + vitb12_mcg.y + vitb12_mcg.x.x + vitb12_mcg.y.y,
                fe_mg = fe_mg.x + fe_mg.y + fe_mg.x.x + fe_mg.y.y,
                zn_mg = zn_mg.x + zn_mg.y + zn_mg.x.x + zn_mg.y.y) %>%
  dplyr::select("hhid", "vita_rae_mcg", "thia_mg", "ribo_mg", "niac_mg",
                "vitb6_mg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg") %>% 
  dplyr::mutate_all(funs(replace(., is.na(.), 0)))

#-------------------------------------------------------------------------------

# For current analyses, focus on the 5 select micnonutrients:

base_ai <- base_ai %>% 
  dplyr::select("hhid", "vita_rae_mcg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

fortification_ai <- fortification_ai %>%
  dplyr::select("hhid", "vita_rae_mcg", "folate_mcg", "vitb12_mcg", "fe_mg", "zn_mg")

#-------------------------------------------------------------------------------

# Now, firstly calculate nutrient adequacy under a base scenario: 

base_ai <- base_ai %>% 
  dplyr::mutate(vita_rae_mcg = ifelse(vita_rae_mcg < 490, "Inadequate", "Adequate"),
                folate_mcg = ifelse(folate_mcg < 250, "Inadequate", "Adequate"),
                vitb12_mcg = ifelse(vitb12_mcg < 2, "Inadequate", "Adequate"),
                fe_mg = ifelse(fe_mg < 22.4, "Inadequate", "Adequate"),
                zn_mg = ifelse(zn_mg < 10.2, "Inadequate", "Adequate"))

# Create an additional variable for overall adequacy (adequacy of 3 or more MNs)
# Adequate == 1, Inadequate == 0:
base_ai <- base_ai %>% 
  mutate(nut_adeq = ifelse(rowSums(base_ai[,2:6] == "Adequate") >= 3, 
                           1, 0))


# Now, calculate nutrient adequacy under a fortification scenario:

fortification_ai <- fortification_ai %>% 
  dplyr::mutate(vita_rae_mcg = ifelse(vita_rae_mcg < 490, "Inadequate", "Adequate"),
                folate_mcg = ifelse(folate_mcg < 250, "Inadequate", "Adequate"),
                vitb12_mcg = ifelse(vitb12_mcg < 2, "Inadequate", "Adequate"),
                fe_mg = ifelse(fe_mg < 22.4, "Inadequate", "Adequate"),
                zn_mg = ifelse(zn_mg < 10.2, "Inadequate", "Adequate"))

# Create an additional variable for overall adequacy (adequacy of 3 or more MNs)
# Adequate == 1, Inadequate == 0:
fortification_ai <- fortification_ai %>% 
  mutate(nut_adeq = ifelse(rowSums(fortification_ai[,2:6] == "Adequate") >= 3, 
                           1, 0))

#-------------------------------------------------------------------------------

# Now, use this data to create dose variable: 

dose3 <- base_ai %>% 
  dplyr::select("hhid", "nut_adeq") %>%
  rename(base_adeq = nut_adeq) %>% 
  dplyr::left_join(fortification_ai %>% dplyr::select("hhid", "nut_adeq"), 
                   by = "hhid") %>%
  rename(fort_adeq = nut_adeq) %>% 
  dplyr::mutate(dose3 = ifelse(base_adeq == 0 & fort_adeq == 1, 1,
                               ifelse(base_adeq == 0 & fort_adeq == 0, 0, NA)))

#-------------------------------------------------------------------------------

# Add to eth_dose: 

eth_dose <- eth_dose %>% 
  dplyr::left_join(dose3 %>% dplyr::select("hhid", "base_adeq", "dose3"), 
                   by = "hhid")

# Remove objects not required further: 
rm(list = c("base_ai", "dose3", "fortification_ai", "maizeflour_fortified", "rice_fortified", 
            "wheatflour_fortified"))

#-------------------------------------------------------------------------------

# FINAL EDITS AND WRITE CSV: 

# Ensure that the denominator of the dose variable is households with inadequate 
# base intake:

eth_dose <- eth_dose %>% 
  dplyr::mutate(dose1 = ifelse(base_adeq == 0, dose1, NA),
                dose2 = ifelse(base_adeq == 0, dose2, NA))

# Export csv file with relevant variables for further analysis: 

eth_indicators <- eth_dose %>% 
  dplyr::select("hhid", "base_adeq", "dose1", "dose2", "dose3") %>% 
  dplyr::left_join(vehicle_quantities %>% dplyr::select("hhid", "staple_grain"), 
                   by = "hhid")

# Write to csv:
# write_csv(eth_indicators, "LSFF_indicators/data/eth_hices1516_indicators.csv")

rm(list = ls())

#-------------------------------------------------------------------------------

################################### INDIA ######################################





################################################################################
############################### END OF SCRIPT ##################################
################################################################################








