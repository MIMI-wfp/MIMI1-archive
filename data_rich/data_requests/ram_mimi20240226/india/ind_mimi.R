################################################################################
##################### RAM DATA REQUEST - MIMI INDEX IND ########################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 18-Mar-2024
# Last edited: 

# Data request from RAM team, for nutrient inadequacy data to be displayed on the
# HML.

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

# READ DATA:

# Base apparent intake:
ind_nss1112_base_ai <- read_csv("data_rich/fortification_models/data/ind_nss1112_base_ai.csv")
ind_nss1112_hh_info <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_hh_info.csv")

#-------------------------------------------------------------------------------

# India E-AR

# Create data-frame with nutrient E-AR values for India: 
ind_ear <- data.frame(
  vita_rae_mcg = 390,
  folate_mcg = 180,
  vitb12_mcg = 2,
  fe_mg = 15,
  zn_mg = 11
)

#-------------------------------------------------------------------------------

# Apply cut-points to the base apparent intake to binarise adequacy: 
ind_nss1112_base_ai <- ind_nss1112_base_ai %>% 
  mutate(va_ai = ifelse(vita_rae_mcg >= ind_ear$vita_rae_mcg, 1, 0),
    folate_ai = ifelse(folate_mcg >= ind_ear$folate_mcg, 1, 0),
    vitb12_ai = ifelse(vitb12_mcg >= ind_ear$vitb12_mcg, 1, 0),
    fe_ai = ifelse(fe_mg >= ind_ear$fe_mg, 1, 0),
    zn_ai = ifelse(zn_mg >= ind_ear$zn_mg, 1, 0)) 

# Calculate NAR for each MN to calculate the MIMI index: 
ind_nss1112_base_ai <- ind_nss1112_base_ai %>% 
  mutate(va_nar = vita_rae_mcg / ind_ear$vita_rae_mcg,
    folate_nar = folate_mcg / ind_ear$folate_mcg,
    vitb12_nar = vitb12_mcg / ind_ear$vitb12_mcg,
    fe_nar = fe_mg / ind_ear$fe_mg,
    zn_nar = zn_mg / ind_ear$zn_mg)

# If NAR > 1, ==1, else == NAR:
ind_nss1112_base_ai <- ind_nss1112_base_ai %>% 
  mutate(across(ends_with("_nar"), ~ifelse(. > 1, 1, .)))

# Calculate MAR (MIMI simple index):
ind_nss1112_base_ai <- ind_nss1112_base_ai %>% 
  mutate(mimi_simple = rowMeans(across(va_nar:zn_nar), na.rm = T))

# Binarise MIMI simple index, 1 if inadequate: 
ind_nss1112_base_ai <- ind_nss1112_base_ai %>% 
  mutate(mimi_simple_bin = ifelse(mimi_simple >= 0.75, 1, 0))
