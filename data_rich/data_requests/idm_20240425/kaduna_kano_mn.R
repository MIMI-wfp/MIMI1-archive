################################################################################
############################# IDM DATA REQUEST  ################################
################################################################################

# Author: Mo Osman
# Date created: 25-04-2024
# Last edited: 

# Data request for IDM research group - Risk of inadequate MN intake estomates
# for Kaduna and Kano states in Nigeria, disaggregated by LGA and SEP.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN REQUIRED DATA:

nga_base_ai <- read_csv("data_rich/all_base_models/data/KA_nga_lss1819_fct/KAnga_lss1819_base_ai.csv") %>% 
  select(hhid, vita_rae_mcg, folate_mcg, vitb12_mcg, fe_mg, zn_mg)

nga_hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv") %>% 
  select(hhid, adm1, adm2, res, survey_wgt)

nga_tot_cons <- read_csv("survey_data/nga/totcons.csv") %>% 
  select(hhid, totcons_adj)

#-------------------------------------------------------------------------------

# CONSOLIDATE DATA:

kaduna_kano <- nga_hh_info %>% 
  filter(adm1 %in% c(18, 19)) %>% 
  left_join(nga_base_ai, by = "hhid") %>%
  left_join(nga_tot_cons, by = "hhid")

# Remove other dataframes no longer required: 
rm(list = c("nga_base_ai", "nga_hh_info", "nga_tot_cons"))

#-------------------------------------------------------------------------------

# SET HARMONISED AVERAGE REQUIREMENTS:
allen_har <- data.frame(
  energy_kcal = 2200,#who
  vita_rae_mcg  = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg_low = 22.4, #low absorption
  fe_mg_mod = 9.6,
  fe_mg_high = 7,
  ca_mg = 860, 
  zn_mg_su = 8.9,#semi unrefined
  zn_mg_u = 10.2
)

#-------------------------------------------------------------------------------

# CALCULATE NUTRIENT ADEQUACY RATIOS (NAR):

kaduna_kano <- kaduna_kano %>% 
  mutate(vita_rae_nar = vita_rae_mcg / allen_har$vita_rae_mcg,
    folate_nar = folate_mcg / allen_har$folate_mcg,
    vitb12_nar = vitb12_mcg / allen_har$vitb12_mcg,
    fe_nar = fe_mg / allen_har$fe_mg_low,
    zn_nar = zn_mg / allen_har$zn_mg_u) %>% 
  # IF NAR > 1, SET TO 1:
  mutate_at(vars(ends_with("_nar")), ~ifelse(. > 1, 1, .))

# Use these values to calculate the MAR: 
kaduna_kano <- kaduna_kano %>% 
  mutate(MAR_5mn_ai = rowMeans(select(., ends_with("_nar")))) %>% 
  # If MAR < 0.75, code as 1 (Inadequate): 
  mutate(MAR_5mn_ai = ifelse(MAR_5mn_ai < 0.75, 1, 0))

# For other MN's, if NAR < 1, code as 1 (Inadequate):
kaduna_kano <- kaduna_kano %>% 
  mutate(va_ai = ifelse(vita_rae_nar < 1, 1, 0),
    fol_ai = ifelse(folate_nar < 1, 1, 0),
    vb12_ai = ifelse(vitb12_nar < 1, 1, 0),
    fe_ai = ifelse(fe_nar < 1, 1, 0),
    zn_ai = ifelse(zn_nar < 1, 1, 0))

#-------------------------------------------------------------------------------

# Select the required variables: 
kaduna_kano <- kaduna_kano %>% 
  dplyr::select(hhid, adm1, adm2, res, totcons_adj, survey_wgt, va_ai, fol_ai, 
                vb12_ai, fe_ai, zn_ai, MAR_5mn_ai)

#-------------------------------------------------------------------------------

# NAME STATES AND LGA's:

kaduna_kano <- kaduna_kano %>% 
  mutate(state = ifelse(adm1 == 18, "Kaduna", "Kano"))

# For LGA's, read in the adm2 matches: 
adm2_lga <- read_csv("data_rich/data_requests/ram_mimi20240226/admin2_ramcodes.csv") %>% 
  rename(adm2 = lga) %>% 
  rename(lga = admin2) %>% 
  dplyr::select(adm2, lga)

# Left join to Kaduna/Kano: 
kaduna_kano <- kaduna_kano %>% 
  left_join(adm2_lga, by = "adm2")

#-------------------------------------------------------------------------------

# SELECT REQUIRED VARIABLES: 

kaduna_kano <- kaduna_kano %>% 
  dplyr::select(hhid, survey_wgt, state, lga, res, totcons_adj, va_ai, fol_ai, 
                vb12_ai, fe_ai, zn_ai, MAR_5mn_ai)

# Write csv: 
write_csv(kaduna_kano, "data_rich/data_requests/idm_20240425/nut_inadequacy_kaduna_kano.csv")


################################################################################
############################### END OF SCRIPT ##################################
################################################################################






