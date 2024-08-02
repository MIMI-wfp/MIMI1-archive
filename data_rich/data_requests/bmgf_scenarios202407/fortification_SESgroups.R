################################################################################
##################### NIGERIA - FORTIFICATION SCENARIOS #######################
################################################################################

# Author: Mo Osman
# Collaborator: Kevin Tang
# Date created: 30-Jul-2024
# Last edited: 

# This script is to perform analyses of fortification scenarios in Nigeria, 
# disaggregated by SES groups and urban/rural locations

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("tidyverse", "readr", "ggplot2", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA: 
apparent_intake <- read_csv("data_rich/data_requests/bmgf_scenarios202407/nga_lss1819_bmgfrequest2.csv")

# Set E-AR values: 
ear <- data_frame(nutrient = c("thia_mg", "ribo_mg"),
                  ear = c(0.9, 1.3))

#-------------------------------------------------------------------------------

# Binarise adequate intake: 
apparent_intake <- apparent_intake %>% 
  mutate(base_thia_inadequacy = ifelse(thia_ai <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         base_ribo_inadequacy = ifelse(ribo_ai <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         wf_thia_inadequacy = ifelse((thia_ai + thia_wheatflour) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         wf_ribo_inadequacy = ifelse((ribo_ai + ribo_wheatflour) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         rice_thia_inadequacy = ifelse((thia_ai + thia_rice) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         rice_ribo_inadequacy = ifelse((ribo_ai + ribo_rice) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         bouillon_thia_inadequacy1 = ifelse((thia_ai + thia_bouillon1) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         bouillon_ribo_inadequacy1 = ifelse((ribo_ai + ribo_bouillon1) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         bouillon_thia_inadequacy2 = ifelse((thia_ai + thia_bouillon2) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         bouillon_ribo_inadequacy2 = ifelse((ribo_ai + ribo_bouillon2) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         all_thia_inadequacy1 = ifelse((thia_ai + thia_allvehicles1) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         all_thia_inadequacy2 = ifelse((thia_ai + thia_allvehicles2) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         all_ribo_inadequacy1 = ifelse((ribo_ai + ribo_allvehicles1) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         all_ribo_inadequacy2 = ifelse((ribo_ai + ribo_allvehicles2) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0))

# De-select variables no longer required: 
apparent_intake <- apparent_intake %>% 
  dplyr::select(hhid, res, sep_quintile, res_quintile, survey_wgt, base_thia_inadequacy,
                base_ribo_inadequacy, wf_thia_inadequacy, wf_ribo_inadequacy, 
                rice_thia_inadequacy, rice_ribo_inadequacy, bouillon_thia_inadequacy1,
                bouillon_ribo_inadequacy1, bouillon_thia_inadequacy2, bouillon_ribo_inadequacy2,
                all_thia_inadequacy1, all_thia_inadequacy2, all_ribo_inadequacy1, all_ribo_inadequacy2)

#-------------------------------------------------------------------------------

# Calculate prevalence of inadequacy for thiamine and riboflavin under different
# fortification sccenarios. 

# Firstly create tbl_svy object for analysis: 
svy_apparent_intake <- apparent_intake %>% 
  as_survey_design(weights = survey_wgt)

# Thiamine: 
national_thia_inadequacy <- svy_apparent_intake %>% 
  summarise(base = round(survey_mean(base_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            wf = round(survey_mean(wf_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            rice = round(survey_mean(rice_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            bouillon1 = round(survey_mean(bouillon_thia_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            bouillon2 = round(survey_mean(bouillon_thia_inadequacy2, na.rm = T, vartype = NULL), digits = 4),
            all1 = round(survey_mean(all_thia_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            all2 = round(survey_mean(all_thia_inadequacy2, na.rm = T, vartype = NULL), digits = 4)) %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Riboflavin:
national_ribo_inadequacy <- svy_apparent_intake %>% 
  summarise(base = round(survey_mean(base_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            wf = round(survey_mean(wf_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            rice = round(survey_mean(rice_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            bouillon1 = round(survey_mean(bouillon_ribo_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            bouillon2 = round(survey_mean(bouillon_ribo_inadequacy2, na.rm = T, vartype = NULL), digits = 4),
            all1 = round(survey_mean(all_ribo_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            all2 = round(survey_mean(all_ribo_inadequacy2, na.rm = T, vartype = NULL), digits = 4)) %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

#-------------------------------------------------------------------------------

# Calculate prevalence of inadequacy disaggregated by urban/rural and SES groups

# Thiamine: 
thia_inadequacy <- svy_apparent_intake %>% 
  mutate(res_quintile = as.factor(res_quintile)) %>%
  group_by(res, res_quintile) %>%
  summarise(base = round(survey_mean(base_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            wf = round(survey_mean(wf_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            rice = round(survey_mean(rice_thia_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            bouillon1 = round(survey_mean(bouillon_thia_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            bouillon2 = round(survey_mean(bouillon_thia_inadequacy2, na.rm = T, vartype = NULL), digits = 4),
            all1 = round(survey_mean(all_thia_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            all2 = round(survey_mean(all_thia_inadequacy2, na.rm = T, vartype = NULL), digits = 4)) %>% 
  ungroup() %>% mutate(across(where(is.numeric), ~ .x * 100)) %>% filter(!is.na(res_quintile))

# Riboflavin:
ribo_inadequacy <- svy_apparent_intake %>% 
  mutate(res_quintile = as.factor(res_quintile)) %>%
  group_by(res, res_quintile) %>%
  summarise(base = round(survey_mean(base_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            wf = round(survey_mean(wf_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            rice = round(survey_mean(rice_ribo_inadequacy, na.rm = T, vartype = NULL), digits = 4),
            bouillon1 = round(survey_mean(bouillon_ribo_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            bouillon2 = round(survey_mean(bouillon_ribo_inadequacy2, na.rm = T, vartype = NULL), digits = 4),
            all1 = round(survey_mean(all_ribo_inadequacy1, na.rm = T, vartype = NULL), digits = 4),
            all2 = round(survey_mean(all_ribo_inadequacy2, na.rm = T, vartype = NULL), digits = 4)) %>% 
  ungroup() %>% mutate(across(where(is.numeric), ~ .x * 100)) %>% filter(!is.na(res_quintile))

# Write csv files: 
# write_csv(thia_inadequacy, "data_rich/data_requests/bmgf_scenarios202407/thiamine/SEP_thia_inadequacy.csv")
# write_csv(ribo_inadequacy, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/SEP_ribo_inadequacy.csv")

#-------------------------------------------------------------------------------

# PREPARE DATA FOR DUMBELL PLOTS: 
thia_inadequacy <- thia_inadequacy %>% 
  mutate(res_quintile = case_when(
    res_quintile == 1 ~ "1 - Poorest",
    res_quintile == 5 ~ "5 - Richest",
    TRUE ~ as.character(res_quintile)
  ))

ribo_inadequacy <- ribo_inadequacy %>%
  mutate(res_quintile = case_when(
    res_quintile == 1 ~ "1 - Poorest",
    res_quintile == 5 ~ "5 - Richest",
    TRUE ~ as.character(res_quintile)
  ))

#-------------------------------------------------------------------------------

# DUMBELL PLOTS 

# Thiamine
thia_dumbell <- ggplot(data = thia_inadequacy) +
  geom_segment(aes(x = base, xend = all1, y = res_quintile, yend = res_quintile), colour = "darkgrey") +
  geom_point(aes(x = base, y = res_quintile, colour = "Base", shape = "Base"), size = 2.5) + 
  geom_point(aes(x = wf, y = res_quintile, colour = "wf", shape = "wf"), size = 2.5) +
  geom_point(aes(x = rice, y = res_quintile, colour = "rice", shape = "rice"), size = 2.5) +
  geom_point(aes(x = bouillon1, y = res_quintile, colour = "bouillon1", shape = "bouillon1"), size = 2.5) +
  geom_point(aes(x = all1, y = res_quintile, colour = "All1", shape = "All1"), size = 2.5) +
  xlim(0, 100) +
  facet_wrap(~res, scales = "free_y", dir = "v") +
  xlab("Prevalence of inadequate Thiamine intake (%)") + 
  ylab("Socio-economic quintile") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_colour_manual(name = "Fortification",
    values = c("Base" = "darkred", "wf" = "darkblue", "rice" = "darkorange", 
               "bouillon1" = "aquamarine4", "All1" = "darkgreen"),
    labels = c("Base" = "None (base case)", "wf" = "Wheat flour", "rice" = "Rice", 
               "bouillon1" = "Bouillon", "All1" = "All vehicles")) +
  scale_shape_manual(name = "Fortification",
    values = c("Base" = 15, "wf" = 3, "rice" = 6, "bouillon1" = 4, "All1" = 16), 
    labels = c("Base" = "None (base case)", "wf" = "Wheat flour", "rice" = "Rice",
               "bouillon1" = "Bouillon", "All1" = "All vehicles")) +
  theme_minimal()

thia_dumbell

# Riboflavin: 
ribo_dumbell <- ggplot(data = ribo_inadequacy) + 
  geom_segment(aes(x = base, xend = all1, y = res_quintile, yend = res_quintile), colour = "darkgrey") +
  geom_point(aes(x = base, y = res_quintile, colour = "Base", shape = "Base"), size = 2.5) + 
  geom_point(aes(x = wf, y = res_quintile, colour = "wf", shape = "wf"), size = 2.5) +
  geom_point(aes(x = rice, y = res_quintile, colour = "rice", shape = "rice"), size = 2.5) +
  geom_point(aes(x = bouillon1, y = res_quintile, colour = "bouillon1", shape = "bouillon1"), size = 2.5) +
  geom_point(aes(x = all1, y = res_quintile, colour = "All1", shape = "All1"), size = 2.5) +
  xlim(0, 100) +
  facet_wrap(~res, scales = "free_y", dir = "v") +
  xlab("Prevalence of inadequate Riboflavin intake (%)") + 
  ylab("Socio-economic quintile") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_colour_manual(name = "Fortification",
    values = c("Base" = "darkred", "wf" = "darkblue", "rice" = "darkorange", 
               "bouillon1" = "aquamarine4", "All1" = "darkgreen"),
    labels = c("Base" = "None (base case)", "wf" = "Wheat flour", "rice" = "Rice", 
               "bouillon1" = "Bouillon", "All1" = "All vehicles")) +
  scale_shape_manual(name = "Fortification",
    values = c("Base" = 15, "wf" = 3, "rice" = 6, "bouillon1" = 4, "All1" = 16), 
    labels = c("Base" = "None (base case)", "wf" = "Wheat flour", "rice" = "Rice",
               "bouillon1" = "Bouillon", "All1" = "All vehicles")) +
  theme_minimal()

ribo_dumbell

#-------------------------------------------------------------------------------

rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################
  
