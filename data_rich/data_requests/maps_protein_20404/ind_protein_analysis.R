#########################################
#        Protein/Amino acid       #
#########################################

# Author: Gabriel Battcock
# Created: 25 Apr 24
# Last updated: 25 Apr 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# call the india mnapping functions
source(here::here("data_rich/India/src/preparation/india_lsff/ind_mapping.R"))

# ------------------------------------------------------------------------------
amino_acid_fct <- read_xlsx("~/Documents/MIMI/MIMI_data/India/India_NSSO_2012/nsso_fct_amino_acids.xlsx") %>% 
  select(item_code, lysine_g,tryptophan_g)

file_path <- here::here("data_rich/India/data/processed/lsff/")
read_in_survey("ind_nss1112",file_path)

ind_consumption <- food_consumption



apparent_aa_intake <- function(name_of_survey = "ind_nss1112", path_to_file = file_path){
  # Estimates apparent intake of amino acids and proteins based on consumed food items
  # and adult female equivalent unit of the household
  read_in_survey(name_of_survey, path_to_file)
  
  x <- food_consumption %>% 
    left_join(fc_table, by = "item_code") %>% 
    mutate(
      across(
        -c(item_code, hhid,item_name ,food_group, quantity_100g, quantity_g),
        ~ifelse(is.na(.x), 0, .x*quantity_100g)
      )
    ) %>% 
    select(hhid,item_code, quantity_100g, protein_g) %>% 
    left_join(amino_acid_fct, by = "item_code") %>% 
    mutate(across(
      c(lysine_g, tryptophan_g),
      # amino acids are provided in grams per 100g of protein
      ~.x*(protein_g/100)
    )) %>% 
    group_by(hhid) %>%
    summarise(
      across(-c(item_code,quantity_100g,),
             ~sum(.,na.rm = T))
    ) %>%
    left_join(hh_info %>% select(hhid, afe), by = "hhid") %>%
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) %>%
    ungroup()
    
  x
}


file_path <- here::here("data_rich/India/data/processed/lsff/")
read_in_survey("ind_nss1112",file_path)
ind_nsso1112_hh_info <- hh_info
rm(hh_info)

  # create intake at an admin level
mean_aa_intake <- function(...){
  

  x <- ind_nsso1112_hh_info %>% 
    dplyr::select(
      hhid, 
      res,
      adm1,
      adm2,
      sep_quintile,
      res_quintile,
      survey_wgt
    ) %>% 
    dplyr::left_join(
      ind_aa_intake,
      by = "hhid"
    ) %>% 
    as_survey_design(
      ids = hhid,
      
      weights = survey_wgt
    ) %>% 
    srvyr::group_by(
      # aggregate by who 
      ...
    ) %>% 
    srvyr::summarise(
      across(
        c(protein_g,lysine_g, tryptophan_g),
        ~survey_mean(.x, na.rm = T)
      )
    )
  return(x)
}

sep_quintile_intake_aa <- mean_aa_intake(sep_quintile) %>% 
  filter(!is.na(sep_quintile))

res_quintile = c(res,res_quintile)

res_quintile_intake_aa <- mean_aa_intake(res,res_quintile)  %>% 
  filter(!is.na(res_quintile))

res_intake_aa <-  mean_aa_intake(res) %>% 
  mutate(res_quintile = 0)

res_quintile_intake_aa <- bind_rows(res_intake_aa,res_quintile_intake_aa)

adm1_intake_aa <-  mean_aa_intake(adm1)

# prevalence of inadequacy -----------------------------------------------------

# RDA 

aa_rda <- data.frame(
  amino_acid = c("lysine", "tryptophan"),
  value_g_per_kg = c(0.030,0.0035)
)

weight_kg = 55

aa_rda <- aa_rda %>% 
  mutate(rda = value_g_per_kg*55)

ind_aa_intake %>% 
  mutate(
    lys_rda = 1.65, 
    tryp_rda = 0.1925,
    lys_inad = ifelse(lysine_g<lys_rda, 1,0),
    tryp_inad = ifelse(tryptophan_g<tryp_rda, 1,0)
  ) %>% 
  summarise(
    sum(lys_inad, na.rm = T),
    sum(tryp_inad, na.rm = T),
    n()
  )
 



