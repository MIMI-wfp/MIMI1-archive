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

ind_consumption <- consumption



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

ind_aa_intake <- apparent_aa_intake()
  
# create intake at an admin level

ind_nsso1112_hh_info %>% 
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
    adm1
  ) %>% 
  srvyr::summarise(
    "mean_lysine":= srvyr::survey_mean(
      lysine_g,
      na.rm = T
    )
  )















# maps for maps
ind_micronutrient_maps('adm1', lysine_g)+
  tm_layout(main.title = "Lysine", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm1', protein_g)+
  tm_layout(main.title = "Protein", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm1', tryptophan_g ) +
  tm_layout(main.title = "Tryptophan", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm1', energy_kcal)



ind_full_item <- full_item_list("ind_nss1112")




x <- intake_aggregate(adm1,energy_kcal)

india_ai %>% 
  select(hhid,lysine_g, tryptophan_g, protein_g)
