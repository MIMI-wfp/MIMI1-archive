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
source(here::here("data_rich/India/india_lsff/ind_mapping.R"))

# ------------------------------------------------------------------------------
amino_acid_fct <- read_xlsx("../MIMI_data/India/India_NSSO_2012/nsso_fct_amino_acids.xlsx") %>% 
  select(item_code, lysine_g,tryptophan_g)

file_path <- here::here("data_rich/India/data/processed/lsff//")
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


file_path <- here::here("data_rich/India/data/processed/lsff//")
read_in_survey("ind_nss1112",file_path)
ind_nsso1112_hh_info <- hh_info 
rm(hh_info)

  # create intake at an admin level
mean_aa_intake <- function(...){
  ind_aa_intake <- apparent_aa_intake()

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

res_quintile_intake_aa <- mean_aa_intake(res,res_quintile)  %>% 
  filter(!is.na(res_quintile)) %>% 
  bind_rows(
    mean_aa_intake(res) %>% 
      mutate(res_quintile = 0)
  ) %>% mutate(res_quintile = case_when(
    res_quintile == 0 ~ "National",
    res_quintile == 1 ~ "Poorest",
    res_quintile == 2 ~ "Poor",
    res_quintile == 3 ~ "Middle",
    res_quintile == 4 ~ "Rich",
    res_quintile == 5 ~ "Richest"
  ))

pop_intake_aa <- mean_aa_intake()

adm1_intake_aa <-  mean_aa_intake(adm1)

# prevalence of inadequacy -----------------------------------------------------

# RDA 
inad_aa_intake <- function(...){
  
  ind_aa_intake <- apparent_aa_intake()
  
  aa_rda <- data.frame(
    amino_acid = c("lysine", "tryptophan", "protein"),
    value_g_per_kg = c(0.030,0.004, 0.66)#WHO recommendations - https://iris.who.int/bitstream/handle/10665/43411/WHO_TRS_935_eng.pdf
  )
  
  weight_kg = 55 # ASSUMPTION - Based on average body weight of women (EAR india)
  
  aa_rda <- aa_rda %>% 
    mutate(rda = value_g_per_kg*55)
  
  aa_intake_inad <- ind_aa_intake %>% 
    mutate(
      lys_rda = 1.65, 
      tryp_rda = 0.22,
      prot_rda = 36.30,
      lys_inad = ifelse(lysine_g<lys_rda, 1,0),
      tryp_inad = ifelse(tryptophan_g<tryp_rda, 1,0),
      protein_inad = ifelse(protein_g<prot_rda, 1,0)
    ) %>%
    left_join(ind_nsso1112_hh_info, by = 'hhid') %>% 
    as_survey_design(ids = hhid , weights = survey_wgt ) %>% 
    filter(!is.na(res_quintile)) %>% 
    srvyr::group_by(...) %>% 
    srvyr::summarise(
      lys_inad = srvyr::survey_mean(lys_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      tryp_inad = srvyr::survey_mean(tryp_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      protein_inad = srvyr::survey_mean(protein_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
    )
  return(aa_intake_inad)
}

# calculations of sub-population

pop_inad <- inad_aa_intake()

res_inad <- inad_aa_intake(res)

res_quintile_inad <-
  inad_aa_intake(res,res_quintile) %>% 
  filter(!is.na(res_quintile)) %>% 
  bind_rows(
    inad_aa_intake(res) %>% 
      mutate(res_quintile = 0)
  ) %>% mutate(res_quintile = case_when(
    res_quintile == 0 ~ "National",
    res_quintile == 1 ~ "Poorest",
    res_quintile == 2 ~ "Poor",
    res_quintile == 3 ~ "Middle",
    res_quintile == 4 ~ "Rich",
    res_quintile == 5 ~ "Richest"
  ))
  
  
  inad_aa_intake(res, res_quintile)

sep_quintile_inad <- inad_aa_intake(sep_quintile)

adm1_inad <- inad_aa_intake(adm1)

# inadequacy maps --------------------------------------------------------------


ind_prev_maps <- function(amino_acid){
  
  
  adm1_inad <- inad_aa_intake(adm1)
    
  shape_file <- india_adm1 %>% 
      rename(adm1 = "State_code" ) %>% 
      left_join(adm1_inad, by = 'adm1') %>% 
      st_as_sf()

  
  
  # create a string vavrialbe of the micronutrient we're looking at 
  # column_name <-  paste0("mean_",deparse(substitute(micronutrient)))
  title_name <- stringr::str_to_title(gsub("_", " ",deparse(substitute(micronutrient))))
  
  # create the map
  tm_shape(shape_file) +
    tm_fill(col = amino_acid, style = "cont", breaks = seq(0,100,by=10),
            # RColorBrewer::brewer.pal( "Reds"),
            title = "Prevalence of inadequacy" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    tm_borders(col = "black", lwd = 0) +
    tm_shape(india_adm1) +
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1)+
    tm_legend(show = T)
}

# Plot the maps

lysine_inad_plot <- ind_prev_maps('lys_inad')+
  tm_layout(main.title = "Lysine")+
  tm_text("NAME_1", size = 1/2, remove.overlap = TRUE) +
  tm_legend(show = F)+
  tm_credits("Source: India - Household Consumer Expenditure, NSS 68th Round, Sch 1, Type 1",
             position = 'left',
             size = 0.5)

tmap_save(lysine_inad_plot, "data_rich/data_requests/maps_protein_20404/outputs/lysine.png",
          width = 8, height = 8, units = "in", dpi = 600)

# tryptophan
tryptophan_inad_plot <- ind_prev_maps('tryp_inad')+
  tm_layout(main.title = "Tryptophan")+
  tm_text("NAME_1", size = 1/2, remove.overlap = TRUE) +
  tm_legend(show = F)+
  tm_credits("Source: India - Household Consumer Expenditure, NSS 68th Round, Sch 1, Type 1",
             position = 'left',
             size = 0.5)

tmap_save(tryptophan_inad_plot, "data_rich/data_requests/maps_protein_20404/outputs/tryptophan.png",
          width = 8, height = 8, units = "in", dpi = 600)

# protein

protein_inad_plot <- ind_prev_maps('protein_inad')+
  tm_layout(main.title = "Protein")+
  tm_text("NAME_1", size = 1/2, remove.overlap = TRUE) +
  tm_legend(show = F)+
  tm_credits("Source: India - Household Consumer Expenditure, NSS 68th Round, Sch 1, Type 1",
             position = 'left',
             size = 0.5)

tmap_save(protein_inad_plot, "data_rich/data_requests/maps_protein_20404/outputs/protein.png",
          width = 8, height = 8, units = "in", dpi = 600)


# legend

adm1_inad <- inad_aa_intake(adm1)

shape_file <- india_adm1 %>% 
  rename(adm1 = "State_code" ) %>% 
  left_join(adm1_inad, by = 'adm1') %>% 
  st_as_sf()

legend <- tm_shape(shape_file) + 
  tm_fill(col = "protein_inad",style = "cont", breaks = seq(0,100,by=10),
          # palette = RColorBrewer::brewer.pal(7, "Blues"),
          title = "Prevalence of inadequacy (%)" ,
          legend.is.portrait = FALSE
  ) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))
tmap_save(legend, "data_rich/data_requests/maps_protein_20404/outputs/legend.png",
          width = 8, height = 8, units = "in", dpi = 600)



## create an excel spreadsheet for mean intake and inadequacy ------------------
library(writexl)

intake_sheets <- list(
  "population" = pop_intake_aa,
  "state" = adm1_intake_aa,
  "sep" = sep_quintile_intake_aa,
  "residence" = res_quintile_intake_aa
)

write_xlsx(intake_sheets, here::here("data_rich/data_requests/maps_protein_20404/outputs/intake.xlsx"))


inad_sheets <- list(
  "population" = pop_inad,
  "state" = adm1_inad,
  "sep" = sep_quintile_inad,
  "residence" = res_quintile_inad
)


write_xlsx(inad_sheets, here::here("data_rich/data_requests/maps_protein_20404/outputs/inadequacy.xlsx"))
