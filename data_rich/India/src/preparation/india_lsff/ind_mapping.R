#########################################
#        create mapping function        #
#########################################

# Author: Gabriel Battcock
# Created: 23 Apr 24
# Last updated: 8 May 24

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

# source the base models function
source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

# ------------------------------------------------------------------------------       
# read in nsso clean data
file_path <- here::here("data_rich/India/data/processed/lsff/")

ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
food_consumption<- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))

india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))

india_ai <- apparent_intake("ind_nss1112", here::here("data_rich/India/data/processed/lsff/"))

# Functions --------------------------------------------------------------------

# Aggregating function ---------------------------------------------------------

intake_aggregate <- function( aggregate_level, micronutrient, path_to_file = file_path){
  # this reads in the Indian apparent intake data and aggregates at different sub-populations
  # Sub-populations could be adm1, adm2, res, sep.
  
  india_ai <- apparent_intake("ind_nss1112",path_to_file)
  ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, "ind_nss1112", "_hh_info.csv"))
  
  output_df <- ind_nsso1112_hh_info %>% 
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
      india_ai,
      by = "hhid"
    ) %>% 
    as_survey_design(
      ids = hhid,
  
      weights = survey_wgt
    ) %>% 
    srvyr::group_by(
      # aggregate by who 
      {{aggregate_level}}
      ) %>% 
    srvyr::summarise(
      "mean_{{micronutrient}}":= srvyr::survey_mean(
        {{micronutrient}},
        na.rm = T
      )
    )
  return(output_df)
  
}
# ind_vita_adm2 <- intake_aggregate(adm2, vita_rae_mcg)
# ind_vitb12_adm2 <- intake_aggregate(adm2, vitb12_mcg)
# ind_lys <- intake_aggregate(adm2, lysine_g)




prevalence_aggregate <- function( aggregate_level, micronutrient, path_to_file = file_path){
  # this reads in the Indian apparent intake data and aggregates at different sub-populations
  # Sub-populations could be adm1, adm2, res, sep.
  
  india_ai <- apparent_intake("ind_nss1112",path_to_file)
  ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file,"ind_nss1112", "_hh_info.csv"))
  
  
  
  nin_ear <- data.frame(
    nutrient = c("energy_kcal",
                 "vita_rae_mcg",
                 "thia_mg",
                 "ribo_mg",
                 "niac_mg",
                 "vitb6_mg",
                 "folate_mcg",
                 "vitb12_mcg",
                 "fe_mg",
                 "ca_mg",
                 "zn_mg"
    ),
    ear_value = c(
      2130,
      390,
      1.4,
      2.0,
      12,
      1.6,
      180,
      2,
      15,
      800,
      11
    )
  )
  
  output_df <- ind_nsso1112_hh_info %>% 
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
      india_ai  %>% 
        select(-c(value,protein_g, lysine_g, tryptophan_g, fat_g, carb_g, cholesterol_mg,
                  vitc_mg,fibre_g, freesugar_g, carotenoids_ug,afe)) %>% 
        pivot_longer(cols = -hhid, names_to = "nutrient") %>% 
        left_join(nin_ear, by= "nutrient"),
      by = "hhid"
    ) %>% 
    mutate(inad = ifelse(value<ear_value,1,0)) %>% 
    as_survey_design(
      ids = hhid,
      
      weights = survey_wgt
    ) %>% 
    srvyr::group_by(
      # aggregate by who 
      {{aggregate_level}}
    ) %>% 
    srvyr::filter(nutrient == {{micronutrient}}) %>% 
    srvyr::summarise(
       inad_prev = srvyr::survey_mean(
        inad == 1, proportion = TRUE,
        na.rm = T
      )
    )
  return(output_df)
  
}

# 
# prevalence_aggregate(adm1, "zn_mg")
# x <- india_adm1 %>% 
#   rename(adm1 = 'State_code') %>%  
#   inner_join(fol_adm1, by= "adm1")
# 
# tm_shape(x) +
#   tm_fill(col = "inad_prev", style = "cont",
#           breaks = seq(0,1,by=.10),
#           palette = (wesanderson::wes_palette("Zissou1Continuous")),
#           legend.is.portrait = FALSE
#   )

# create maps ------------------------------------------------------------------

ind_micronutrient_maps <- function (adm_selection, micronutrient, path_to_file = file_path){

# read in the aggregated data and join to adm1 or adm2
  if(adm_selection == "adm1"){
    ind_micronutrient <- intake_aggregate(adm1, {{micronutrient}}, path_to_file)
    
    shape_file <- india_adm1 %>% 
      rename(adm1 =  State_code) %>% 
      left_join(ind_micronutrient, by = 'adm1') %>% 
      st_as_sf()
  }

  if(adm_selection == "adm2"){
    ind_micronutrient <- intake_aggregate(adm2, {{micronutrient}}, path_to_file)
      
    shape_file <- india_adm2 %>% 
      rename(adm2 = Dstrct_c ) %>% 
      left_join(ind_micronutrient , by = 'adm2') %>% 
      st_as_sf()
  }

  # create a string vavrialbe of the micronutrient we're looking at 
  column_name <-  paste0("mean_",deparse(substitute(micronutrient)))
  title_name <- stringr::str_to_title(gsub("_", " ",deparse(substitute(micronutrient))))

  # create the map
  tm_shape(shape_file) +
    tm_fill(col = column_name, style = "cont",
            #breaks = seq(0,1,by=.10),
            palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
            title = title_name ,
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
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = T)
}

ind_prev_maps <- function (adm_selection, micronutrient, path_to_file = file_path){
  
  # read in the aggregated data and join to adm1 or adm2
  if(adm_selection == "adm1"){
    ind_micronutrient <- prevalence_aggregate(adm1, {{micronutrient}},path_to_file)
    
    shape_file <- india_adm1 %>% 
      rename(adm1 = "State_code" ) %>% 
      left_join(ind_micronutrient, by = 'adm1') %>% 
      st_as_sf()
  }
  
  if(adm_selection == "adm2"){
    ind_micronutrient <- prevalence_aggregate(adm2, {{micronutrient}}, path_to_file)
    
    shape_file <- india_adm2 %>% 
      rename(adm2 = Dstrct_c ) %>% 
      left_join(ind_micronutrient , by = 'adm2') %>% 
      st_as_sf()
  }
  
  # create a string vavrialbe of the micronutrient we're looking at 
  # column_name <-  paste0("mean_",deparse(substitute(micronutrient)))
  title_name <- stringr::str_to_title(gsub("_", " ",deparse(substitute(micronutrient))))
  
  # create the map
  tm_shape(shape_file) +
    tm_fill(col = "inad_prev", style = "cont", breaks = seq(0,1,by=.10),
            palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
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
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = T)
}

# prevalence maps ------------------------------------------------------------

# ind_prev_maps("adm1","vita_rae_mcg")
# 
# ind_prev_maps("adm1","folate_mcg")+ 
#   tm_layout(
#     main.title = "Folate"
#     
#   ) 
# 
# 
# ind_prev_maps("adm1","fe_mg") +
#   tm_layout(
#     main.title = "Iron (not full prob)"
#     
#   ) 
# 
# 
# ind_prev_maps("adm1","zn_mg")+ tm_layout(
#   main.title = "Zinc"
#   
# ) 
# 
# ind_prev_maps("adm1","vitb12_mcg")+
#   tm_layout(
#     main.title = "Vitamin B12"
#     
#   ) 
# 
# ind_prev_maps("adm2","thia_mg")+
#   tm_layout(
#     main.title = "Thiamin"
#     
#   ) 
# 
# 
# ind_prev_maps("adm2","vita_rae_mcg")
# 
# ind_prev_maps("adm2","vita_rae_mcg")
# 
# ind_prev_maps("adm2","vita_rae_mcg")
# 
# ind_prev_maps("adm2","vita_rae_mcg")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ind_micronutrient_maps("adm1", vita_rae_mcg) +
#   tm_layout(
#     main.title = "Vitamin A"
#    
#   ) 
#   
# 
# 
# # maps for maps
# 
ind_micronutrient_maps('adm2', lysine_g)+
  tm_layout(main.title = "Lysine", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', protein_g)+
  tm_layout(main.title = "Protein", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', tryptophan_g ) +
  tm_layout(main.title = "Tryptophan", frame = F,
            main.title.size = 0.8)


