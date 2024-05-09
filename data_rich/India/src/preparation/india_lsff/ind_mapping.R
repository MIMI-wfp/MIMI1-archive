#########################################
#        create mapping function        #
#########################################

# Author: Gabriel Battcock
# Created: 23 Apr 24
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

# source the base models function
source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

# ------------------------------------------------------------------------------       
# read in nsso clean data
path_to_file <- here::here("data_rich/India/data/processed/lsff/")

ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
food_consumption<- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))

india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))


# Functions --------------------------------------------------------------------

apparent_intake_india <- function(path_to_file){
  # Estimates apparent intake of nutrients based on consumed food items
  # and adult female equivalent unit of the household
  # By setting the path to file, you can change which population you use
  
  #### TODO when we have a matched full India map, set to full country 
  #### but allow selection of which states to use. 
  
  food_consumption <- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
  ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
  fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))
  
  x <- food_consumption %>% 
    left_join(fc_table %>% distinct(item_code, .keep_all = TRUE), by = "item_code") %>% 
    mutate(
      across(
        -c(item_code, hhid,item_name ,food_group, quantity_100g, quantity_g),
        ~ifelse(is.na(.x), 0, .x*quantity_100g)
      )
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      across(-c(item_code,item_name,quantity_100g,quantity_g, food_group),
             ~sum(.,na.rm = T))
    ) %>% 
    left_join(ind_nsso1112_hh_info %>% select(hhid, afe), by = "hhid") %>% 
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) %>% 
    ungroup()
  x
}
india_ai <- apparent_intake_india(path_to_file)

# Aggregating function ---------------------------------------------------------

intake_aggregate <- function( aggregate_level, micronutrient){
  # this reads in the Indian apparent intake data and aggregates at different sub-populations
  # Sub-populations could be adm1, adm2, res, sep.
  
  india_ai <- apparent_intake_india(path_to_file)
  ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
  
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
ind_vita_adm2 <- intake_aggregate(adm2, vita_rae_mcg)
ind_vitb12_adm2 <- intake_aggregate(adm2, vitb12_mcg)
ind_lys <- intake_aggregate(adm2, lysine_g)

# create maps ------------------------------------------------------------------

ind_micronutrient_maps <- function (adm_selection, micronutrient){

# read in the aggregated data and join to adm1 or adm2
  if(adm_selection == "adm1"){
    ind_micronutrient <- intake_aggregate(adm1, {{micronutrient}})
    
    shape_file <- india_adm1 %>% 
      rename(adm1 =  ) %>% 
      left_join(ind_micronutrient, by = 'adm1') %>% 
      st_as_sf()
  }

  if(adm_selection == "adm2"){
    ind_micronutrient <- intake_aggregate(adm2, {{micronutrient}})
      
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

ind_micronutrient_maps("adm2", vita_rae_mcg) +
  tm_layout(
    main.title = "Vitamin A"
   
  ) 
  


# maps for maps

ind_micronutrient_maps('adm2', lysine_g)+
  tm_layout(main.title = "Lysine", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', protein_g)+
  tm_layout(main.title = "Protein", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', tryptophan_g ) +
  tm_layout(main.title = "Tryptophan", frame = F,
            main.title.size = 0.8)



x <- india_adm1 %>% 
  mutate(highlight = ifelse(
    NAME_1 %in% c(
      "Andhra Pradesh",
      "Himachal Pradesh",
      "Telangana",
      "Madhya Pradesh",
      "West Bengal",
      "Jharkhand",
      "Odisha"
    ),
    1,
    ifelse(
      NAME_1 %in% c(
        "Chhattisgarh",
        "Bihar",
        "Uttar Pradesh"
        
      ),
      2,0
    )
    )
    ) %>% 
  st_as_sf()

r <- intake_aggregate(adm2, lysine_g)
# 2218 with lowest

ind_nsso1112_hh_info %>% 
  filter(adm2 == 2218)

tm_shape(x) +
  tm_fill(col = "highlight",
          legend.show =  FALSE
          # , 
          #breaks = seq(0,1,by=.10),
          # palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          # title = title_name ,
          # legend.is.portrait = FALSE
  ) 
  