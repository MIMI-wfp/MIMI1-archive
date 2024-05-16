#########################################
#               BMGF maps               #
#########################################

# Author: Gabriel Battcock
# Created: 15 May 24
# Last updated: 15 May 24

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

# INDIA ------------------------------------------------------------------------

# read in nsso clean data
file_path <- here::here("~/Documents/MIMI/MIMI_data/data_requests/bmgf_052024/")

# ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
# food_consumption<- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
# fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))

ind_cnns16 <- read.csv(paste0(file_path, "ind_cnns_vmd.csv"))
india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read("~/Documents/MIMI/MIMI_data/India/gadm41_IND_shp/gadm41_IND_1.shp")


ind_states <-  india_adm1 %>%
  # left_join(adm1_nsso_link, by =c("GID_1", "NAME_1")) %>%
  group_by(NAME_1) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  slice(1)



ind_cnns16 <- ind_cnns16 %>% 
  mutate(
    NAME_1 = case_when(
      X == "Delhi" ~ "NCT of Delhi",
      X == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      .default = X
    )
  )

## create shape file

ind_cnns_sp <- ind_states %>% 
  inner_join(ind_cnns16, by = "NAME_1") %>% 
  st_as_sf() 
  







cnns_iron <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "iron",style = "cont", breaks = seq(0,100,by=10),
        palette = (wesanderson::wes_palette("Zissou1Continuous")),
        title = "Prevalence of VMD" ,
        legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Iron",  frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)


tmap_save(cnns_iron, paste0(file_path, "cnns_iron.png"),
          width = 9, height = 9, units = "in", dpi = 600)

cnns_zinc <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "zinc",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Zinc",  frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+ +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)


tmap_save(cnns_zinc, paste0(file_path, "cnns_zinc.png"),
          width = 9, height = 9, units = "in", dpi = 600)

cnns_vita <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_a",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin A",  frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+ 
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)

tmap_save(cnns_vita, paste0(file_path, "cnns_vita.png"),
          width = 9, height = 9, units = "in", dpi = 600)

cnns_folate <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "folate",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Folate",  frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+ 
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)

tmap_save(cnns_folate, paste0(file_path, "cnns_folate.png"),
          width = 9, height = 9, units = "in", dpi = 600)


cnns_vitb12 <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_b12",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin B12", frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+ 
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)

tmap_save(cnns_vitb12, paste0(file_path, "cnns_vitb12.png"),
          width = 9, height = 9, units = "in", dpi = 600)

cnns_vitd <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_d",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin D", frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+ 
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Indian Comprehensive Nation Nutrition Survey 2016-18",
             position = 'left',
             size = 0.5)

tmap_save(cnns_vitd, paste0(file_path, "cnns_vitd.png"),
          width = 9, height = 9, units = "in", dpi = 600)



# Legend: 
legend <- tm_shape(ind_cnns_sp) + 
  tm_fill(col = "iron",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))


tmap_save(legend, paste0(file_path, "legend.png"),
          width = 9, height = 9, units = "in", dpi = 600)

# ETHIOPIA ---------------------------------------------------------------------

eth_nns <- read.csv(paste0(file_path,"eth_nns22.csv"))
eth_adm1 <-  st_read("../MIMI_data/shapefile_data/eth_adm_csa_bofedb_2021_shp/eth_admbnda_adm1_csa_bofedb_2021.shp")


eth_adm1 <- eth_adm1 %>% 
  mutate(
    adm1 = case_when(
      ADM1_EN == "Tigray" ~ "Tigray",
      ADM1_EN == "Afar" ~ "Afar",
      ADM1_EN == "Amhara" ~ "Amhara",
      ADM1_EN == "Oromia"~ "Oromia",
      ADM1_EN == "Somali" ~ "Somalia",
      ADM1_EN == "Benishangul Gumz" ~ "Benishangul-Gumuz",
      ADM1_EN == "SNNP" ~ "SNNP",
      ADM1_EN == "Gambela" ~ "Gambela",
      ADM1_EN == "Harari" ~ "Harari", 
      ADM1_EN == "Addis Ababa" ~ "Addis", 
      ADM1_EN == "Dire Dawa" ~ "Dire Dawa",
      ADM1_EN == "Sidama" ~ "Sidama",
      ADM1_EN == "South West Ethiopia" ~ "SNNP"
    )
  ) %>% 
  select(adm1, geometry) %>% 
  group_by(adm1) %>% 
  mutate(geometry = sf::st_union(geometry)) %>%
  slice(1) 

eth_nns_sp <- eth_adm1 %>% 
  left_join(eth_nns, by = c('adm1' = "zone")) %>% 
  st_as_sf()

# maps

### VMD

# vitamin d
eth_vitd <- tm_shape(eth_nns_sp)+
  tm_fill(col = "vitamin_d",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD (preschool children)" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin D", 
            frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_vitd, paste0(file_path, "eth_vitd.png"),
          width = 9, height = 9, units = "in", dpi = 600)
            
# iron

eth_iron <- tm_shape(eth_nns_sp)+
  tm_fill(col = "iron",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD (WRA)" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Iron", 
            frame = F, 
            main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)


tmap_save(eth_iron, paste0(file_path, "eth_iron.png"),
          width = 9, height = 9, units = "in", dpi = 600)


eth_vita <- tm_shape(eth_nns_sp)+
  tm_fill(col = "vitamin_a",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD (WRA)" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin A", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  )+
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_vita, paste0(file_path, "eth_vita.png"),
          width = 9, height = 9, units = "in", dpi = 600)


eth_fol <- tm_shape(eth_nns_sp)+
  tm_fill(col = "folate",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Folate", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_fol, paste0(file_path, "eth_fol.png"),
          width = 9, height = 9, units = "in", dpi = 600)

eth_vitb12 <- tm_shape(eth_nns_sp)+
  tm_fill(col = "vitamin_b12",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin B12", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_vitb12, paste0(file_path, "eth_vitb12.png"),
          width = 9, height = 9, units = "in", dpi = 600)

# Legend: 
legend <- tm_shape(eth_nns_sp) + 
  tm_fill(col = "iron",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))



# reach of vehices
eth_wheat <- tm_shape(eth_nns_sp) +
  tm_fill(col = "wheatflour",
        title = "Reach (%)", 
        # palette = viridis(10, direction = -1), 
        style = "cont",
        textNA = "Missing Data",
        legend.is.portrait = F) + 
  tm_layout(main.title = "Wheat Flour", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)



tmap_save(eth_wheat, paste0(file_path, "eth_wheat.png"),
          width = 9, height = 9, units = "in", dpi = 600)

eth_oil <- tm_shape(eth_nns_sp) +
  tm_fill(col = "edible_oil",
          title = "Reach (%)", 
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Edible Oil", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_oil, paste0(file_path, "eth_oil.png"),
          width = 9, height = 9, units = "in", dpi = 600)

eth_salt <- tm_shape(eth_nns_sp) +
  tm_fill(col = "salt",
          title = "Reach (%)", 
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Salt", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopia National Food and Nutrition Survey 2021-22",
             position = 'left',
             size = 0.5)

tmap_save(eth_salt, paste0(file_path, "eth_salt.png"),
          width = 9, height = 9, units = "in", dpi = 600)


legend_reach <- tm_shape(eth_nns_sp) + 
  tm_fill(col = "salt",
          # palette = viridis(10, direction = -1), 
          breaks = seq(0,100,by=10),
          style = "cont",
          textNA = "Missing Data",
          title = "Fortification vehicle coverage (%)", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))

tmap_save(legend_reach, paste0(file_path, "reach_legend.png"),
          width = 9, height = 9, units = "in", dpi = 600)


# ethiopia inadequacy

eth_hices <- apparent_intake("eth_hices1516")
eth_hh_info <- household_data("eth_hices1516")

eth_adm1_hices <- st_read("../MIMI_data/Ethiopia/gadm41_ETH_shp/gadm41_ETH_1.shp")

eth_adm1_hices <- eth_adm1_hices %>% mutate(
  adm1 = case_when(
    NAME_1 == "Tigray" ~ "Tigray",
    NAME_1 == "Afar" ~ "Afar",
    NAME_1 == "Amhara" ~ "Amhara",
    NAME_1 == "Oromia"~ "Oromiya",
    NAME_1 == "Somali" ~ "Somali",
    NAME_1 == "Benshangul-Gumaz" ~ "Benshangul",
    NAME_1 == "Southern Nations, Nationalities" ~ "SNNP",
    NAME_1 == "Gambela Peoples" ~ "Gambella",
    NAME_1 == "Harari People" ~ "Harari", 
    NAME_1 == "Addis Abeba" ~ "Addis Ababa", 
    NAME_1 == "Dire Dawa" ~ "Dire Dawa"
  )
) %>% 
  select(adm1, geometry)

hices_inad <- eth_hices %>% 
  select(hhid, vita_rae_mcg, fe_mg, folate_mcg, vitb12_mcg,zn_mg) %>% 
  mutate(
  va_ref =   allen_ear$ear_value[allen_ear$nutrient == "vita_rae_mcg"],
     fo_ref = allen_ear$ear_value[allen_ear$nutrient == "folate_mcg"],
    vb12_ref = allen_ear$ear_value[allen_ear$nutrient == "vitb12_mcg"],
    fe_ref =  allen_ear$ear_value[allen_ear$nutrient == "fe_mg"],
    zn_ref = allen_ear$ear_value[allen_ear$nutrient == "zn_mg"],
    va_inad = ifelse(vita_rae_mcg<va_ref, 1,0),
    fo_inad = ifelse(folate_mcg<fo_ref, 1,0),
    vb12_inad = ifelse(vitb12_mcg<vb12_ref,1,0),
    fe_inad = ifelse(fe_mg<fe_ref, 1,0),
    zn_inad = ifelse(zn_mg<zn_ref, 1,0)
) %>%
  left_join(eth_hh_info %>% select(hhid,adm1,survey_wgt) %>% 
              distinct(hhid,.keep_all = TRUE), by='hhid') %>% 
  as_survey_design(ids = hhid, strata = adm1, weights = survey_wgt) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    va_inad =srvyr::survey_mean(
      va_inad == 1, proportion = TRUE,
      na.rm = T
    )*100,
    fo_inad =  srvyr::survey_mean(
      fo_inad == 1, proportion = TRUE,
      na.rm = T
    )*100,
    fe_inad = srvyr::survey_mean(
      fe_inad == 1, proportion = TRUE,
      na.rm = T
    )*100,
    zn_inad  = srvyr::survey_mean(
      zn_inad == 1, proportion = TRUE,
      na.rm = T
    )*100,
    vb12_inad = srvyr::survey_mean(
      vb12_inad == 1, proportion = TRUE,
      na.rm = T
    )*100
  )


hices_inad_sp <- hices_inad %>% left_join(eth_adm1_hices, by = "adm1") %>% 
  st_as_sf()

hices_vita <- tm_shape(hices_inad_sp)+
  tm_fill(col = "va_inad",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin A", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(hices_vita, paste0(file_path, "hiecs_vita.png"),
          width = 9, height = 9, units = "in", dpi = 600)



hices_fo <- tm_shape(hices_inad_sp)+
  tm_fill(col = "fo_inad",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Folate", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(hices_fo, paste0(file_path, "hiecs_fo.png"),
          width = 9, height = 9, units = "in", dpi = 600)


hices_fe <- tm_shape(hices_inad_sp)+
  tm_fill(col = "fe_inad",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Iron", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(hices_fe, paste0(file_path, "hiecs_iron.png"),
          width = 9, height = 9, units = "in", dpi = 600)


hices_zn <- tm_shape(hices_inad_sp)+
  tm_fill(col = "zn_inad",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Zinc", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(hices_zn, paste0(file_path, "hiecs_zinc.png"),
          width = 9, height = 9, units = "in", dpi = 600)
  
hices_b12 <- tm_shape(hices_inad_sp)+
  tm_fill(col = "vb12_inad",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin B12", 
            frame = F, main.title.size = 0.8, 
            main.title.position = "center",
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(hices_b12, paste0(file_path, "hiecs_b12.png"),
          width = 9, height = 9, units = "in", dpi = 600)



legend_inad_intake <- tm_shape(hices_inad_sp) + 
  tm_fill(col = "va_inad",
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          breaks = seq(0,100,by=10),
          style = "cont",
          textNA = "Missing Data",
          title = "Prevalence of inadequate micronutrient intake (%)", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))

tmap_save(legend_inad_intake, paste0(file_path, "inad_intake_legend.png"),
          width = 9, height = 9, units = "in", dpi = 600)

