
#########################################
#          NAR and MAR Nigeria          #
#########################################


# Author: Gabriel Battcock
# Created: 29 May 24
# Last updated: 29 May 24

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

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

# Read in data ----------------------------------------------------------------


nga1819 <- apparent_intake("nga_lss1819")
nga1819_hh_info <- household_data("nga_lss1819")
nga_lga_dict <- read.csv("../nigeria_mapping/data_dictionary/lga.csv")
hh_to_lga <-as_tibble(read.csv("../nigeria_mapping/hh_to_lga.csv"))
nga_adm2 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_2.shp"))
nga_adm1 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_1.shp"))

# Nigeria ----------------------------------------------------------------------


plot(nga_adm1$geometry)
allen_ear <- data.frame(
  energy_kcal = 2200,#who
  vita_rae_mcg  = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg = 9.6, #moderate absorption
  ca_mg = 860, 
  zn_mg = 11#semi unrefined
)


library(dplyr)



nga1819_hh_info <- nga1819_hh_info %>% 
  left_join(hh_to_lga %>% dplyr::select(hhid, lga), by ="hhid") %>% 
  dplyr::select(hhid,survey_wgt,lga,ea)


nga_sw_mean_intake <- nga1819 %>% 
  dplyr::left_join(
    nga1819_hh_info , by = "hhid"
  ) %>% 
  dplyr::mutate(
    vita_rae_mcg = ifelse(vita_rae_mcg<allen_ear$vita_rae_mcg, 
                          vita_rae_mcg/allen_ear$vita_rae_mcg,
                          1),
    thia_mg = ifelse(thia_mg<allen_ear$thia_mg, 
                     thia_mg/allen_ear$thia_mg,
                     1),
    ribo_mg = ifelse(ribo_mg<allen_ear$ribo_mg, 
                     ribo_mg/allen_ear$ribo_mg,
                     1),
    niac_mg = ifelse(niac_mg<allen_ear$niac_mg, 
                     niac_mg/allen_ear$niac_mg,
                     1),
    vitb6_mg = ifelse(vitb6_mg<allen_ear$vitb6_mg, 
                      vitb6_mg/allen_ear$vitb6_mg,
                      1),
    folate_mcg = ifelse(folate_mcg<allen_ear$folate_mcg, 
                        folate_mcg/allen_ear$folate_mcg,
                        1),
    vitb12_mcg = ifelse(vitb12_mcg<allen_ear$vitb12_mcg, 
                        vitb12_mcg/allen_ear$vitb12_mcg,
                        1),
    fe_mg = ifelse(fe_mg<allen_ear$fe_mg, 
                   fe_mg/allen_ear$fe_mg,
                   1),
    ca_mg= ifelse(ca_mg<allen_ear$ca_mg, 
                  ca_mg/allen_ear$ca_mg,
                  1),
    zn_mg = ifelse(zn_mg<allen_ear$zn_mg, 
                   zn_mg/allen_ear$zn_mg,
                   1)
    
  ) %>%
  dplyr::mutate(mar = (vita_rae_mcg+thia_mg+ribo_mg+
                         niac_mg+vitb6_mg+folate_mcg+
                         vitb12_mcg+ ca_mg+fe_mg+zn_mg)/10) %>% 
  srvyr::as_survey_design(id = hhid, strata = ea,
                          weights = survey_wgt, nest=T)


#create median values
nga_sw_mean_intake_lga_tot <- nga_sw_mean_intake %>% 
  srvyr::group_by(lga) %>% 
  srvyr::summarise(
    srvyr::across(-c(hhid,afe, survey_wgt),
                  ~median(.))
  ) %>% 
  ungroup() %>% 
  dplyr::left_join(
    nga_adm2, 
    by = c("lga")
  ) %>% 
  dplyr::ungroup() %>% 
  # dplyr::filter(!is.na(shapeID)) %>% 
  st_as_sf()


# energy
nga_energy_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "energy_kcal", style = "cont", breaks = c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Energy (kcal)")+
  tm_layout(main.title =  "Energy", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_energy_adm2
tmap_save(eth_oil, paste0(file_path, "eth_reach_edible_oil.png"),
          width = 9, height = 9, units = "in", dpi = 600)

nga_vita_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vita_rae_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin A", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_vita_adm2

nga_thia_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "thia_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Thiamin", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_thia_adm2

nga_ribo_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "ribo_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Riboflavin", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_ribo_adm2

nga_niac_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "niac_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Niacin", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F) +
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_niac_adm2

nga_vb6_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vitb6_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin B6", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F) + 
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_vb6_adm2

nga_fol_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "folate_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Folate", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)


nga_fol_adm2

nga_vb12_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vitb12_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin B12", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)

nga_vb12_adm2

nga_ca_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "ca_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Calcium", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)

nga_ca_adm2

nga_fe_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "fe_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Iron", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)

nga_fe_adm2

nga_zn_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "zn_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Zinc", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F) +
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)

nga_zn_adm2


legend_nar <- tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "zn_mg",
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          breaks = seq(0,1,by=.10),
          style = "cont",
          textNA = "Missing Data",
          title = "Nutrient Adequacy Ratio", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))

tmap_save(legend_nar, paste0(file_path, "legend_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

# MAR --------------------------------------------------------------------------

nga_mar_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "mar", style = "cont", breaks = seq(0.3,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Mean Adequacy \n Ratio")+
  tm_layout(main.title =  "Mean adequacy ratio", 
            frame = F,
            main.title.size = 0.8) +
  # tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)+
  tm_credits("Source: Nigerian Living Standards Survey 2018-19",
             position = 'left',
             size = 0.5)

nga_mar_adm2

legend_mar <- tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "mar",
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          breaks = seq(0.3,1,by=.10),
          style = "cont",
          textNA = "Missing Data",
          title = "Mean Adequacy Ratio", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))

tmap_save(legend_mar, paste0(file_path, "legend_mar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

# save the files
file_path <- here::here("data_rich/data_requests/nga_nar_202405/maps//")


tmap_save(nga_vita_adm2, paste0(file_path, "nga_vita_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_thia_adm2, paste0(file_path, "nga_thia_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_ribo_adm2, paste0(file_path, "nga_ribo_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_niac_adm2, paste0(file_path, "nga_niac_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_vb6_adm2, paste0(file_path, "nga_vitb6_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

ttmap_save(nga_fol_adm2, paste0(file_path, "nga_fol_nar.png"),
           width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_vb12_adm2, paste0(file_path, "nga_vitb12_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_fe_adm2, paste0(file_path, "nga_fe_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_ca_adm2, paste0(file_path, "nga_ca_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_zn_adm2, paste0(file_path, "nga_zn_nar.png"),
          width = 9, height = 9, units = "in", dpi = 600)

tmap_save(nga_mar_adm2, paste0(file_path, "nga_mar.png"),
          width = 9, height = 9, units = "in", dpi = 600)
s