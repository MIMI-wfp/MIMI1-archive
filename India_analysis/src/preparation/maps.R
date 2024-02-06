## Map building for 

# load(here::here("India_analysis/src/processing/data_validation"))
library(tmap)
library(tidyr)
library(readr)
library(sf)
library(rmapshaper)
library(readxl)
library(here)
library(ggplot2)
library(hrbrthemes)
library(wesanderson)
library(srvyr)
library(treemap)
library(treemapify)
library(ggridges)
library(gt)


source(here::here("all_base_models/scripts/base_model_functions.R"))
source(here::here("dietary_assessment/processing/individual_level_clean.R"))

# India ------------------------------------------------------------------------

base_model <- read_csv(here::here("India_analysis/data/final/extra_states/base_model.csv"))
india_adm2 <- st_read(here::here("India_analysis/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("India_analysis/data/processed/state_shape.shp"))
household_characteristics <- read_csv(paste0("India_analysis/data/processed/extra_states/household_char.csv"))
# x <-  india_adm2%>% dplyr::anti_join(base_model, by = c("Dstrct_c" = "District_code"))

# create average intakes at adm2 level 
nin_ear <- data.frame(
  energy_kcal = 2130,
  vita_mg  = 390, 
  vitb1_mg = 1.4,
  vitb2_mg = 2.0, 
  vitb3_mg = 12, 
  vitb5_mg = 4,#from allen 2020
  vitb6_mg = 1.6, 
  folate_ug = 180, 
  vitaminb12_in_mg = 2, 
  iron_mg = 15, 
  calcium_mg = 800, 
  zinc_mg = 11
)
unique(base_model$State_code)

sw_mean_intake <- base_model %>% 
  dplyr::left_join(
    household_characteristics %>% 
      dplyr::select(
        HHID, Combined_multiplier, HH_Type_code
      ), by = "HHID"
  ) %>% 
  dplyr::mutate(
    energy_kcal = ifelse(energy_kcal<nin_ear$energy_kcal, 
                         energy_kcal/nin_ear$energy_kcal,
                     1),
    vita_mg = ifelse(vita_mg<nin_ear$vita_mg, 
                     vita_mg/nin_ear$vita_mg,
                         1),
    vitb1_mg = ifelse(vitb1_mg<nin_ear$vitb1_mg, 
                      vitb1_mg/nin_ear$vitb1_mg,
                         1),
    vitb2_mg = ifelse(vitb2_mg<nin_ear$vitb2_mg, 
                      vitb2_mg/nin_ear$vitb2_mg,
                         1),
    vitb3_mg = ifelse(vitb3_mg<nin_ear$vitb3_mg, 
                      vitb3_mg/nin_ear$vitb3_mg,
                         1),
    vitb5_mg = ifelse(vitb5_mg<nin_ear$vitb5_mg, 
                      vitb5_mg/nin_ear$vitb5_mg,
                         1),
    vitb6_mg = ifelse(vitb6_mg<nin_ear$vitb6_mg, 
                      vitb6_mg/nin_ear$vitb6_mg,
                         1),
    folate_ug = ifelse(folate_ug<nin_ear$folate_ug, 
                       folate_ug/nin_ear$folate_ug,
                         1),
    vitaminb12_in_mg = ifelse(vitaminb12_in_mg<nin_ear$vitaminb12_in_mg, 
                              vitaminb12_in_mg/nin_ear$vitaminb12_in_mg,
                         1),
    iron_mg = ifelse(iron_mg<nin_ear$iron_mg, 
                     iron_mg/nin_ear$iron_mg,
                         1),
    calcium_mg= ifelse(calcium_mg<nin_ear$calcium_mg, 
                       calcium_mg/nin_ear$calcium_mg,
                       1),
    zinc_mg = ifelse(zinc_mg<nin_ear$zinc_mg, 
                     zinc_mg/nin_ear$zinc_mg,
                    1)
  
  ) %>% 
  dplyr::mutate(mar = (vita_mg+vitb1_mg+vitb2_mg+
                         vitb3_mg+vitb6_mg+folate_ug+
                         vitaminb12_in_mg+ calcium_mg+iron_mg+zinc_mg)/10) %>%
  srvyr::as_survey_design(id = HHID, strata = District_code,
                          weights = Combined_multiplier, nest=T)

intake <- base_model %>%
  dplyr::left_join(
    household_characteristics %>% 
      dplyr::select(
        HHID, Combined_multiplier, HH_Type_code
      ), by = "HHID"
  ) %>% 
  srvyr::as_survey_design(id = HHID, strata = District_code,
                          weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(District_code) %>% 
  srvyr::summarise(
    srvyr::across(-c(HHID,State_code, State_name,Combined_multiplier,HH_Type_code),
                  ~mean(.))
  ) %>% 
  dplyr::mutate(District_code = as.numeric(District_code)) %>% 
  dplyr::left_join(
    india_adm2, 
    by = c("District_code" = "Dstrct_c")
  ) %>% 
  dplyr::ungroup() %>% 
  st_as_sf()

sw_mean_intake_district_tot <- sw_mean_intake %>% 
  srvyr::group_by(District_code) %>% 
  srvyr::summarise(
    srvyr::across(-c(HHID,State_code, State_name,Combined_multiplier,HH_Type_code),
                  ~median(.))
  ) %>% 
  dplyr::mutate(District_code = as.numeric(District_code)) %>% 
  dplyr::left_join(
    india_adm2, 
    by = c("District_code" = "Dstrct_c")
  ) %>% 
  dplyr::ungroup() %>% 
  st_as_sf()

sw_mean_intake_district_urb_rural <- sw_mean_intake %>%
  srvyr::group_by(District_code, HH_Type_code) %>% 
  srvyr::summarise(
    srvyr::across(-c(HHID,State_code, State_name,Combined_multiplier),
                  ~mean(.))
  ) %>% 
  dplyr::mutate(District_code = as.numeric(District_code)) %>% 
  dplyr::left_join(
    india_adm2, 
    by = c("District_code" = "Dstrct_c")
  ) %>% 
  st_as_sf()
  


# showing which states we are using

india_sp <- india_adm1 %>%
  dplyr::mutate(
    state = ifelse(NAME_1%in%c("Uttar Pradesh", "Bihar", "Chhattisgarh", 
                               "Odisha"
                               ),
                   1,
                   0
  )) %>% 
  st_as_sf()



states_sp <- india_sp %>% 
  dplyr::filter(state == 1)

# Create maps: 
  india_co <- tm_shape(india_sp) + 
    tm_fill(col = "state") +
    tm_layout(main.title = "", frame = F,
              main.title.size = 0.8) +
    tm_borders(col = "black", lwd = 0.7) +
    tm_legend(show = F)

  state_outline <- tm_shape(states_sp) + 
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1) +
    tm_legend(show = F)
  

# rm(breaks)
# 
energy_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "energy_kcal", style = "cont", breaks = seq(0,1,by=.10),
            # c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Energy (kcal)")+
  tm_layout(main.title =  "Energy", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

energy_adm2

# energy_adm2_cont <- tm_shape(intake) + 
#   tm_fill(col = "energy_kcal", style = "cont",
#           palette = "Blues")+
#   tm_layout(main.title =  "Energy", 
#             frame = F,
#             main.title.size = 0.8) +
#   tm_borders(col = "black", lwd = 0.5)+
#   tm_legend(show = T)
# 
# energy_adm2

# tmap_save(energy_adm2, here::here("../energy_adm2.png"))

#
vita_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vita_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = 
              "Vitamin A", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vita_adm2

# tmap_save(vita_adm2, here::here("../vita_adm2.png"))
#  
vitb1_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitb1_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = "Thiamin", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb1_adm2


vitb2_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitb2_mg",style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = "Riboflavin", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb2_adm2


vitb3_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitb3_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = "Niacin", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb3_adm2



vitb5_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitb5_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = "Vitamin B5", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb5_adm2


vitb6_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitb6_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title = "Vitamin B6", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb6_adm2


folate_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "folate_ug", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio") +
  tm_layout(main.title = "Folate", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

folate_adm2

 # tmap_save(folate_adm2, here::here("../folate_adm2.png"))

vitb12_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "vitaminb12_in_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio") +
  tm_layout(main.title = "Vitamin B12", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

vitb12_adm2

 # tmap_save(vitb12_adm2, here::here("../vitb12_adm2.png"))

iron_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "iron_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio") +
  tm_layout(main.title = "Iron", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

iron_adm2
 # tmap_save(iron_adm2, here::here("../iron_adm2.png"))

calcium_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "calcium_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio") +
  tm_layout(main.title = "Calcium", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

calcium_adm2

zinc_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "zinc_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio") +
  tm_layout(main.title = "Zinc", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+ 
  tm_legend(show = F)

zinc_adm2

mar_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "mar", style = "cont", breaks = seq(0.4,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Mean Adequacy \n Ratio") +
  tm_layout(main.title = "Mean Adequacy Ratio", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+ 
  tm_legend(show = T)

mar_adm2



# tmap_save(zinc_adm2, here::here("../zinc_adm2.png"))

# legend: 
legend_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "zinc_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous", type = "continuous")),
          title = "Nurtient Adequacy Ratio",
          # labels = c("),
          textNA = "Missing Data",
          colorNA = "gray35",
          legend.is.portrait = FALSE) +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 50,
            # legend.height = 
            ) 
# +
  # tm_compass(north = 0, type = "arrow")

legend_adm2


# Integrate these mapsand legend into a single figure: 

nar_adm2 <- list(energy_adm2, vita_adm2, vitb1_adm2, vitb2_adm2,
                 vitb3_adm2,vitb5_adm2,vitb6_adm2,folate_adm2,vitb12_adm2,
                 iron_adm2,calcium_adm2,zinc_adm2,legend_adm2,india_co)

nar_adm2 <- tmap_arrange(nar_adm2, ncol = 4, sync = TRUE)

nar_adm2

tmap_save(nar_adm2, "nar_adm2.png", height = 4.5, width = 7)



# to have a mixed figure of maps and ggplot, need to grob() each tmap
# energy_adm2_grob <-tmap_grob(energy_adm2)
# vita_adm2_grob <- tmap_grob(vita_adm2)
# folate_adm2_grob <- tmap_grob(folate_adm2)
# vitb12_adm2_grob <- tmap_grob(vitb12_adm2)
# iron_adm2_grob <- tmap_grob(iron_adm2)
# zinc_adm2_grob <- tmap_grob(zinc_adm2)
# legend_grob <- tmap_grob(legend_adm2)
# india_co_grob <- tmap_grob(india_co)


# post_plotlist <- list(energy_adm2_grob,energy_fg,vita_adm2_grob,vita_fg,
#                       folate_adm2_grob,folate_fg,vitb12_adm2_grob,vitb12_fg,
#                       iron_adm2_grob,fe_fg,zinc_adm2_grob,zn_fg)

# energy_fg 
# vita_fg 
# folate_fg 
# vitb12_fg 
# fe_fg 
# zn_fg 
# 
# energy_comb <- cowplot::plot_grid(energy_adm2_grob,energy_fg, tit)
# vita_comb <- cowplot::plot_grid(vita_adm2_grob,vita_fg)
# folate_comb <- cowplot::plot_grid(folate_adm2_grob,folate_fg)
# vitb12_comb <- cowplot::plot_grid(vitb12_adm2_grob,vitb12_fg)
# iron_comb <- cowplot::plot_grid(iron_adm2_grob,fe_fg)
# zinc_comb <- cowplot::plot_grid(zinc_adm2_grob,zn_fg)
# 
# 
#  arranged <- ggpubr::ggarrange(energy_comb,vita_comb,
#                   folate_comb,vitb12_comb,
#                   iron_comb,zinc_comb,
#                   legend_grob, legend_fg, india_co_grob)
# 
# ggsave(arranged, "arranged_plot.png")
# 
# cowplot::plot_grid(plotlist = post_plotlist)
# ggpubr::ggarrange(energy_adm2_grob,energy_fg,vita_adm2_grob,vita_fg,
#                    folate_adm2_grob,folate_fg,vitb12_adm2_grob,vitb12_fg,
#                    iron_adm2_grob,fe_fg,zinc_adm2_grob,zn_fg
#                   )

# Nigeria ----------------------------------------------------------------------

nga1819 <- apparent_intake("nga_lss1819")
nga1819_hh_info <- household_data("nga_lss1819")
nga_lga_dict <- read.csv("../nigeria_mapping/data_dictionary/lga.csv")
hh_to_lga <-as_tibble(read.csv("../nigeria_mapping/hh_to_lga.csv"))
nga_adm2 <- st_read(here::here("../nigeria_mapping/map_data/new_shapefiles/nigeria_2.shp"))
nga_adm1 <- st_read(here::here("../nigeria_mapping/map_data/new_shapefiles/nigeria_1.shp"))
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
  dplyr::select(hhid,survey_wgt,lga)


nga_sw_mean_intake <- nga1819 %>% 
  dplyr::left_join(
    nga1819_hh_info, by = "hhid"
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
  srvyr::as_survey_design(id = hhid, strata = lga,
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
    by = "lga"
  ) %>% 
  dplyr::ungroup() %>% 
  # dplyr::filter(!is.na(shapeID)) %>% 
  st_as_sf()


# energy
nga_energy_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "energy_kcal", style = "cont", breaks = c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Energy (kcal)")+
  tm_layout(main.title =  "Energy", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_energy_adm2


nga_vita_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray35")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vita_rae_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nurtient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin A", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_vita_adm2

nga_thia_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "thia_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Thiamin", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_thia_adm2

nga_ribo_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "ribo_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Riboflavin", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_ribo_adm2

nga_niac_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "niac_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Niacin", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_niac_adm2

nga_vb6_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vitb6_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin B6", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_vb6_adm2

nga_fol_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "folate_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Folate", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


nga_fol_adm2

nga_vb12_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "vitb12_mcg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Vitamin B12", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

nga_vb12_adm2

nga_ca_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "ca_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Calcium", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

nga_ca_adm2

nga_fe_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "fe_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Iron", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

nga_fe_adm2

nga_zn_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "zn_mg", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Nutrient Adequacy \n Ratio")+
  tm_layout(main.title =  "Zinc", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

nga_zn_adm2

nga_mar_adm2 <- tm_shape(nga_adm2) +
  tm_fill(palette = "gray")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_sw_mean_intake_lga_tot) + 
  tm_fill(col = "mar", style = "cont", breaks = seq(0.4,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Mean Adequacy \n Ratio")+
  tm_layout(main.title =  "Mean adequacy ratio", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

nga_mar_adm2


