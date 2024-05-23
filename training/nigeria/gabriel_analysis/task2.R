
library(sf)
library(tmap)
library(hrbrthemes)


### Fortification values
# mcg/kg

wheatflour_fort_min <- 2000
maizeflour_fort_min <- 2000
edible_oil_fort_min <- 6000
sugar_fort_min <- 7500

wheatflour_fort_max <- 2000*2
maizeflour_fort_max <- 2000*2
edible_oil_fort_max<- 6000*2
sugar_fort_max <- 7500*2

# read in the data
nga_lss1819 <- read.csv('~/Desktop/nga_lss1819_analysis_df.csv')

# using the base case, model the distribution of vitamin A ----------------------

nga_lss1819 %>% 
  ggplot(aes(x= vita_rae_mcg))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) No Fortification",
       )


nga_lss1819_wheat_fort <- nga_lss1819 %>% 
  select(hhid, vita_rae_mcg, wheatflour_100g) %>% 
  mutate(wheatflour_100g = ifelse(is.na(wheatflour_100g),0,wheatflour_100g),
         va_wheatflour = (wheatflour_100g/10)*wheatflour_fort,
        vita_rae_mcg_fort_wheat = vita_rae_mcg+va_wheatflour)



# fortification <- function(vehicle, value){
#   nga_lss1819 %>% 
#     select(hhid, vita_rae_mcg, {{vehicle}}) %>% 
#     mutate({{vehicle}} = ifelse(is.na(as.numeric({{vehicle}})),0.0,as.numeric({{vehicle}})),
#            va_vehicle = ({{vehicle}}/10)*value,
#           "{{vehicle}}_vita_rae" := vita_rae_mcg+va_vehicle)
# }

# fortification("wheatflour_100g", 2000)



nga_lss1819_wheat_fort %>% 
  ggplot(aes(x= vita_rae_mcg_fort_wheat))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) Wheat flour fortification",
  )



# 
# for(name in list('maizeflour_100g')){
#   nga_lss1819_maize_fort <- nga_lss1819 %>% 
#     select(hhid, vita_rae_mcg, name) %>% 
#     mutate(name = ifelse(is.na({{name}}),0,{{name}}),
#            va_add = ({{name}}/10)*2000,
#            vita_rae_mcg_fort = vita_rae_mcg+va_add)
# }


nga_lss1819_maize_fort <- nga_lss1819 %>% 
  select(hhid, vita_rae_mcg, maizeflour_100g) %>% 
  mutate(maizeflour_100g = ifelse(is.na(maizeflour_100g),0,maizeflour_100g),
         va_mazieflour = (maizeflour_100g/10)*maizeflour_fort,
         vita_rae_mcg_fort_maize = vita_rae_mcg+va_mazieflour)


nga_lss1819_maize_fort %>% 
  ggplot(aes(x= vita_rae_mcg_fort_maize))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) Wheat flour fortification",
  )


# oil

nga_lss1819_oil_fort <- nga_lss1819 %>% 
  select(hhid, vita_rae_mcg, edible_oil_100g) %>% 
  mutate(edible_oil_100g = ifelse(is.na(edible_oil_100g),0,edible_oil_100g),
         va_edibleoil = (edible_oil_100g/10)*edible_oil_fort,
         vita_rae_mcg_fort_oil = vita_rae_mcg+va_edibleoil)

nga_lss1819_oil_fort %>% 
  ggplot(aes(x= vita_rae_mcg_fort_oil))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) oil fortification",
  )

#sugar
nga_lss1819_sugar_fort <- nga_lss1819 %>% 
  select(hhid, vita_rae_mcg, sugar_100g) %>% 
  mutate(sugar_100g = ifelse(is.na(sugar_100g),0,sugar_100g),
         va_sugar = (sugar_100g/10)*sugar_fort,
         vita_rae_mcg_fort_sugar = vita_rae_mcg+va_sugar)

nga_lss1819_sugar_fort %>% 
  ggplot(aes(x= vita_rae_mcg_fort_sugar))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) sugar fortification",
  )

nga_lss_1819_fort <- nga_lss1819 %>% 
  select(hhid, vita_rae_mcg) %>% 
  left_join(nga_lss1819_wheat_fort %>% 
              select(hhid,va_wheatflour),by = "hhid") %>% 
  left_join(nga_lss1819_maize_fort %>% 
              select(hhid,va_mazieflour),by = "hhid") %>% 
  left_join(nga_lss1819_oil_fort %>% 
              select(hhid,va_edibleoil),by = "hhid") %>% 
  left_join(nga_lss1819_sugar_fort %>% 
              select(hhid,va_sugar),by = "hhid") %>% 
  mutate(vita_rae_mcg_tot_fort = vita_rae_mcg+va_wheatflour+va_mazieflour+va_edibleoil+va_sugar,
         vita_rae_mcg_fort_wf = vita_rae_mcg+va_wheatflour,
         vita_rae_mcg_fort_mf = vita_rae_mcg+va_mazieflour,
         vita_rae_mcg_fort_ed = vita_rae_mcg+va_edibleoil,
         vita_rae_mcg_fort_su = vita_rae_mcg+va_sugar)


nga_lss_1819_fort_rp %>% 
  summarise(across
          (
            c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
              vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
            ~max(.x,na.rm = TRUE)
          ))
nga_lss_1819_fort %>% 
  
  tidyr::pivot_longer(cols = c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
                               vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su)) %>% 
  mutate(name = factor(case_when(name == "vita_rae_mcg" ~ "Base case",
                   name == "vita_rae_mcg_tot_fort" ~ "All vehicles",
                   name == "vita_rae_mcg_fort_mf" ~ "Maize flour",
                   name == "vita_rae_mcg_fort_wf" ~ "Wheat flour",
                   name == "vita_rae_mcg_fort_ed" ~ "Edible oil",
                   name == "vita_rae_mcg_fort_su" ~ "Sugar"),
                   levels = c("Base case", "Maize flour", "Wheat flour",
                              "Edible oil", "Sugar", "All vehicles"))) %>% 
  # mutate(name = fct_relevel(name, order = c("Base case", "Maize flour", "Wheat flour",
  #                           "Edible oil", "Sugar", "All vehicles"))) %>% 
  ggplot(aes(x = value,y= name, fill = name))+
    geom_density_ridges(stat = "binline",position = 'dodge',alpha = 0.5)+
    geom_vline(xintercept = 490, color = 'blue',size = 1.1,linetype="dashed")+
    geom_vline(xintercept = 3000, color = 'red',size = 1.1) +
    xlab("Vitamin A RAE (mcg)") + 
    ylab("")


nga_lss1819 %>% 
  summarise(sum(wheatflour)/n())
# -----------------------------------------------------------------------------
nga_adm0 <- st_read("/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/MAPS Partnership/Workshop March 2024/analysis_materials/shapefiles/nigeria_0/nigeria_0.shp")
nga_adm1 <- st_read("/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/MAPS Partnership/Workshop March 2024/analysis_materials/shapefiles/nigeria_1/nigeria_1.shp")
nga_adm2 <- st_read("/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/MAPS Partnership/Workshop March 2024/analysis_materials/shapefiles/nigeria_2/nigeria_2.shp")

nga_adm0 <- nga_adm0 %>% 
  st_as_sf()

nga_adm1 <- nga_adm1 %>% 
  st_as_sf()

nga_lss1819_fort_prev <- nga_lss_1819_fort %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
             vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
           ~ifelse(.x<490, 0,1),
           .names = "{.col}_yn"
         )) %>% 
  left_join(nga_lss1819 %>% 
              select(hhid, survey_wgt,lga), 
            by = 'hhid') 

# %>% 
#   as_survey_design(id = hhid, weights = survey_wgt) %>% 
#   summarise(
#     across(
#       c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_yn,vita_rae_mcg_fort_mf_yn,
#         vita_rae_mcg_fort_wf_yn,vita_rae_mcg_fort_ed_yn,vita_rae_mcg_fort_su_yn),
#       ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
#     )
#   )


nga_shape <- nga_lss1819_fort_prev %>% 
  as_survey_design(id = hhid, weights = survey_wgt) %>% 
  group_by(lga) %>% 
  summarise(
    across(
      c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_yn,vita_rae_mcg_fort_mf_yn,
        vita_rae_mcg_fort_wf_yn,vita_rae_mcg_fort_ed_yn,vita_rae_mcg_fort_su_yn),
      ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
    )
  ) %>% 
  left_join(nga_adm2, by= "lga") %>% 
  st_as_sf()

# base

tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "Base case", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#all vehicles
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_tot_fort_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "All fortification vehicles", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


#wheat flour
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_fort_wf_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "Wheat flour", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#maize flour
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_fort_mf_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "Maize flour", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#edible oil
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_fort_ed_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "Edible oil", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


#sugar
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape) + 
  tm_fill(col = "vita_rae_mcg_fort_su_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of \n inadequate micronutrient \n intake")+
  tm_layout(main.title =  "Sugar", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T) 


###### USING UCDAVIS model -----------------------------------------------------

path_to_data <- "~/Documents/MIMI/MIMI_data/"
nga_lss1_red_palm <- haven::read_dta(paste0(path_to_data,"nga/NGA_LSS1819_estimates.dta"))

# base case

nga_lss1_red_palm %>% 
  mutate(vita_rae_mcg = VA_hh_day/hhafe) %>% 
  ggplot(aes(x = vita_rae_mcg))+
  geom_histogram() +
  geom_vline(xintercept = 490, color = 'blue')+
  geom_vline(xintercept = 3000, color = 'red') +
  xlab("Vitamin A RAE (mcg)")+
  labs(title = "WRA (18-29) No Fortification",
  )

nga_lss1_red_palm <- nga_lss1_red_palm %>% 
  mutate(vita_rae_mcg = VA_hh_day/hhafe,
         wheatflour_100g = wheat_flour_grams_day/(hhafe*100),
         maizeflour_100g = maize_flour_grams_day/(hhafe*100),
         edible_oil_100g =  oil_grams_day/(hhafe*100),
         sugar_100g = sugar_grams_day/(hhafe*100)) %>% 
  select(hhid, vita_rae_mcg, wheatflour_100g,maizeflour_100g,edible_oil_100g,sugar_100g)



nga_lss1819_wheat_fort_rp_min <- nga_lss1_red_palm %>% 
  select(hhid, vita_rae_mcg, wheatflour_100g) %>% 
  mutate(wheatflour_100g = ifelse(is.na(wheatflour_100g),0,wheatflour_100g),
         va_wheatflour_min = (wheatflour_100g/10)*wheatflour_fort_min,
         va_wheatflour_max = (wheatflour_100g/10)*wheatflour_fort_max,
         vita_rae_mcg_fort_wheat = vita_rae_mcg+va_wheatflour_min)




nga_lss1819_maize_fort_rp_min <- nga_lss1_red_palm %>% 
  select(hhid, vita_rae_mcg, maizeflour_100g) %>% 
  mutate(maizeflour_100g = ifelse(is.na(maizeflour_100g),0,maizeflour_100g),
         va_mazieflour_min = (maizeflour_100g/10)*maizeflour_fort_min,
         va_mazieflour_max = (maizeflour_100g/10)*maizeflour_fort_max,
         vita_rae_mcg_fort_maize = vita_rae_mcg+va_mazieflour_min)



# oil

nga_lss1819_oil_fort_rp_min <- nga_lss1_red_palm %>% 
  select(hhid, vita_rae_mcg, edible_oil_100g) %>% 
  mutate(edible_oil_100g = ifelse(is.na(edible_oil_100g),0,edible_oil_100g),
         va_edibleoil_min = (edible_oil_100g/10)*edible_oil_fort_min,
         va_edibleoil_max = (edible_oil_100g/10)*edible_oil_fort_max,
         vita_rae_mcg_fort_oil = vita_rae_mcg+va_edibleoil_min)



#sugar
nga_lss1819_sugar_fort_rp_min <- nga_lss1_red_palm %>% 
  select(hhid, vita_rae_mcg, sugar_100g) %>% 
  mutate(sugar_100g = ifelse(is.na(sugar_100g),0,sugar_100g),
         va_sugar_min = (sugar_100g/10)*sugar_fort_min,
         va_sugar_max = (sugar_100g/10)*sugar_fort_max,
         vita_rae_mcg_fort_sugar = vita_rae_mcg+va_sugar_min)






# join into one dataframe 
nga_lss_1819_fort_rp <- nga_lss1_red_palm %>% 
  select(hhid, vita_rae_mcg) %>% 
  left_join(nga_lss1819_wheat_fort_rp_min %>% 
              select(hhid,va_wheatflour_min,va_wheatflour_max),by = "hhid") %>% 
  left_join(nga_lss1819_maize_fort_rp_min %>% 
              select(hhid,va_mazieflour_min,va_mazieflour_max),by = "hhid") %>% 
  left_join(nga_lss1819_oil_fort_rp_min %>% 
              select(hhid,va_edibleoil_min,va_edibleoil_max),by = "hhid") %>% 
  left_join(nga_lss1819_sugar_fort_rp_min %>% 
              select(hhid,va_sugar_min,va_sugar_max),by = "hhid") %>% 
  mutate(vita_rae_mcg_tot_fort_min = vita_rae_mcg+va_wheatflour_min+va_mazieflour_min+va_edibleoil_min+va_sugar_min,
         vita_rae_mcg_tot_fort_max = vita_rae_mcg+va_wheatflour_max+va_mazieflour_max+va_edibleoil_max+va_sugar_max,
         vita_rae_mcg_fort_wf = vita_rae_mcg+va_wheatflour_max,
         vita_rae_mcg_fort_mf = vita_rae_mcg+va_mazieflour_max,
         vita_rae_mcg_fort_ed = vita_rae_mcg+va_edibleoil_max,
         vita_rae_mcg_fort_su = vita_rae_mcg+va_sugar_max)






nga_lss_1819_fort_rp %>% 
  summarise()


### Create density plots

nga_lss_1819_fort_rp %>% 
  select(hhid,vita_rae_mcg,vita_rae_mcg_tot_fort_max,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
         vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su) %>% 
  # tidyr::pivot_longer(cols = c(vita_rae_mcg,vita_rae_mcg_tot_fort_min,vita_rae_mcg_tot_fort_max)) %>% 
  tidyr::pivot_longer(cols = c(vita_rae_mcg,vita_rae_mcg_tot_fort_max,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
                               vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su)) %>%
  mutate(name = factor(case_when(name == "vita_rae_mcg" ~ "Base case",
                                 name == "vita_rae_mcg_tot_fort_max" ~ "All fortificants",
                                  name == "vita_rae_mcg_fort_mf" ~ "Maize flour",
                                                                name == "vita_rae_mcg_fort_wf" ~ "Wheat flour",
                                                                name == "vita_rae_mcg_fort_ed" ~ "Edible oil",
                                                                name == "vita_rae_mcg_fort_su" ~ "Sugar"),
                                  levels = c("Base case", "Wheat flour","Maize flour","Edible oil", "Sugar","All fortificants"))
  ) %>% 
                                 #                                 "Edible oil", "Sugar", "All vehicles"))) 
  # mutate(name = factor(case_when(name == "vita_rae_mcg" ~ "Base case",
  #                                name == "vita_rae_mcg_tot_fort" ~ "All vehicles",
  #                                name == "vita_rae_mcg_fort_mf" ~ "Maize flour",
  #                                name == "vita_rae_mcg_fort_wf" ~ "Wheat flour",
  #                                name == "vita_rae_mcg_fort_ed" ~ "Edible oil",
  #                                name == "vita_rae_mcg_fort_su" ~ "Sugar"),
  #                      levels = c("Base case", "Maize flour", "Wheat flour",
  #                                 "Edible oil", "Sugar", "All vehicles"))) %>% 
  # mutate(name = fct_relevel(name, order = c("Base case", "Maize flour", "Wheat flour",
  #                           "Edible oil", "Sugar", "All vehicles"))) %>% 
  ggplot(aes(x = value,y= name, fill = name))+
  geom_density_ridges(position = 'dodge',alpha = 0.5)+
  geom_vline(xintercept = 490, color = 'blue',size = 1.1,linetype="dashed")+
  geom_vline(xintercept = 3000, color = 'red',size = 1.1) +
  xlab("Vitamin A RAE (mcg)") + 
  ylab("")+
  ggtitle("Maximum level (target + 100%)")



# prevelance of adequate intake
nga_lss1819_fort_prev_rp <- nga_lss_1819_fort_rp %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort_min,vita_rae_mcg_tot_fort_max),
           ~ifelse(.x<490, 0,1),
           .names = "{.col}_yn"
         )) %>% 
  inner_join(nga_lss1819 %>% 
              select(hhid, survey_wgt,lga), 
            by = 'hhid') 

# %>% 
#   as_survey_design(id = hhid, weights = survey_wgt) %>% 
#   summarise(
#     across(
#       c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_yn,vita_rae_mcg_fort_mf_yn,
#         vita_rae_mcg_fort_wf_yn,vita_rae_mcg_fort_ed_yn,vita_rae_mcg_fort_su_yn),
#       ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
#     )
#   )


nga_shape_rp <- nga_lss1819_fort_prev_rp %>% 
  as_survey_design(id = hhid, weights = survey_wgt) %>% 
  group_by(lga) %>% 
  summarise(
    across(
      c(vita_rae_mcg,vita_rae_mcg_tot_fort_max,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
        vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
      ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
    )
  ) %>% 
  left_join(nga_adm2, by= "lga") %>% 
  st_as_sf()

# number above upper limit

nga_lss_1819_fort_rp %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort_max,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
             vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
           ~ifelse(.x<3000, 0,1)
           
         )) %>% 
  summarise(
    across(
      c(vita_rae_mcg,vita_rae_mcg_tot_fort_max,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
      vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
      ~sum(.x,na.rm = T)
  )
  )
  



################################################################################
################################################################################
################################################################################
# MAPS 


# base

tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitamin A intake",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "No fortification", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            # legend.title.size = 1,
            
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#all vehicles
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_tot_fort_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitmain A intake",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "All fortification vehicles", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#wheat flour
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_fort_wf_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitamin A take",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "Wheat flour", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#maize flour
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_fort_mf_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitamin A intake",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "Maize flour", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

#edible oil
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_fort_ed_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitamin A intake",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "Edible oil", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


#sugar
tm_shape(nga_adm0) +
  tm_fill(palette = "gray40")+
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_shape_rp) + 
  tm_fill(col = "vita_rae_mcg_fort_su_yn", style = "cont", breaks = seq(0.2,1,by=.20),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevelance of adequate vitamin A intake",
          legend.is.portrait = FALSE)+
  tm_layout(main.title =  "Sugar", 
            frame = F,
            main.title.size = 0.8,
            legend.outside = TRUE,
            legend.outside.size = 0.1,
            legend.outside.position = 'bottom') +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(nga_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

# -------------------------------------------------------------------------------

#disaggregation by sep 

nga_fort_rp_sep <- nga_lss_1819_fort_rp %>% 
  select(hhid,vita_rae_mcg,vita_rae_mcg_tot_fort_min,vita_rae_mcg_tot_fort_max) %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort_min,vita_rae_mcg_tot_fort_max),
           ~ifelse(.x<490, 0,1),
           .names = "{.col}_yn"
         )) %>% 
  inner_join(nga_lss1819 %>% 
               select(hhid, survey_wgt,lga, res,sep_quintile,res_quintile), 
             by = 'hhid') 

sum(is.na(nga_fort_rp_sep$res_quintile))

nga_fort_rp_sep_sw <- nga_fort_rp_sep %>% 
  as_survey_design(id= hhid, weights = survey_wgt)

nga_fort_rp_sep_sw %>% 
  srvyr::group_by(res) %>% 
  summarise(
    sum(vita_rae_mcg_yn))
  


nga_fort_rp_sep_sw_prev <- nga_fort_rp_sep_sw %>% 
  srvyr::group_by(res,res_quintile) %>%
  srvyr::summarise(
    across(
      c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_min_yn,vita_rae_mcg_tot_fort_max_yn),
      ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
    )) %>% 
  filter(!is.na(res_quintile)) %>% 
  mutate(res_quintile = paste(res,res_quintile))

nga_fort_rp_sep_sw_prev_2 <- nga_fort_rp_sep_sw %>% 
  group_by(res) %>% 
  srvyr::summarise(
    across(
      c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_min_yn,vita_rae_mcg_tot_fort_max_yn),
      ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
    )) %>% 
  mutate(res_quintile = paste(res, "Total"))
  
nga_fort_rp_sep_sw_prev <- rbind(nga_fort_rp_sep_sw_prev,nga_fort_rp_sep_sw_prev_2)


nga_fort <- nga_fort_rp_sep_sw_prev %>% 
  pivot_longer(cols =  c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_min_yn,vita_rae_mcg_tot_fort_max_yn)) %>% 
  select(res_quintile,name,value) %>% 
  mutate(subpop_lab = case_when(res_quintile=="Rural Total" ~ "Rural 6 (Total)",
                                res_quintile=="Rural 1" ~ "Rural 1 (Poorest)",
                                res_quintile=="Rural 2" ~ "Rural 2 (Poor)",
                                res_quintile=="Rural 3" ~ "Rural 3 (Neither poor nor wealthy)",
                                res_quintile=="Rural 4" ~ "Rural 4 (Wealthy)",
                                res_quintile=="Rural 5" ~ "Rural 5 (Wealthiest)",
                                res_quintile=="Urban Total" ~ "Urban 6 (Total)",
                                res_quintile=="Urban 1" ~ "Urban 1 (Poorest)",
                                res_quintile=="Urban 2" ~ "Urban 2 (Poor)",
                                res_quintile=="Urban 3" ~ "Urban 3 (Neither poor nor wealthy)",
                                res_quintile=="Urban 4" ~ "Urban 4 (Wealthy)",
                                res_quintile=="Urban 5" ~ "Urban 5 (Wealthiest)")) %>% 
  ungroup() %>% 
  mutate(scenario = case_when(name == "vita_rae_mcg_yn" ~ "No Fortification",
                              name == "vita_rae_mcg_tot_fort_min_yn" ~ "Standard",
                              name == "vita_rae_mcg_tot_fort_max_yn" ~ "Standard + 40%"
                              ))


nga_fort_range <- nga_fort %>% 
  group_by(res_quintile) %>% 
  summarise(nga_low = min(value),
            nga_high = max(value))

ngafort_dumbell <- nga_fort %>% 
  dplyr::left_join( nga_fort_range, by ='res_quintile')


ggplot(data = ngafort_dumbell) + 
  geom_pointrange(aes(x = subpop_lab, y = value, 
                      ymin = nga_low, ymax = nga_high, 
                      colour = scenario, shape = scenario
                      #, size = population
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  coord_flip(ylim = c(0.2, 1)) + 
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of vitamin A adequacy",
       x = "",
      
  )






# look at different child group
# time series vitamin A

##### isolate hh with children under 5 -----------------------------------------

# split the data into hh with children and no childrem
nga_nutdens_children <- nga_lss1819 %>% 
  filter(under_5>0) %>% 
  select(hhid) %>% 
  left_join(nga_lss_1819_fort_rp, by = "hhid") %>% 
  left_join(nga_lss1_estimates %>% select(hhid,hhafe,energy_hh_day) %>% 
              mutate(energy_kcal = energy_hh_day/hhafe),by = 'hhid') %>% 
  mutate(nut_density_va = vita_rae_mcg*1000/energy_kcal,
         nut_density_va_tot= vita_rae_mcg_tot_fort_min*1000/energy_kcal)

nga_nutdens_no_children <- nga_lss1819 %>% 
  filter(under_5>=0) %>% 
  select(hhid,energy_kcal) %>% 
  left_join(nga_lss_1819_fort, by = "hhid") %>% 
  mutate(nut_density_va = vita_rae_mcg*1000/energy_kcal)


# look at the distribution of nut density
nga_nutdens_children %>% 
  ggplot(aes(x = nut_density_va))+
  geom_histogram() + 
  xlim(0,2500)

#### child between 2-3 energy requirement of 1047

assumed_nutrients <- nga_nutdens_children %>% 
  mutate(assumed_vita = 1.047*nut_density_va,
         assumed_vita_fort_tot = 1.047*nut_density_va_tot) 

assumed_nutrients %>% 
  ggplot(aes(x = assumed_vita))+ 
  geom_histogram(bins = 300)+
  geom_vline(xintercept = 205, color = "blue",size = 1.1,linetype="dashed" )+
  geom_vline(xintercept = 800, color = "red", size = 1.1) +
  xlab("Vitamin A intake RAE (mcg)")+
  ylab("count")+
  labs(title = "Estimated intake of a child aged 2-3",
       subtitle = "No fortification")+

  # annotate("text", x=1300, y=50, label= "3% of HH above UL") +
  theme_ipsum()


assumed_nutrients  %>% 
  ggplot(aes(x = assumed_vita_fort_tot))+ 
  geom_histogram(bins = 300)+
  geom_vline(xintercept = 205, color = "blue",size = 1.1,linetype="dashed" )+
  geom_vline(xintercept = 800, color = "red", size = 1.1) +
  xlab("Vitamin A intake RAE (mcg)")+
  ylab("count")+
  labs(title = "Estimated intake of a child aged 2-3",
       subtitle = "Fortification at standard")+
  
  annotate("text", x=1300, y=50, label= "6% of HH above UL") +
  theme_ipsum()
  
assumed_nutrients %>% 
  mutate(above_ul_fort = ifelse(assumed_vita_fort_tot>=800,1,0),
         above_ul_base = ifelse(assumed_vita>=800,1,0)) %>% 
  left_join(nga_lss1819 %>% select(hhid,survey_wgt),by = "hhid") %>% 
  as_survey_design(id = hhid, weights = survey_wgt) %>% 
  srvyr::summarise(prop_above = survey_mean(above_ul_base == 1, proportion = TRUE,na.rm = T),
                   prop_above_fort = survey_mean(above_ul_fort == 1, proportion = TRUE))





################################################################################
# Time series consumption of vitammin A

nga_lss1819 %>% 
  filter(!is.na(month)) %>% 
  ggplot(aes(x = factor(month), y = vita_rae_mcg))+
  geom_boxplot(outlier.shape = NA)+
  ylim(0,2000)+
  xlab("Month") + 
  ylab("Vitamin A intake RAE (mcg)")+
  theme_ipsum()



vita_highmonth <- nga_lss1819 %>% 
  filter((month %in% c(3,4,5,6))) %>% 
  select(hhid) %>% 
  left_join(nga_lss_1819_fort_rp, by = 'hhid')




vita_highmonth %>% 
  
  tidyr::pivot_longer(cols = c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
                               vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su)) %>% 
  mutate(name = factor(case_when(name == "vita_rae_mcg" ~ "Base case",
                                 name == "vita_rae_mcg_tot_fort" ~ "All vehicles",
                                 name == "vita_rae_mcg_fort_mf" ~ "Maize flour",
                                 name == "vita_rae_mcg_fort_wf" ~ "Wheat flour",
                                 name == "vita_rae_mcg_fort_ed" ~ "Edible oil",
                                 name == "vita_rae_mcg_fort_su" ~ "Sugar"),
                       levels = c("Base case", "Maize flour", "Wheat flour",
                                  "Edible oil", "Sugar", "All vehicles"))) %>% 
  # mutate(name = fct_relevel(name, order = c("Base case", "Maize flour", "Wheat flour",
  #                           "Edible oil", "Sugar", "All vehicles"))) %>% 
  ggplot(aes(x = value,y= name, fill = name))+
  geom_density_ridges(position = 'dodge',alpha = 0.5)+
  geom_vline(xintercept = 490, color = 'blue',size = 1.1,linetype="dashed")+
  geom_vline(xintercept = 3000, color = 'red',size = 1.1) +
  xlab("Vitamin A RAE (mcg)") + 
  ylab("")+
  ggtitle("Maximum level (target + 40%)")



# prevelance of adequate intake
vita_highmonth <- vita_highmonth %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
             vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
           ~ifelse(.x<490, 0,1),
           .names = "{.col}_yn"
         )) %>% 
  inner_join(nga_lss1819 %>% 
               select(hhid, survey_wgt,lga), 
             by = 'hhid') 

# %>% 
#   as_survey_design(id = hhid, weights = survey_wgt) %>% 
#   summarise(
#     across(
#       c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_yn,vita_rae_mcg_fort_mf_yn,
#         vita_rae_mcg_fort_wf_yn,vita_rae_mcg_fort_ed_yn,vita_rae_mcg_fort_su_yn),
#       ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
#     )
#   )


high_month_prev <- vita_highmonth %>% 
  as_survey_design(id = hhid, weights = survey_wgt) %>% 
  group_by(lga) %>% 
  summarise(
    across(
      c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_yn,vita_rae_mcg_fort_mf_yn,
        vita_rae_mcg_fort_wf_yn,vita_rae_mcg_fort_ed_yn,vita_rae_mcg_fort_su_yn),
      ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
    )
  ) %>% 
  left_join(nga_adm2, by= "lga") %>% 
  st_as_sf()

# number above upper limit

vita_highmonth %>% 
  mutate(across
         (
           c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
             vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
           ~ifelse(.x<3000, 0,1)
           
         )) %>% 
  summarise(
    across(
      c(vita_rae_mcg,vita_rae_mcg_tot_fort,vita_rae_mcg_fort_mf,vita_rae_mcg_fort_wf,
        vita_rae_mcg_fort_ed,vita_rae_mcg_fort_su),
      ~sum(.x,na.rm = T)
    )
  )
# tm_shape(nga_adm0)+
#   tm_fill(col = "white")+
# tm_shape(nga_adm1 %>% 
#            filter(state == "kebbi"))+ 
#   tm_fill(col = "green4", alpha =0.4)+
# tm_shape(nga_adm0) +
#   # tm_fill(palette = "white")+
#   tm_borders(col = "black",lwd = 5) 
#   
  
  


