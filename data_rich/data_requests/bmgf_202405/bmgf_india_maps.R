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

# ------------------------------------------------------------------------------       
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
  tm_layout(main.title = "Iron", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)

cnns_zinc <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "zinc",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Zinc", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)

cnns_vita <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_a",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin A", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)


cnns_folate <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "folate",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Folate", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)


cnns_folate <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "folate",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Folate", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)




cnns_vitb12 <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_b12",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin B12", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)

cnns_vitd <- tm_shape(ind_cnns_sp)+
  tm_fill(col = "vitamin_d",style = "cont", breaks = seq(0,100,by=10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of VMD" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Vitamin D", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
            
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  tm_legend(show = F) +
  tm_credits("Source: The Comprehensive Nation Nutrition Survey (CNNS) 2016-18",
             position = 'left',
             size = 0.6)




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
 