# EPHI Training
# Admin 1 mapping 
# Authors: Gabriel Battcock and Kevin Tang

# -------------------------- Packages ------------------------------------------

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt","sp","sf","tmap")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))


# ----------------------- Read in data -----------------------------------------

eth_adm1 <- st_read("/Users/gabrielbattcock/Documents/MIMI/gadm41_ETH_shp/gadm41_ETH_1.shp")
eth_adm1_dict <- read.csv("/Users/gabrielbattcock/Documents/MIMI/eth_adm1_shp.csv")


hices1516 <- apparent_intake("eth_hices1516")
hices_hh_info <- household_data("eth_hices1516")



# ---------------------- Aggregate data ----------------------------------------




x <- hices1516 %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  left_join(hices_hh_info , by = "hhid") %>% 
  mutate(b12_nar = ifelse(vitb12_mcg<=2,vitb12_mcg/2, 1 )) %>% 
  group_by(adm1) %>% 
  summarise(median_b12_nar = median(b12_nar)) %>% 
  left_join(eth_adm1_dict, by = 'adm1') %>% 
  left_join(eth_adm1, by = "GID_1") 


x %>% 
  st_as_sf() %>% 
  tm_shape() + 
  tm_fill(col = "median_b12_nar", style = "cont", breaks = seq(0,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")))+
  tm_layout(main.title =  "", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.5)+
  tm_legend(show = T)
