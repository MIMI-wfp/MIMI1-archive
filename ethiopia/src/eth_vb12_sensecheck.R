## Ethiopia 

library(sp)
library(sf)
library(tmap)

hices1516 <- apparent_intake("eth_hices1516")
hices_hh_info <- household_data("eth_hices1516")
kev_eth_codes <- read.csv("/Users/gabrielbattcock/Documents/MIMI/ETH_HCES1516_district_codes 1.csv")
eth_shp <- st_read("/Users/gabrielbattcock/Documents/MIMI/gadm41_ETH_shp/gadm41_ETH_2.shp")



x <- hices1516 %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  left_join(hices_hh_info , by = "hhid") %>% 
  mutate(b12_nar = ifelse(vitb12_mcg<=2,vitb12_mcg/2, 1 )) %>% 
  group_by(adm1,adm2) %>% 
  summarise(median_b12_nar = mean(b12_nar)) %>% 
  left_join(kev_eth_codes, by = c("adm1" = "CQ11", "adm2" = "CQ12")) %>% 
  left_join(eth_shp, by = "GID_2") 
  
  
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
