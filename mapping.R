# Gabriel Battcock
# Spatial distribution 


library(sp)
source("data_vis.R")

india_adm1 <- st_read("../geoBoundary-IND-ADM1/geoBoundaries-IND-ADM1.shp")
india_adm1 <- india_adm1 %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = "+init=epsg:3857 +units=m")

unique(india_adm1$shapeName)

vit_a_household <- vit_a_household %>% 
  left_join(user %>% 
              select(HOUSEHOLD, ADM1_NAME) %>% 
              arrange(HOUSEHOLD) %>% 
              group_by(HOUSEHOLD) %>% 
              slice(n = 1), 
            by = "HOUSEHOLD")


inner_join(vit_a_household, india_adm1 %>% rename(ADM1_NAME = shapeName), by = "ADM1_NAME")