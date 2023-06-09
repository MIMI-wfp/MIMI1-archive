#### mapping 

# Map adequacy of MN's at ADM1 level

breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

# Vitamin A:
vitamina_map <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(va_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu") +
  tm_layout(main.title = "Vitamin A inadequacy: Women", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

vitamina_map

# Folate
folate_map <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(fo_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu") +
  tm_layout(main.title = "Folate inadequacy: Women", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

folate_map

# Zinc: 
zinc_map <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(zn_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu") +
  tm_layout(main.title = "Zinc inadequacy: Women", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)


zinc_map

# Iron: 
iron_map <-  tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(ir_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu") +
  tm_layout(main.title = "Iron inadequacy: Women", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

iron_map

# B12: 
b12_map <-  tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(vb12_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu") +
  tm_layout(main.title = "Vitamin B12 inadequacy: Women", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

b12_map

# legend: 
map_legend <- tm_shape(st_as_sf(vb12_usual_sp)) + 
  tm_fill(col = "inad_women", breaks = breaks, palette = "PuBu",
          title = "Micronutrient Inadequacy",
          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")) +
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1) +
  tm_compass(north = 0, type = "arrow")

map_legend


# Integrate these 5 maps and legend into a single figure: 

maps_list <- list(vitamina_map, folate_map, zinc_map, iron_map, b12_map, map_legend)

mn_ADM1 <- tmap_arrange(maps_list, ncol = 3, sync = TRUE)

mn_ADM1

tmap_save(mn_ADM1, "outputs/mn_ADM1.png", height = 4.5, width = 7)
