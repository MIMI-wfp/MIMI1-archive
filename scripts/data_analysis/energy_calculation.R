### energy distribution


head(energy_population)

adm2_energy <- energy_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 

  group_by(ADM2_NAME, SEX, ADM1_NAME) %>% 
  summarise(mean = mean(sum_ENERGY_kcal), 
            sd = sd(sum_ENERGY_kcal))

energy_population %>% 
  filter(AGE_YEAR>=18) %>% 
  ggplot(aes(x = sum_ENERGY_kcal, fill = SEX))+
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  geom_vline(xintercept = 2100, color = my_colours[5])+
  geom_vline(xintercept = 2600, color = my_colours[3])+
  annotate("text", x=1250, y=2500, label= "Energy requirement \n for women",size = unit(2.4, "pt")) + 
  annotate("text", x=3500, y=2500, label= "Energy requirement \n for men",size = unit(2.4, "pt"))+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of energy intake \n Adults",
      x = "Energy intake", 
      y = "count",
      fill = "Sex")


adm2_energy %>% 
  # filter(AGE_YEAR>=18) %>% 
  ggplot(aes(x = mean, fill = SEX))+
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  geom_vline(xintercept = 2100, color = my_colours[5])+
  geom_vline(xintercept = 2600, color = my_colours[3])+
  annotate("text", x=1850, y=13, label= "Energy requirement \n for women",size = unit(2.4, "pt")) + 
  annotate("text", x=2400, y=13, label= "Energy requirement \n for men",size = unit(2.4, "pt")) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of energy intake \n Mean ADM2",
       x = "Energy intake", 
       y = "count",
       fill = "Sex")


energy_sp <- inner_join(
  adm2_energy, 
  india_adm2, 
  by = "ADM2_NAME"
)


breaks_men <- c(0, 1000, 2000,  2600, 3000, 3500, 4000)

energy_map__men <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(energy_sp %>% filter(SEX == "Male"))) + 
  tm_fill(col = "mean", breaks = breaks_men, palette = "RdBu",
          title = "Mean energy intake \n(kcal)") +
  tm_layout(main.title = "Calorie intake: Men", frame = F,
            main.title.size = 0.8,legend.title.size = 0.75) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = T)


breaks_women <- c(0, 1000, 1500,  2100, 3000, 3500, 4000)

energy_map__women <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(energy_sp %>% filter(SEX == "Female"))) + 
  tm_fill(col = "mean", breaks = breaks_women, palette = "RdBu",
          title = "Mean energy intake \n(kcal)") +
  tm_layout(main.title = "Calorie intake: Women", frame = F,
            main.title.size = 0.8,
            legend.title.size = 0.75
            ) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = T)

# hello this is me testing my keyboard§

