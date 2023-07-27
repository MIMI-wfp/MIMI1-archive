### energy distribution


head(energy_population)

adm2_energy <- energy_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 

  group_by(ADM2_NAME, SEX, ADM1_NAME) %>% 
  summarise(mean = mean(sum_ENERGY_kcal), 
            sd = sd(sum_ENERGY_kcal))
load("datasets/simple_macro_output/en_men.RData")
load("datasets/simple_macro_output/en_women.RData")

energy_population %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(PREG_LACT!= 1 | PREG_LACT!= 2) %>% 
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


energy_men_sp <- inner_join(
  en_men, 
  india_adm2, 
  by = "ADM2_NAME"
)

energy_women_sp <- inner_join(
  en_women, 
  india_adm2, 
  by = "ADM2_NAME"
)

breaks_men <- c(0, 1000, 2000,  2600, 3000, 3500, 4000)

energy_map_men <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(energy_men_sp)) + 
  tm_fill(col = "mean", breaks = breaks_men, palette = "RdBu",
          title = "Usual energy intake \n(kcal)") +
  tm_layout(main.title = "Calorie intake: Men", frame = F,
            main.title.size = 0.8,legend.title.size = 0.75) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = T)


breaks_women <- c(0, 1000, 1500,  2100, 3000, 3500, 4000)

energy_map__women <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(energy_women_sp)) + 
  tm_fill(col = "mean", breaks = breaks_women, palette = "RdBu",
          title = "Usual energy intake \n(kcal)") +
  tm_layout(main.title = "Calorie intake: Women", frame = F,
            main.title.size = 0.8,
            legend.title.size = 0.75
            ) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = T)

# hello this is me testing my keyboard§
energy_corr <- en_men %>% 
  left_join(va_men %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_va = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(fo_men %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_fo = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(ir_men %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_ir = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(zn_men %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_zn = inadequate_percent), by = c("ADM2_NAME", "note")) 




zinc_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_zn, color = "")) +
  scale_color_manual(values = c(my_colours[1])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Zinc",
       x = "", 
       y = "")

va_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_va, color = "")) +
  scale_color_manual(values = c(my_colours[2])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Vitamin A",
       x = "", 
       y = "")

ir_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_ir, color = "")) +
  scale_color_manual(values = c(my_colours[5])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Iron",
       x = "", 
       y = "")


fo_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_fo, color = "")) +
  scale_color_manual(values = c(my_colours[4])) +
  theme_ipsum() +
  theme(legend.position = "none") +
  labs(title = "Folate",
       x = "", 
       y = "")


men_energy <- ggarrange(va_men_corr, fo_men_corr, ir_men_corr,zinc_men_corr,  
                     ncol = 2, nrow = 2)

annotate_figure(men_energy,
                top = text_grob("Micronutrient inadequacy vs energy inadequacy at ADM2 \n Men", color = "#404080", face = "bold", size = 14),
                left = text_grob("Percentage inadequacy of micronutrient", rot = 90),
                bottom = text_grob("Percentage inadequacy of energy")
                
)

energy_corr_women <- en_women %>% 
  left_join(va_women %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_va = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(fo_women %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_fo = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(ir_women %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_ir = inadequate_percent), by = c("ADM2_NAME", "note")) %>% 
  left_join(zn_women %>% select(ADM2_NAME, note, inadequate_percent) %>% 
              rename(inad_zn = inadequate_percent), by = c("ADM2_NAME", "note")) 




zinc_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_zn, color = "")) +
  scale_color_manual(values = c(my_colours[1])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Zinc",
       x = "", 
       y = "")

va_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_va, color = "")) +
  scale_color_manual(values = c(my_colours[2])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Vitamin A",
       x = "", 
       y = "")

ir_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_ir, color = "")) +
  scale_color_manual(values = c(my_colours[5])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  labs(title = "Iron",
       x = "", 
       y = "")


fo_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_fo, color = "")) +
  scale_color_manual(values = c(my_colours[4])) +
  theme_ipsum() +
  theme(legend.position = "none") +
  labs(title = "Folate",
       x = "", 
       y = "")


women_energy <- ggarrange(va_women_corr, fo_women_corr, ir_women_corr,zinc_women_corr,  
                        ncol = 2, nrow = 2)

annotate_figure(women_energy,
                top = text_grob("Micronutrient inadequacy vs energy inadequacy at ADM2 \n Women", color = "#404080", face = "bold", size = 14),
                left = text_grob("Percentage inadequacy of micronutrient", rot = 90),
                bottom = text_grob("Percentage inadequacy of energy")
                
)


# energy correlation with vitamin

cor(en_men$inadequate_percent, va_men$inadequate_percent, method = "spearman")
cor(en_men$inadequate_percent, fo_men$inadequate_percent, method = "spearman")
cor(en_men$inadequate_percent, ir_men$inadequate_percent, method = "spearman")
cor(en_men$inadequate_percent, zn_men$inadequate_percent, method = "spearman")



cor((en_women %>% filter(note!= "WB"))$inadequate_percent, va_women$inadequate_percent, method = "spearman")
cor(en_women$inadequate_percent, fo_women$inadequate_percent, method = "spearman")
cor(en_women$inadequate_percent, ir_women$inadequate_percent, method = "spearman")
cor(en_women$inadequate_percent, zn_women$inadequate_percent, method = "spearman")


#---------------- checking the energy contribution -----------------------------


conversion <- joined %>% 
  group_by(SUBJECT, ADM2_NAME, SEX) %>% 
  filter(FOODEX2_INGR_CODE %in%
           (GDQS %>% filter(g11_whole_grain== 1) %>% 
              select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE) %>% 
  mutate(energy_conv = (ENERGY_kcal/FOOD_AMOUNT_REPORTED)*100*4.184) %>% 
  ungroup() 
            
                        

energy_table <- conversion %>% 
  ungroup() %>% 
  select(INGREDIENT_ENG.y,energy_conv, FOOD_AMOUNT_REPORTED) %>% 
  
  group_by(INGREDIENT_ENG.y) %>% 
  summarise(mean_KJ = mean(energy_conv),
            sd_KJ = sd(energy_conv),
            median_KJ = median(energy_conv))

write_csv(energy_table, "outputs/energy_conversion_table.csv")


energy_conv_plot <- conversion %>%  
  ggplot(aes(energy_conv)) +
  geom_histogram()+
  theme_ipsum()+
  labs(x = "Energy conversion factor (KJ/100g)",
       y = "",
       title = "Energy conversion check "
       ) +
  facet_wrap(vars(INGREDIENT_ENG.y)) 


#### energy calculation of which is rice #############

joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>% 
  select(SUBJECT,ADM1_NAME,ADM2_NAME,SEX, AGE_YEAR, ENERGY_kcal) %>% 
  group_by(SUBJECT,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(RICE_kcal = sum(ENERGY_kcal)) %>% 
  ungroup() %>% 
  select(SUBJECT, RICE_kcal) %>% 
  left_join(energy_population %>% 
              filter(AGE_YEAR>=18),
            by = "SUBJECT"
  ) %>% 
  pivot_longer(cols = c(sum_ENERGY_kcal,RICE_kcal)) %>% 
  mutate(name = ifelse(name == "RICE_kcal", "Rice", "All food")) %>% 
  # filter(PREG_LACT <1, rm.na == F) %>% 
  ggplot(aes(x = value, fill = name))+
  geom_histogram( color="#e9ecef", alpha = 0.7, position = 'identity') +
  geom_vline(xintercept = 2100, color = my_colours[5])+
  geom_vline(xintercept = 2600, color = my_colours[3])+
  annotate("text", x=1250, y=2500, label= "Energy requirement \n for women",size = unit(2.4, "pt")) + 
  annotate("text", x=3500, y=2500, label= "Energy requirement \n for men",size = unit(2.4, "pt"))+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of energy intake \n Adults",
       x = "Energy intake (kcals)", 
       y = "count",
       fill = "Source of energy") + 
  facet_wrap(vars(SEX))
  
 
joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>% 
  select(SUBJECT,ADM1_NAME,ADM2_NAME,SEX, AGE_YEAR, ENERGY_kcal) %>% 
  group_by(SUBJECT,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(RICE_kcal = sum(ENERGY_kcal)) %>% 
  ungroup() %>% 
  select(SUBJECT, RICE_kcal) %>% 
  left_join(energy_population %>% 
              filter(AGE_YEAR>=18),
            by = "SUBJECT"
  ) %>% 
  mutate(percent_rice = RICE_kcal/sum_ENERGY_kcal) %>% 
  ggplot(aes(x = sum_ENERGY_kcal, y = percent_rice, color = SEX ))+ 
  geom_point() +
  # geom_bin2d(bins = 10) +
  # scale_fill_continuous(type = "viridis") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() %>%

  labs(title = "Proportion of rice for calorie intake \n Adults",
       x = "Total Energy intake (kcals)", 
       y = "Percentage of energy from rice",
       color = "Sex")+
  facet_wrap(vars(ADM1_NAME))
  



joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>% 
  select(SUBJECT,ADM1_NAME,ADM2_NAME,SEX, AGE_YEAR, ENERGY_kcal) %>% 
  group_by(SUBJECT,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(RICE_kcal = sum(ENERGY_kcal)) %>% 
  ungroup() %>% 
  select(SUBJECT, RICE_kcal) %>% 
  left_join(energy_population %>% 
              filter(AGE_YEAR>=18),
            by = "SUBJECT"
  ) %>% 
  mutate(percent_rice = RICE_kcal/sum_ENERGY_kcal) %>% 
  group_by(HOUSEHOLD) %>% 
  select()





RICE_men <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
   
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)
                             ) %>% 
  
  left_join(
    joined %>% 
      filter(AGE_YEAR>=18) %>% 
      mutate(rice_ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>%
      group_by(SUBJECT) %>% 
      summarise(
    RICE_sum = sum(rice_ENERGY_kcal),
    ENERGY_sum = sum(ENERGY_kcal)) %>% 
      mutate(percent_rice = RICE_sum/ENERGY_sum),
    by = "SUBJECT"
  ) %>% 
  
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Male") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  dplyr::slice(1)

RICE_women <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)
  ) %>% 
  
  left_join(
    joined %>% 
      filter(AGE_YEAR>=18) %>% 
      mutate(rice_ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>%
      group_by(SUBJECT) %>% 
      summarise(
        RICE_sum = sum(rice_ENERGY_kcal),
        ENERGY_sum = sum(ENERGY_kcal)) %>% 
      mutate(percent_rice = RICE_sum/ENERGY_sum),
    by = "SUBJECT"
  ) %>% 
  
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(SEX == "Female") %>%
  arrange(HOUSEHOLD,desc(AGE_YEAR)) %>% 
  dplyr::slice(1)


RICE_men %>% 
  ungroup() %>% 
  rename(RICE_men_g = sum_RICE_g,
         percent_rice_men = percent_rice) %>% 
  select(HOUSEHOLD, ADM2_NAME, RICE_men_g,percent_rice_men) %>% 
  full_join((RICE_women %>% ungroup() %>% 
               rename(RICE_women_g = sum_RICE_g,
                      percent_rice_women = percent_rice
               ) %>% 
               select(HOUSEHOLD, ADM2_NAME, RICE_women_g,percent_rice_women)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_rice_g = RICE_men_g - RICE_women_g) %>% 
  pivot_longer(cols = c(percent_rice_men,percent_rice_women)) %>% 
  ggplot(aes(x =diff_rice_g, y = value, color = name))+
  geom_point(alpha = .5)



 
  
  