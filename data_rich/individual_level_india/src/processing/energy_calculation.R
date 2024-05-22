### energy distribution


head(energy_population)

adm2_energy <- energy_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
  group_by(ADM2_NAME, SEX, ADM1_NAME) %>% 
  summarise(mean = mean(sum_ENERGY_kcal), 
            sd = sd(sum_ENERGY_kcal))
load("datasets/simple_macro_output/en_men.RData")
load("datasets/simple_macro_output/en_women.RData")

energy_population %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
  ggplot(aes(x = sum_ENERGY_kcal, fill = SEX))+
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  geom_vline(xintercept = 2130, color = my_colours[5])+
  geom_vline(xintercept = 2710, color = my_colours[3])+
  annotate("text", x=1250, y=2500, label= "Energy requirement \n for women",size = unit(2.4, "pt")) + 
  annotate("text", x=3500, y=2500, label= "Energy requirement \n for men",size = unit(2.4, "pt"))+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of energy intake \n Adults",
      x = "Energy intake (kcal)", 
      y = "count",
      fill = "Sex")

women_energy <- energy_population %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
  filter(SEX == "Female")

with(women_energy, t.test(sum_ENERGY_kcal, mu = 2130, alternative = "less"))

men_energy <- energy_population %>% 
  filter(AGE_YEAR>=18) %>% 
  # mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  # filter(PREG_LACT<=1) %>% 
  filter(SEX == "Male")

with(men_energy, t.test(sum_ENERGY_kcal, mu = 2710, alternative = "less"))

adm2_energy %>% 
  # filter(AGE_YEAR>=18) %>%
  # mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  # filter(PREG_LACT<=1) %>% 
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

breaks_men <- c(0, 1000, 1500,  2710, 3000, 3500, 4000)

energy_map_men <- tm_shape(st_as_sf(india_adm2))+
  tm_fill()+
  tm_shape(st_as_sf(energy_men_sp)) + 
  tm_fill(col = "mean", breaks = breaks_men, palette = "RdBu",
          title = "Usual energy intake \n(kcal)") +
  tm_layout(main.title = "Calorie intake: Men", frame = F,
            main.title.size = 0.8,legend.title.size = 0.75) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = T)


breaks_women <- c(0, 1000, 1500,  2130, 3000)

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

en_map <- tmap_arrange(energy_map_men, energy_map__women)

tmap_save(en_map, "outputs/en_map.png", height = 4.5, width = 7)

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
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Zinc",
       x = "", 
       y = "")

va_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_va, color = "")) +
  scale_color_manual(values = c(my_colours[2])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Vitamin A",
       x = "", 
       y = "")

ir_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_ir, color = "")) +
  scale_color_manual(values = c(my_colours[5])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Iron",
       x = "", 
       y = "")


fo_men_corr <- energy_corr %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_fo, color = "")) +
  scale_color_manual(values = c(my_colours[4])) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlim(0,100)+
  ylim(0,100)+
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
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Zinc",
       x = "", 
       y = "")

va_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_va, color = "")) +
  scale_color_manual(values = c(my_colours[2])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Vitamin A",
       x = "", 
       y = "")

ir_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_ir, color = "")) +
  scale_color_manual(values = c(my_colours[5])) +
  theme_ipsum()  +
  theme(legend.position = "none")+
  xlim(0,100)+
  ylim(0,100)+
  labs(title = "Iron",
       x = "", 
       y = "")


fo_women_corr <- energy_corr_women %>% 
  ggplot(aes(x = inadequate_percent)) +
  geom_point(aes(y = inad_fo, color = "")) +
  scale_color_manual(values = c(my_colours[4])) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlim(0,100)+
  ylim(0,100)+
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

cor_va_men <- cor(en_men$inadequate_percent, va_men$inadequate_percent, method = "spearman")
cor_fo_men <- cor(en_men$inadequate_percent, fo_men$inadequate_percent, method = "spearman")
cor_ir_men <- cor(en_men$inadequate_percent, ir_men$inadequate_percent, method = "spearman")
cor_zn_men <- cor(en_men$inadequate_percent, zn_men$inadequate_percent, method = "spearman")



cor_va_women <- cor((en_women %>% filter(note!= "WB"))$inadequate_percent, va_women$inadequate_percent, method = "spearman")
cor_fo_women <- cor(en_women$inadequate_percent, fo_women$inadequate_percent, method = "spearman")
cor_ir_women <- cor(en_women$inadequate_percent, ir_women$inadequate_percent, method = "spearman")
cor_zn_women <- cor(en_women$inadequate_percent, zn_women$inadequate_percent, method = "spearman")

library(knitr)
library(kableExtra)

correlation_table <- data.frame("Micronutrient" = c("Vitamin A", "Folate", "Iron", "Zinc"),
                                "Men" = round(c(cor_va_men,cor_fo_men, cor_ir_men, cor_zn_men),2),
                                "Women" = round(c(cor_va_women,cor_fo_women,cor_ir_women,cor_zn_women),2))

gt(
  correlation_table
 
)|>
  tab_header(title = "Spearman's rank correlation",
            subtitle = paste0("Prevalance of energy inadequacy against prevalence of micronutrient inadequacy"))



ttest <- data.frame(
  "Micronutrient" = c("Vitamin A", "Folate", "Iron", "Zinc"),
  diff = c("23.69 (7.8, 39.57) mcg", "38.20 (24.32, 52.08) mcg", "1.99 (1.75, 2.23) mg", "1.33 (1.20, 1.44) mg"),
  pval = c(0.0034, "6.9 x10^{-8}", "2.2 x10^{-16}", "2.2 x10-16")
  )

gt(
  ttest
  
)|>
  tab_header(title = "t-test of intake inadequacy prevalence between men and women ") |>
  cols_label(diff = **"Difference in mean value (95% CI)"**,
             pval = **"p-value")
  
the energy contribution -----------------------------


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
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
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
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
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
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
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
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<=1) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)
  ) %>% 
  
  left_join(
    joined %>% 
      filter(AGE_YEAR>=18) %>% 
      mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
      filter(PREG_LACT<1) %>% 
      mutate(rice_ENERGY_kcal = ifelse(grepl("RICE",INGREDIENT_ENG.y),ENERGY_kcal,0 )) %>%
      group_by(SUBJECT) %>% 
      summarise(
        RICE_sum = sum(rice_ENERGY_kcal),
        ENERGY_sum = sum(ENERGY_kcal)) %>% 
      mutate(percent_rice = RICE_sum/ENERGY_sum),#percetnage of energy from rice
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
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
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
  inner_join((RICE_women %>% ungroup() %>% 
               rename(RICE_women_g = sum_RICE_g,
                      percent_rice_women = percent_rice
               ) %>% 
               select(HOUSEHOLD, ADM2_NAME, RICE_women_g,percent_rice_women)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_rice_g = RICE_men_g - RICE_women_g) %>% 
  pivot_longer(cols = c(percent_rice_men,percent_rice_women)) #%>% 
  # ggplot(aes(x =diff_rice_g))+
  # geom_histogram(alpha = .5)





joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<=1) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)
  ) %>% 
  ungroup() %>% 
  group_by(ADM1_NAME, SEX) %>%
  summarise(value = list(sum_RICE_g)) %>% 
  spread(SEX, value) %>% 
  rename( "men" =`1`,
          "women" = `2` ) %>% 
  group_by(ADM1_NAME) %>% 
  mutate(men_mean_intake = mean(unlist(men)),
         women_mean_intake = mean(unlist(women)),
         diff = men_mean_intake - women_mean_intake,
         p_value = t.test( unlist(men),unlist(women),)$p.value,
         t_value = t.test( unlist(men),unlist(women))$statistic
         ) %>% 
  mutate() %>% 
  select(-c(men, women, t_value)) %>% 
  mutate(across(1:3, round, 2)) %>% 
  ungroup()|>
  gt() |>
  tab_header(title = "Rice intake difference by State")|>
  tab_spanner(
    label =  paste0("Rice consumption (g)"),
    columns = c(
      men_mean_intake,
      women_mean_intake,
      diff
    )
    )|>
  cols_label(
    ADM1_NAME = "State",
    men_mean_intake = "Men",
    women_mean_intake = "Women", 
    diff = "Difference (men - women)",
    p_value = "p-value"
  )

  # summarise(mean_intake = mean(sum_RICE_g)) %>% 

  # %>% 
  # mutate(diff_mean = )
  # 
  


rice_diff <- RICE_men %>% 
  ungroup() %>% 
  rename(RICE_men_g = sum_RICE_g,
         percent_rice_men = percent_rice) %>% 
  select(HOUSEHOLD, ADM2_NAME, RICE_men_g,percent_rice_men) %>% 
  inner_join((RICE_women %>% ungroup() %>% 
               rename(RICE_women_g = sum_RICE_g,
                      percent_rice_women = percent_rice
               ) %>% 
               select(HOUSEHOLD, ADM2_NAME, RICE_women_g,percent_rice_women)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_rice_g = RICE_men_g - RICE_women_g) %>% 
  pivot_longer(cols = c(percent_rice_men,percent_rice_women))

with(rice_diff, t.test(RICE_men_g,RICE_women_g))

vita_men_ml

###### looking at BMI and rice/energy consumption

energy_minimum <- data.frame(SEX = c("Male", "Female"),
                             intercept = c(2710,2130),
                             colors = c(my_colours[3], my_colours[5])) #From NIN

bmi_energy_gg <- user %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>%
  mutate(BMI = WEIGHT/(HEIGHT/100)**2) %>% 
  select(SUBJECT, BMI) %>% 
  left_join(energy_population %>% 
              filter(AGE_YEAR>=18) ,
              mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
              filter(PREG_LACT<1),
            by = "SUBJECT"
  ) %>% 
  ggplot(aes(x = BMI, y = sum_ENERGY_kcal))+
  geom_bin2d(bins = 20)+
  geom_vline(xintercept = 18.5, color = 'red')+
 
  # geom_hline(yintercept = 2130, color = my_colours[5])+
  #  geom_hline(yintercept = 2710, color = my_colours[3])+
  # geom_point(aes(alpha = 0.7))+
  theme_ipsum() +
  labs(title = "BMI with energy intake",
       subtitle = "A density plot showing the distribution of all adults. 
   The red line indicates the boundary for being underweight while the blue 
   and brown lines are the MDA for men and women respectively.",
       x = "BMI", 
       y = "Total observered energy intake (kcals)",
       color = "Sex")+
  facet_wrap(vars(SEX)) +
  geom_hline(data = energy_minimum, aes(yintercept = intercept), color = colors)

  
bmi_energy_gg+
  geom_hline(data = energy_minimum,
             aes(yintercept = intercept),
             color = c('#8cdaec','#d48c84')) +
  theme_ipsum()

bmi_men <- bmi_energy %>%
  
  filter(SEX == "Male" ) %>% 
  filter(!is.na(BMI))# %>% 
  # summarise(mean = mean(BMI),
  #           sd = sd(BMI),
  #           p_underweight = sum(BMI<=18.5)/n(),
  #           n_underweight = sum(BMI<=18.5),
  #           )


bmi_women <- bmi_energy %>% filter(SEX == "Female" )%>% 
  filter(!is.na(BMI))

bmi_energy %>% filter(SEX == "Male" ) %>% 
  summarise(
            p_underweight = sum(sum_ENERGY_kcal<=2710)/n(),
            n_underweight = sum(sum_ENERGY_kcal<=2710),
  )

with(bmi_men, t.test(BMI, mu = 18.5, alternative = "less" ))
with(bmi_women, t.test(BMI, mu = 18.5, alternative = "less" ))

bmi_energy %>% filter(SEX == "Female" ) %>% 
  summarise(
    p_underweight = sum(sum_ENERGY_kcal<=2130)/n(),
    n_underweight = sum(sum_ENERGY_kcal<=2130),
  )
                      
 
Nbmi_energy <- user %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>%
  mutate(BMI = WEIGHT/(HEIGHT/100)**2) %>% 
  select(SUBJECT, BMI) %>% 
  left_join(energy_population %>% 
              filter(AGE_YEAR>=18) ,
            mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
              filter(PREG_LACT<1),
            by = "SUBJECT"
  )

      
ggplot(data = bmi_energy, aes(x = BMI, y = sum_ENERGY_kcal))+
  geom_point()+
  facet_wrap(vars(ADM1_NAME))
 
summary(lm(sum_ENERGY_kcal~BMI, data = bmi_energy))
summary(lm(sum_ENERGY_kcal~BMI+factor(ADM1_NAME), data = bmi_energy))
