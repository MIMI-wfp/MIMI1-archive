## grams per day of mn foods

setwd("~/Documents/LSHTM/WFP_project/MIMI/scripts")
source("data_extraction/data_loading.R")


## Vitamin A -------------------------------------------------------------------

names(joined)
head(joined)
head(GDQS)
names(GDQS)



#dark greens

weight_food_group <- function(food_group){
  joined %>% 
    group_by(SUBJECT, ADM2_NAME, SEX) %>% 
    mutate(amount_g = ifelse(FOODEX2_INGR_CODE %in%
                               (GDQS %>% filter({{food_group} }== 1) %>% 
                                  select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE,
                             FOOD_AMOUNT_REPORTED,0
                             
    )) %>% 
    select(SUBJECT, ADM2_NAME,SEX, amount_g, AGE_YEAR) %>% 
    summarise(sum_g = sum(amount_g)) %>% 
    ungroup()
}



dark_green <-   weight_food_group(g4_dark_leafy_green)

#deep orange fruit
deep_orange_fruit <-  weight_food_group(g2_deep_orange_fruit)

#deep orange veg
deep_orange_veg <-  weight_food_group(g6_deep_orange_veg)

#other veg
other_veg <-  weight_food_group(g7_other_veg ) 

#fish
fish <-  weight_food_group(g13_fish ) 

#poultry
poultry <-  weight_food_group(g14_poultry_game) 

#red meat
red_meat <-  weight_food_group(g18_red_meat ) 

names(GDQS)

#citrus
citrus <- weight_food_group(g1_citrus)

#other fruits
other_fruts <- weight_food_group(g3_other_fruits)

# cruciferous

cruciferous <- weight_food_group(g5_cruiciferous_veg)

#other veg
legumes <- weight_food_group(g8_legumes)

#orange tubers
orange_tubers <- weight_food_group(g9_deep_orange_tubers)

#nuts
nuts_seeds <- weight_food_group(g10_nuts_seeds)

# whole grain

whole_grain <- weight_food_group(g11_whole_grain)

#lf dairy
lf_dairy <- weight_food_group(g15_lf_dairy)

#eggs
eggs <- weight_food_group(g16_eggs)


### plots for food groups and vitmain inadequacy

#men



# vitamin a men 

dark_green %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to leafy greens consumed",
       fill = "Sex")

deep_orange_fruit %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange fruit consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Orange Fruit consumed",
       fill = "Sex")


deep_orange_veg%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange vegetables consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Orange Vegeatables consumed",
       fill = "Sex")

fish %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Fish consumed",
       fill = "Sex")

fish %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Fish consumed",
       fill = "Sex")

poultry %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of poultry consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Poultry consumed",
       fill = "Sex")

red_meat %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of red_meat consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to red_meat consumed",
       fill = "Sex")

legumes%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of legumes consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to Legumes consumed",
       fill = "Sex")

whole_grain %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to whole grain consumed",
       fill = "Sex")


# folate

dark_green %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to leafy greens consumed",
       fill = "Sex")

deep_orange_fruit %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange fruit consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to Orange Fruit consumed",
       fill = "Sex")


deep_orange_veg%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange vegetables consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to Orange Vegeatables consumed",
       fill = "Sex")

fish %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to Fish consumed",
       fill = "Sex")



poultry %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of poultry consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to Poultry consumed",
       fill = "Sex")

red_meat %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of red meat consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to red meat consumed",
       fill = "Sex")

legumes%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of legumes consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to Legumes consumed",
       fill = "Sex")

whole_grain %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Folate compared \n to whole grain consumed",
       fill = "Sex")

# iron 
dark_green %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to leafy greens consumed",
       fill = "Sex")

deep_orange_fruit %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange fruit consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to Orange Fruit consumed",
       fill = "Sex")


deep_orange_veg%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange vegetables consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to Orange Vegeatables consumed",
       fill = "Sex")

fish %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to Fish consumed",
       fill = "Sex")



poultry %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of poultry consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to Poultry consumed",
       fill = "Sex")

red_meat %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of red meat consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to red meat consumed",
       fill = "Sex")

legumes%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of legumes consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to Legumes consumed",
       fill = "Sex")

whole_grain %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to whole grain consumed",
       fill = "Sex")

# Zinc men 


dark_green %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to leafy greens consumed",
       fill = "Sex")

deep_orange_fruit %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange fruit consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to Orange Fruit consumed",
       fill = "Sex")


deep_orange_veg%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of orange vegetables consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to Orange Vegeatables consumed",
       fill = "Sex")

fish %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to Fish consumed",
       fill = "Sex")



poultry %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of poultry consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to Poultry consumed",
       fill = "Sex")

red_meat %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of red meat consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Iron compared \n to red meat consumed",
       fill = "Sex")


legumes%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of legumes consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to Legumes consumed",
       fill = "Sex")

whole_grain %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Zinc compared \n to whole grain consumed",
       fill = "Sex")
