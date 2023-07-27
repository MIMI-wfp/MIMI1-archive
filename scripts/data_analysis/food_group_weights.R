## grams per day of mn foods

setwd("~/Documents/LSHTM/WFP_project/MIMI/scripts")
source("data_extraction/data_loading.R")


## Vitamin A -------------------------------------------------------------------

names(joined)
head(joined)
head(GDQS)
names(GDQS)



# function

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


################################################################################
#                                                                              #
#                            states and food groups                            #
#                                                                              # 
#                                                                              #
################################################################################


# joined %>% 
#   group_by(SUBJECT, ADM1_NAME, ADM2_NAME, SEX) %>% 
#   mutate(amount_g = ifelse(FOODEX2_INGR_CODE %in%
#                              (GDQS %>%  %>% 
#                                 select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE,
#                            FOOD_AMOUNT_REPORTED,0
#                            
#   )) %>% 
#   select(SUBJECT, ADM2_NAME,SEX, amount_g, AGE_YEAR) %>% 
#   summarise(sum_g = sum(amount_g)) %>% 
#   ungroup()


## looking at weight of each food groups for adm1

#animal source foods
joined  %>% 
  select(SUBJECT,ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED, FOODEX2_INGR_CODE) %>% 
  full_join(GDQS, by = "FOODEX2_INGR_CODE") %>% 
  mutate(across(.cols = -c("SUBJECT",ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED,FOODEX2_INGR_CODE,INGREDIENT_ENG), 
                .fns = ~ if_else(. == 0, 0, FOOD_AMOUNT_REPORTED),
                .names = "{.col}")) %>% 
  select(-c(FOODEX2_INGR_CODE,INGREDIENT_ENG)) %>% 
  group_by(SUBJECT,ADM1_NAME, ADM2_NAME, SEX) %>% 
  summarize(across(.cols = everything(), .fns = sum)) %>% 
  ungroup() %>% 
  select(-c(SEX,ADM2_NAME, FOOD_AMOUNT_REPORTED)) %>% 
  # group_by(ADM1_NAME) %>% 
  # summarise(across(.cols = everything(), .fns = mean)) %>% 
  mutate(ADM1_NAME = factor(ADM1_NAME)) %>% 
  select(ADM1_NAME,SUBJECT, g15_lf_dairy, g18_red_meat, g14_poultry_game,
          g13_fish, g16_eggs, g17_hf_dairy) %>%
  pivot_longer(cols = -c(ADM1_NAME, SUBJECT)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value, y = ADM1_NAME, fill = ADM1_NAME)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
    # geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    # theme(axis.text.x=element_blank())+
    xlim(0,400)+
    labs(x = "Weight of food consumed pp (g)",
         y = "",
         title = "Animal sourced food group intake \n by State ",
         fill = "State") + 
    scale_fill_manual(values = my_colours) + 
    facet_wrap(vars(name) )


# vegetables
joined  %>% 
  select(SUBJECT,ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED, FOODEX2_INGR_CODE) %>% 
  full_join(GDQS, by = "FOODEX2_INGR_CODE") %>% 
  mutate(across(.cols = -c("SUBJECT",ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED,FOODEX2_INGR_CODE,INGREDIENT_ENG), 
                .fns = ~ if_else(. == 0, 0, FOOD_AMOUNT_REPORTED),
                .names = "{.col}")) %>% 
  select(-c(FOODEX2_INGR_CODE,INGREDIENT_ENG)) %>% 
  group_by(SUBJECT,ADM1_NAME, ADM2_NAME, SEX) %>% 
  summarize(across(.cols = everything(), .fns = sum)) %>% 
  ungroup() %>% 
  select(-c(SEX,ADM2_NAME, FOOD_AMOUNT_REPORTED)) %>% 
  # group_by(ADM1_NAME) %>% 
  # summarise(across(.cols = everything(), .fns = mean)) %>% 
  mutate(ADM1_NAME = factor(ADM1_NAME)) %>% 
  select(ADM1_NAME, SUBJECT, g7_other_veg, g25_white_roots,g6_deep_orange_veg,
         g4_dark_leafy_green, g5_cruiciferous_veg,g1_citrus,g2_deep_orange_fruit,g3_other_fruits) %>%
  pivot_longer(cols = -c(ADM1_NAME, SUBJECT)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value, y = ADM1_NAME, fill = ADM1_NAME)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  # geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  # theme(axis.text.x=element_blank())+
  xlim(0,400)+
  labs(x = "Weight of food consumed pp (g)",
       y = "",
       title = "Fruit and veg intake \n by State ",
       fill = "State") + 
  scale_fill_manual(values = my_colours) + 
  facet_wrap(vars(name) )


# pulses and grains
joined  %>% 
  select(SUBJECT,ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED, FOODEX2_INGR_CODE) %>% 
  full_join(GDQS, by = "FOODEX2_INGR_CODE") %>% 
  mutate(across(.cols = -c("SUBJECT",ADM1_NAME, ADM2_NAME, SEX,FOOD_AMOUNT_REPORTED,FOODEX2_INGR_CODE,INGREDIENT_ENG), 
                .fns = ~ if_else(. == 0, 0, FOOD_AMOUNT_REPORTED),
                .names = "{.col}")) %>% 
  select(-c(FOODEX2_INGR_CODE,INGREDIENT_ENG)) %>% 
  group_by(SUBJECT,ADM1_NAME, ADM2_NAME, SEX) %>% 
  summarize(across(.cols = everything(), .fns = sum)) %>% 
  ungroup() %>% 
  select(-c(SEX,ADM2_NAME, FOOD_AMOUNT_REPORTED)) %>% 
  # group_by(ADM1_NAME) %>% 
  # summarise(across(.cols = everything(), .fns = mean)) %>% 
  mutate(ADM1_NAME = factor(ADM1_NAME)) %>% 
  select(ADM1_NAME, SUBJECT, g8_legumes,g11_whole_grain) %>%
  pivot_longer(cols = -c(ADM1_NAME, SUBJECT)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value, y = ADM1_NAME, fill = ADM1_NAME)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  # geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  # theme(axis.text.x=element_blank())+
  xlim(0,1000)+
  labs(x = "Weight of food consumed pp (g)",
       y = "",
       title = "Whole grains and pulses \n by State ",
       fill = "State") + 
  scale_fill_manual(values = my_colours) + 
  facet_wrap(vars(name) )


################################################################################
#                                                                              #
#                            states and Rice consumption                       #
#                                                                              # 
#                                                                              #
################################################################################

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(FOOD_AMOUNT_REPORTED = ifelse(grepl("RICE",INGREDIENT_ENG.y),FOOD_AMOUNT_REPORTED,0 )) %>% 
  select(SUBJECT,ADM1_NAME,ADM2_NAME,SEX, AGE_YEAR, FOOD_AMOUNT_REPORTED) %>% 
  group_by(SUBJECT,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
  ungroup() %>% 
  # group_by(ADM1_NAME) %>% 
  # summarise(mean_rice_g = mean(sum_RICE_g)) %>% 
  ggplot(aes(x = sum_RICE_g, y = ADM1_NAME, fill = ADM1_NAME)) + 
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
    theme_ipsum() +
    # theme(axis.text.x=element_blank())+
    labs(x = "Rice intake (g)",
         y = "",
         title = "Rice intake distribution \n by State ",
         fill = "State") + 
    xlim(0, 925)+
    scale_fill_manual(values = my_colours) 
  
joined %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(FOOD_AMOUNT_REPORTED = ifelse(grepl("RICE",INGREDIENT_ENG.y),FOOD_AMOUNT_REPORTED,0 )) %>% 
  select(SUBJECT,ADM1_NAME,ADM2_NAME,SEX, AGE_YEAR, FOOD_AMOUNT_REPORTED) %>% 
  group_by(SUBJECT,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
  ungroup() %>% 
  # group_by(ADM1_NAME) %>% 
  # summarise(mean_rice_g = mean(sum_RICE_g)) %>% 
  ggplot(aes(x = sum_RICE_g)) + 
  geom_histogram()+
  theme_ipsum() +
  # theme(axis.text.x=element_blank())+
  labs(x = "Rice intake (g)",
       y = "",
       title = "Rice intake distribution \n overall ",
       fill = "State") + 
  xlim(0, 925)+
  scale_fill_manual(values = my_colours[1]) 


###### check conversion of food groups ##############


vita_conversion <- joined %>% 
  group_by(SUBJECT, ADM2_NAME, SEX) %>% 
  filter(FOODEX2_INGR_CODE %in%
           (GDQS %>% filter(g4_dark_leafy_green== 1) %>% 
              select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE) %>% 
  mutate(vita_conv = (VITA_RAE_mcg/FOOD_AMOUNT_REPORTED)*100) %>% 
  ungroup() 


vita_table <- vita_conversion %>% 
  ungroup() %>% 
  select(INGREDIENT_ENG.y,vita_conv, FOOD_AMOUNT_REPORTED) %>% 
  
  group_by(INGREDIENT_ENG.y) %>% 
  summarise(mean_mcg = mean(vita_conv),
            sd_mcg = sd(vita_conv),
            median_mcg = median(vita_conv))

write_csv(vita_table, "outputs/vita_conversion_table.csv")


iron_conv_plot <- iron_conversion %>%  
  ggplot(aes(iron_conv)) +
  geom_histogram()+
  theme_ipsum()+
  labs(x = "Iron conversion factor (mg/100g)",
       y = "",
       title = "Iron conversion check "
  ) +
  facet_wrap(vars(INGREDIENT_ENG.y)) 



iron_conversion <- joined %>% 
  group_by(SUBJECT, ADM2_NAME, SEX) %>% 
  filter(FOODEX2_INGR_CODE %in%
           (GDQS %>% filter(g4_dark_leafy_green== 1) %>% 
              select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE) %>% 
  mutate(iron_conv = (IRON_mg/FOOD_AMOUNT_REPORTED)*100) %>% 
  ungroup() 


iron_table <- iron_conversion %>% 
  ungroup() %>% 
  select(INGREDIENT_ENG.y,iron_conv, FOOD_AMOUNT_REPORTED) %>% 
  
  group_by(INGREDIENT_ENG.y) %>% 
  summarise(mean_mg = mean(iron_conv),
            sd_mg = sd(iron_conv),
            median_mg = median(iron_conv))

write_csv(iron_table, "outputs/iron_conversion_table.csv")


iron_conv_plot <- iron_conversion %>%  
  ggplot(aes(iron_conv)) +
  geom_histogram()+
  theme_ipsum()+
  labs(x = "Iron conversion factor (mg/100g)",
       y = "",
       title = "Iron conversion check "
  ) +
  facet_wrap(vars(INGREDIENT_ENG.y)) 

