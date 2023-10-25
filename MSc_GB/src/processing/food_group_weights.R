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

hf_dairy <- weight_food_group(g17_hf_dairy)

deep_orange_tubers <- weight_food_group(g9_deep_orange_tubers)

#eggs
eggs <- weight_food_group(g16_eggs)


names(GDQS)


### plots for food groups and vitmain inadequacy

#men

######broader food groups 


# vitamin A rich foods from dark leafy greens, deep orange fruit, deep orange tubers and deep orange vegetables; 
# animal sourced foods from fish, poultry, red meat, eggs and low-fat dairy; 
# grains from nuts and seeds, grains and pulses; and 
# other fruit and vegetables from other vegetables, citrus fruits, other fruits, and cruciferous vegetables. 



vita_rich <- deep_orange_veg %>% 
  left_join(deep_orange_fruit , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(dark_green , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(deep_orange_tubers, by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g.x.x + sum_g.y.y)


animal_source <- fish %>% 
  left_join(red_meat , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(poultry , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(lf_dairy , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>%
  left_join(eggs , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g.x.x+ sum_g.y.y + sum_g )

grains <- nuts_seeds %>% 
  left_join(whole_grain, by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(legumes, by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g+sum_g.x+sum_g.y)

other_fruit_veg <- citrus %>% 
  left_join(cruciferous, by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(other_fruts,by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(other_veg, by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g.x.x+ sum_g.y.y)


# vitamin a men 

green_va <- dark_green %>% 
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
  ylim(0,100)+
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Vitamin A and leafy greens",
       fill = "Sex")

deep_orange_fruit %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  # filter(SEX == 1) %>%
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

deep_orange_tubers %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(sum_g)) %>% 
  # filter(SEX == 1) %>%
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


orange_veg <- deep_orange_veg%>% 
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
  xlim(0,20)+
  ylim(0,100)+
  labs(x = "Weight of orange vegetables consumed (g)",
       y = "Perentage inadequacy ",
       title = "Vitamin A and orange veg",
       fill = "Sex")

####### vita rich foods
deep_orange_veg %>% 
  left_join(deep_orange_veg , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(dark_green , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  ylim(0,100)+
  labs(x = "Weight of vitamin A rich foods consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to vitamin A rich foods consumed",
       fill = "Sex")

#all meat
fish %>% 
  left_join(red_meat , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(poultry , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(lf_dairy , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(eggs , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g.x.x+ sum_g.y.y + sum_g) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of animal products consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy of Vitamin A compared \n to animal products consumed",
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

wg_va <- whole_grain %>% 
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
  xlim(0,450)+
  ylim(0,100)+
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Vitamin A and whole grains",
       fill = "Sex")


rich_va_va <- vita_rich%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Weight of vitamin A rich foods consumed (g)",
       y = "Perentage inadequacy ",
       title = "Vitamin A and vitamin A rich foods",
       fill = "Sex")

vita_rich %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Folate and vitamin A rich foods",
       fill = "Sex")

vita_rich %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Iron and vitamin A rich foods",
       fill = "Sex")

vita_rich %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Zinc and vitamin A rich foods",
       fill = "Sex")


animal_source%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Vitamin A and animal sourced foods",
       fill = "Sex")

fo_animal <- animal_source %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  xlim(0,300)+
  ylim(0,100)+
  labs(x = "Weight of animal sourced foods consumed (g)",
       y = "Perentage inadequacy ",
       title = "Folate and animal source foods",
       fill = "Sex")

animal_source %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Iron and animal source rich foods",
       fill = "Sex")

animal_source %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Zinc and animal source rich foods",
       fill = "Sex")



other_va <- other_fruit_veg%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(va_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  ylim(0,100)+
  labs(x = "Weight of other fruits and veg consumed (g)",
       y = "Perentage inadequacy ",
       title = "Vitamin A and other veg",
       fill = "Sex")

other_fruit_veg %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  # xlim(0,300)+
  ylim(0,100)+
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Folate and animal source foods",
       fill = "Sex")

other_fruit_veg %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Iron and animal source rich foods",
       fill = "Sex")

other_fruit_veg %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Zinc and animal source rich foods",
       fill = "Sex")


grains%>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
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
       title = "Vitamin A and other veg",
       fill = "Sex")

grains %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  # xlim(0,300)+
  ylim(0,100)+
  labs(x = "Weight of leafy greens consumed (g)",
       y = "Perentage inadequacy ",
       title = "Folate and animal source foods",
       fill = "Sex")

ir_grains <- grains %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(ir_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  xlim(0,520)+
  ylim(0,100)+
  labs(x = "Weight of grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Iron and grains",
       fill = "Sex")

zn_grains <- grains %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(zn_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  xlim(0,520)+
  ylim(0,100)+
  labs(x = "Weight of grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Zinc and grains",
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

fo_fish <- fish %>% 
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
  xlim(0,80)+
  ylim(0,100)+
  
  labs(x = "Weight of fish consumed (g)",
       y = "Perentage inadequacy ",
       title = "Folate and fish",
       fill = "Sex")


fish %>% 
  left_join(red_meat , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(poultry , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  left_join(lf_dairy , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>%
  left_join(eggs , by = c("SUBJECT", "ADM2_NAME", "SEX")) %>% 
  mutate(total_g = sum_g.x + sum_g.y + sum_g.x.x+ sum_g.y.y ) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(mean_g = mean(total_g)) %>% 
  filter(SEX == 1) %>%
  left_join(fo_men, by = "ADM2_NAME") %>% 
  ungroup() %>% 
  ggplot( aes(x = mean_g, y = inadequate_percent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  ylim(0,100)+
  labs(x = "Weight of animal products consumed (g)",
       y = "Perentage inadequacy ",
       title = "Inadequacy Folate compared \n to animal products consumed",
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

wg_folate <- whole_grain %>% 
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
  xlim(0,450)+
  ylim(0,100)+
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

ir_wg <- whole_grain %>% 
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
  xlim(0, 450)+
  ylim(0,100)+
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Iron and whole grains",
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

zn_wg <- whole_grain %>% 
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
  xlim(0,450)+
  ylim(0,100)+
  labs(x = "Weight of whole grains consumed (g)",
       y = "Perentage inadequacy ",
       title = "Zinc and whole grains",
       fill = "Sex")


food_groups <- ggarrange(green_va, orange_veg, fo_fish,ir_wg, zn_wg,  
                     ncol = 2, nrow = 3,
                     labels = c("A","B", "C", "D", "E"))

annotate_figure(food_groups,
                top = text_grob("Food groups with strongest association with \n  micronutrient intake adequacy", color = "#404080", face = "bold", size = 14),
                # bottom = text_grob("The observed intake of the oldest man in a household minus the observed intake of the older woman.", color = "#69b3a2",
                                   # hjust = 1, x = 1, face = "italic", size = 10),
                
)


food_groups_2 <- ggarrange(rich_va_va, fo_animal, ir_grains, zn_grains,  
                         ncol = 2, nrow = 2,
                         labels = c("A","B", "C", "D"))

annotate_figure(food_groups_2,
                top = text_grob("Broad food groups with strongest association with \n  micronutrient intake adequacy", color = "#404080", face = "bold", size = 14),
                # bottom = text_grob("The observed intake of the oldest man in a household minus the observed intake of the older woman.", color = "#69b3a2",
                # hjust = 1, x = 1, face = "italic", size = 10),
                
)


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
library(ggridges)
#animal source foods
animal_source_lab <- c(
  `g13_fish` = "Fish",
  `g14_poultry_game` = "Poultry",
  `g15_lf_dairy` = "Low-fat dairy",
  `g16_eggs` = "Eggs",
  `g18_red_meat` = "Red meat"
)

joined  %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
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
  select(ADM1_NAME,SUBJECT, 
         g15_lf_dairy, 
         g18_red_meat,
         g14_poultry_game,
          
         g16_eggs,
         # g17_hf_dairy,
         g13_fish) %>%
  filter(!is.na(ADM1_NAME)) %>%
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
    facet_wrap(vars(name), labeller = as_labeller(animal_source_lab) )


# vegetables
veg_labels <- c(
`g7_other_veg` = "Other veg", 
`g25_white_roots` = "White roots",
`g6_deep_orange_veg` = "Orange veg",
`g4_dark_leafy_green` = "Leafy greens",
`g5_cruiciferous_veg` = "Cruciferous veg",
# g1_citrus,
# g2_deep_orange_fruit,
`g3_other_fruits` = "Other fruits"
)


joined  %>%  
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
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
  select(ADM1_NAME, SUBJECT, 
         g7_other_veg, 
         g25_white_roots,
         g6_deep_orange_veg,
         g4_dark_leafy_green,
         g5_cruiciferous_veg,
         # g1_citrus,
         # g2_deep_orange_fruit,
         g3_other_fruits) %>%
  filter(!is.na(ADM1_NAME)) %>%
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
  facet_wrap(vars(name), labeller = as_labeller(veg_labels) )

sum(is.na(joined$ADM1_NAME))
# pulses and grains
whole_grain_label <- c(
  `g11_whole_grain` = "Whole grains",
  `g8_legumes` = "Legumes"
)

joined  %>% 
  filter(AGE_YEAR>=18) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>%
  filter(PREG_LACT<=1) %>% 
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
  filter(!is.na(ADM1_NAME)) %>% 
  pivot_longer(cols = -c(ADM1_NAME, SUBJECT)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value, y = ADM1_NAME, fill = ADM1_NAME)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  # geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  # theme(axis.text.x=element_blank())+
  xlim(0,600)+
  labs(x = "Weight of food consumed pp (g)",
       y = "",
       title = "Whole grains and pulses \n by State ",
       fill = "State") + 
  scale_fill_manual(values = my_colours) + 
  facet_wrap(vars(name), labeller = as_labeller(whole_grain_label))


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

