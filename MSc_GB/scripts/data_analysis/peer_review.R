### checking the values in paper : https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10270856/




energy_population <- MICRONUT_SUM(joined,ENERGY_kcal)

protein_pop <- MICRONUT_SUM(joined,PROTEIN_g)

protein_pop %>% filter(AGE_YEAR>=18 & AGE_YEAR<=60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(sum = sum(sum_PROTEIN_g ==0))
  summarise(median = mean(sum_PROTEIN_g)/60)


fat_pop <- MICRONUT_SUM(joined,FAT_g)

fat_pop %>% filter(AGE_YEAR>=18 & AGE_YEAR<60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = mean(sum_FAT_g))

energy_population %>% filter(AGE_YEAR>=18 & AGE_YEAR<=60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = mean(sum_ENERGY_kcal))

calc_pop <- MICRONUT_SUM(joined,CALC_mg)
names(joined)

calc_pop %>% filter(AGE_YEAR>=18 & AGE_YEAR<60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = mean(sum_CALC_mg))




vit_a_population %>% filter(AGE_YEAR>=18 & AGE_YEAR<=60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = median(sum_VITA_RAE_mcg))

iron_population %>% filter(AGE_YEAR>=18 & AGE_YEAR<60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = median(sum_IRON_mg) )


thia_pop <- MICRONUT_SUM(joined,THIA_mg)

thia_pop %>% filter(AGE_YEAR>=18 & AGE_YEAR<60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = median(sum_THIA_mg) )

ribo_pop <- MICRONUT_SUM(joined,RIBO_mg)

(unique(ribo_pop$HOUSEHOLD))

ribo_pop %>% filter(AGE_YEAR>=18 & AGE_YEAR<60) %>% 
  mutate(PREG_LACT = replace_na(PREG_LACT,0)) %>% 
  filter(PREG_LACT<1) %>% 
  filter(SEX == "Female") %>%
  ungroup() %>% 
  summarise(median = median(sum_RIBO_mg) )
