# Gabriel Battcock
# background data visualisation
# Differences between mean intake between sexes 

library(ggplot2)
library(hrbrthemes)
source("data_loading.R")#sources the functions and data

#take in date for the whole population for each MN
vit_a_population <- MICRONUT_SUM(joined, VITA_RAE_mcg)
folate_population <- MICRONUT_SUM(joined, FOLDFE_mcg)
iron_population <- MICRONUT_SUM(joined, IRON_mg)
zinc_population <- MICRONUT_SUM(joined, ZINC_mg)

#histograms per sex
vit_a_population %>% 
  filter(sum_VITA_RAE_mcg<1000) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_VITA_RAE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Disrtibution of mean intake: \n Vitamin A",
       x = "Mean intake (mcg)", 
       fill = "Sex")

folate_population %>% 
  filter(sum_FOLDFE_mcg<750) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_FOLDFE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Disrtibution of mean intake: \n Folate",
       x = "Mean intake (mcg)", 
       fill = "Sex")

iron_population %>% 
  filter(sum_IRON_mg<50) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_IRON_mg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Disrtibution of mean intake: \n Iron",
       x = "Mean intake (mg)", 
       fill = "Sex")

zinc_population %>% 
  filter(sum_ZINC_mg<25) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_ZINC_mg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Disrtibution of mean intake: \n Zinc",
       x = "Mean intake (mg)", 
       fill = "Sex")

#violin plots
#whole population
user %>% 
  # filter(sum_VITA_RAE_mcg<1000) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = SEX, y = AGE_YEAR, fill = SEX)) +
  geom_violin( alpha = 1, position = 'dodge', show.legend = FALSE) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Age distribution of population",
       x = "Sex",
       y = "Age (years)")

# heads of household
user %>% 
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  ungroup() %>% 
  group_by(HOUSEHOLD) %>% 
  ggplot(aes(x = SEX, y = AGE_YEAR, fill = SEX)) +
  geom_violin( alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")




vit_a_population %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  ungroup() %>% 
  group_by(HOUSEHOLD) %>% 
  na.omit() %>% 
  filter(sum_VITA_RAE_mcg<1000) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_VITA_RAE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
  

# Head of household difference in intake (Male - Female )


summary(vit_a_household)

vit_a_household %>% 
  filter(DIFF > -100 & DIFF < 100) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram(  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Difference in mean intake per household:\n Vitamin A",
       x = "Difference (male - female) in mean intake (RAE mcg)")

folate_household %>% 
  filter(DIFF > -100 & DIFF < 100) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram( fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Difference in mean intake per household:\n Folate",
       x = "Difference (male - female) in mean intake (mcg)")

iron_household %>% 
  filter(DIFF > -25 & DIFF < 25) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram(  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Difference in mean intake per household:\n Iron",
       x = "Difference (male - female) in mean intake (mg)")

zinc_household %>% 
  filter(DIFF > -25 & DIFF < 25) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram(  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Difference in mean intake per household:\n Zinc",
       x = "Difference (male - female) in mean intake (mg)")

## two sided T-test

with(vit_a_household, t.test(SUM_MALE, SUM_FEMALE)) #not a significant difference 
with(folate_household, t.test(SUM_MALE, SUM_FEMALE)) 
with(iron_household, t.test(SUM_MALE, SUM_FEMALE))
with(zinc_household, t.test(SUM_MALE, SUM_FEMALE))

#
