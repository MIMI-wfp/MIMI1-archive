### food group understanding of results 
setwd("~/Documents/LSHTM/WFP_project/MIMI")
path_to_script <- "scripts/data_extraction/"
path_to_data <- "../IND_00062/"
source(paste0(path_to_script, "data_loading.R"))

library(betareg)

#look at vitamin A and the food groups associated

all_mn_DQQ <- all_mn %>% ungroup() %>% left_join(DQQ_list, by = "SUBJECT")

vit_a_DQQ_lm <- vit_a_DQQ %>% select(!c(ROUND,HOUSEHOLD, AGE_YEAR,AGE_GROUP,CONSUMPTION_DAY,ADM1_NAME,ADM2_NAME))
lm_vita_DQQ <- lm(sum_VITA_RAE_mcg~.,
                  vit_a_DQQ_lm)

which(lm_vita_DQQ$coefficients>1)



DQQ_g_name <- names(DQQ_list %>% select(!SUBJECT))#take names of food grouops

DQQ_food_percentage <-   all_mn_DQQ %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME) %>% 
  summarise(across(all_of(DQQ_g_name), 
                   ~(sum(.x == 1))/n()
                ))


vita_ADM2_dqq <- va_all %>% 
  mutate(inadequate_percent = inadequate_percent/100) %>% 
  select(inadequate_percent, ADM2_NAME) %>% 
  left_join(DQQ_food_percentage, by = "ADM2_NAME") %>% 
  select(!ADM2_NAME)

beta_va_DQQ_2 <- betareg(inadequate_percent~.,
                         data = vita_ADM2_dqq,
                         
)
summary(beta_fo_DQQ_2)

fo_ADM2_dqq <- fo_all %>% 
  mutate(inadequate_percent = inadequate_percent/100) %>% 
  select(inadequate_percent, ADM2_NAME) %>% 
  left_join(DQQ_food_percentage, by = "ADM2_NAME") %>% 
  select(!ADM2_NAME)

beta_fo_DQQ_2 <- betareg(inadequate_percent~.,
                      data = fo_ADM2_dqq,
                    
)
summary(beta_fo_DQQ_2)

ir_ADM2_dqq <- ir_all %>% 
  mutate(inadequate_percent = inadequate_percent/100) %>% 
  select(inadequate_percent, ADM2_NAME) %>% 
  left_join(DQQ_food_percentage, by = "ADM2_NAME") %>% 
  select(!ADM2_NAME)


beta_ir_DQQ_2 <- betareg(inadequate_percent~.,
                         data = ir_ADM2_dqq
                     
                         
)
summary(beta_ir_DQQ_2)



zn_ADM2_dqq <- zn_all %>% 
  mutate(inadequate_percent = inadequate_percent/100) %>% 
  select(inadequate_percent, ADM2_NAME) %>% 
  left_join(DQQ_food_percentage, by = "ADM2_NAME") %>% 
  select(!ADM2_NAME)

zn
# areas with low zinc inadequacy consume more dairy it appears



#look at vitamin b12 and the food groups associated with the lack of spike
