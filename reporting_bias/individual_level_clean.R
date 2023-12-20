## nnmb cleaning into similar format to HCES 

nnmb_consumption <- read.csv("~/Documents/LSHTM/WFP_project/data/IND_00062/consumption_user.csv")
nnmb_subject <- read.csv("~/Documents/LSHTM/WFP_project/data/IND_00062/subject_user.csv")

# create food list from NNMB consumption
nnmb_food_list <- nnmb_consumption %>% 
  select(FOODEX2_INGR_CODE) %>% 
  distinct(FOODEX2_INGR_CODE)

nnmb_all_items <- nnmb_consumption %>% 
  distinct(SUBJECT) %>% 
  cross_join(nnmb_food_list) %>% 
  left_join(nnmb_consumption %>% 
              select(SUBJECT,
                     FOODEX2_INGR_CODE,
                     
                     FOOD_AMOUNT_REPORTED,
                     ENERGY_kcal,
                     FOLDFE_mcg,
                     VITA_RAE_mcg,
                     VITB12_mcg, 
                     IRON_mg,
                     ZINC_mg), by = c("SUBJECT", "FOODEX2_INGR_CODE")) %>% 
  mutate(
    across(
      -c(SUBJECT, FOODEX2_INGR_CODE),
      ~replace_na(0)
    )
  )
  





nnmb_al<- nnmb_consumption %>% 
  group_by(SUBJECT) %>% 
  mutate(CONSUMPTION_YEAR = as.character(CONSUMPTION_YEAR),
         CONSUMPTION_MONTH = ifelse(str_length(as.character(CONSUMPTION_MONTH)) == 1,
                                    paste0("0",as.character(CONSUMPTION_MONTH)),
                                    as.character(CONSUMPTION_MONTH)),
         CONSUMPTION_DAY = ifelse(str_length(as.character(CONSUMPTION_DAY)) == 1,
                                    paste0("0",as.character(CONSUMPTION_DAY)),
                                    as.character(CONSUMPTION_DAY)
         )
  ) %>% 
  mutate(date = as.Date(paste0(CONSUMPTION_YEAR,"-",CONSUMPTION_MONTH,"-",CONSUMPTION_DAY),"%Y-%m-%d")) %>% 
  ungroup() %>% 
  group_by(SUBJECT,date) %>% 
  summarise(
    across(c(ENERGY_kcal,VITA_RAE_mcg,IRON_mg,ZINC_mg,FOLDFE_mcg,VITB12_mcg),
           ~sum(.x)
    )
  )
  

# ethiopia FSS

fcs11 <- haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/all.dta")
fcs11_adults <-  haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/men.dta")
