## nnmb cleaning into similar format to HCES 

nnmb_consumption <- read.csv("~/Documents/LSHTM/WFP_project/data/IND_00062/consumption_user.csv")
nnmb_subject <- read.csv("~/Documents/LSHTM/WFP_project/data/IND_00062/subject_user.csv")

# create food list from NNMB consumption
nnmb_food_list <- nnmb_consumption %>% 
  select(FOODEX2_INGR_CODE) %>% 
  distinct(FOODEX2_INGR_CODE)

nnmb_all_items <- nnmb_consumption %>% 
  distinct(SUBJECT) %>% 
  cross_join(nnmb_food_list %>%  
               select(FOODEX2_INGR_CODE)) %>% 
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
      ~replace_na(.,0)
    )
  )
  
nnmb_all_items %>% 
  filter(SUBJECT == 1)




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
  
max(nnmb_al$date)

nnmb_women <- nnmb_al %>% 
  left_join(nnmb_subject %>% 
              select(SUBJECT, SEX, AGE_YEAR, PREG_LACT),
            by = 'SUBJECT') %>% 
  filter(SEX == 2 &
           AGE_YEAR <31&
           PREG_LACT == 1)


# ethiopia FSS

fcs11_adults <-haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/all.dta")
fcs11_women <- haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/women.dta")
fcs11_women_rur <- read.csv("~/Documents/MIMI/MIMI_data/Ethiopia/eth_fcs12_simplemacro_v2/eth_fcs12_women_nofort_rural_v2.csv")
fcs11_women_urb <- read.csv("~/Documents/MIMI/MIMI_data/Ethiopia/eth_fcs12_simplemacro_v2/eth_fcs12_women_nofort_urban_v2.csv")

fcs11_men <-  haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/men.dta")
fcs11_child <-  haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/child.dta")

fcs11_women <- 
  fcs11_women %>% 
  mutate(id = paste0(CLUSTER,"_",HHNO,"_",SUBJECT)) %>% 
  left_join(fcs11_women_rur %>% 
              select(id, CLUSTER,HHNO,SUBJECT,FOL_sum,VITB12_sum) %>% 
              bind_rows(fcs11_women_urb %>% 
                          select(id, CLUSTER,HHNO,SUBJECT,FOL_sum,VITB12_sum))) %>% 
  filter(Mother_age<=30)





