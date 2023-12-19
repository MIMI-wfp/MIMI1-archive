## nnmb cleaning into similar format to HCES 

nnmb_consumption <- read.csv("~/Documents/LSHTM/WFP_project/data/IND_00062/consumption_user.csv")


nnmb_daily <- nnmb_consumption %>% 
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
  

fcs11 <- haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/all.dta")
fcs11_adults <-  haven::read_dta("/Users/gabrielbattcock/Documents/MIMI/MIMI_data/Ethiopia/eth/fcs12/men.dta")
