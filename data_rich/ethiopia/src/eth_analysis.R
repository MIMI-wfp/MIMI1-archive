## Ethiopia analysis

library(here)
library(tidyr)
library(srvyr)
library(dplyr)
 


#Data load
eth_hces1516_foodbev <- read.csv(here("ethiopia/data/eth_hces1516_foodbev/ETH_HCES1516_foodbev.csv"))
eth_hces1516_foodbev <- eth_hces1516_foodbev %>% filter(TOP1 == "FOOD AND NON - ALCOHOLIC BEVERAGES")
eth_hces1516_foodbev <- eth_hces1516_foodbev %>% #Correct for coding error in food item
  mutate(ITEMC = if_else(ITEMC ==" Wheat & Barley (Duragna), flour", "Wheat & Barley (Duragna), flour", as.character(ITEMC))) %>% 
  mutate(ITEMC = if_else(ITEMC ==" Aresto, Shekla, Zilzil , beef , special", "Aresto, Shekla, Zilzil , beef , special", as.character(ITEMC))) %>% 
  mutate(ITEMC = if_else(ITEMC ==" Coffee Crushed", "Coffee Crushed", as.character(ITEMC))) %>% 
  mutate(ITEMC = if_else(ITEMC ==" Aresto, Shekla, Zilzil sheep / goat  , special", "Aresto, Shekla, Zilzil sheep / goat  , special", as.character(ITEMC))) 
eth_hces1516_foodbev$ITEMC <- as.factor(eth_hces1516_foodbev$ITEMC)


eth_hces1516_afe <- read.csv(here("ethiopia/data/eth_hces1516_foodbev/eth_hces1516_afe.csv"))
eth_hces1516_afe <- eth_hces1516_afe %>% dplyr::select(hhid, afe)


###Food composition 
eth_hces1516_fctmatch <- read_csv(here("ethiopia/data/eth_hces1516_foodbev/ETH_HCES1516_fctmatch.csv"))
eth_hces1516_fctmatch <- eth_hces1516_fctmatch %>% dplyr::select(-TOP1, -TOP2, -TOP3, -TOP4, -TOP5, -n)
eth_hces1516_fctmatch <- eth_hces1516_fctmatch %>% dplyr::select(-contains("nutrient_source"))
eth_hces1516_fctmatch <- eth_hces1516_fctmatch %>% dplyr::select(-contains("original_food"))
eth_hces1516_fctmatch <- eth_hces1516_fctmatch %>% dplyr::select(-contains("fct_name"))
eth_hces1516_fctmatch$energy_in_kcal[is.na(eth_hces1516_fctmatch$energy_in_kcal)] <- 0
eth_hces1516_fctmatch$vitamina_in_rae_in_mcg[is.na(eth_hces1516_fctmatch$vitamina_in_rae_in_mcg)] <- 0
eth_hces1516_fctmatch$vitamind_in_mcg[is.na(eth_hces1516_fctmatch$vitamind_in_mcg)] <- 0
eth_hces1516_fctmatch$folate_in_mcg[is.na(eth_hces1516_fctmatch$folate_in_mcg)] <- 0
eth_hces1516_fctmatch$fe_in_mg[is.na(eth_hces1516_fctmatch$fe_in_mg)] <- 0
eth_hces1516_fctmatch$zn_in_mg[is.na(eth_hces1516_fctmatch$zn_in_mg)] <- 0
eth_hces1516_fctmatch$thiamin_in_mg[is.na(eth_hces1516_fctmatch$thiamin_in_mg)] <- 0
eth_hces1516_fctmatch$riboflavin_in_mg[is.na(eth_hces1516_fctmatch$riboflavin_in_mg)] <- 0
eth_hces1516_fctmatch$niacin_in_mg[is.na(eth_hces1516_fctmatch$niacin_in_mg)] <- 0
eth_hces1516_fctmatch$vitaminb12_in_mcg[is.na(eth_hces1516_fctmatch$vitaminb12_in_mcg)] <- 0
eth_hces1516_fctmatch$vitaminc_in_mg[is.na(eth_hces1516_fctmatch$vitaminc_in_mg)] <- 0
eth_hces1516_fctmatch$ca_in_mg[is.na(eth_hces1516_fctmatch$ca_in_mg)] <- 0
eth_hces1516_fctmatch$vitaminb6_in_mg[is.na(eth_hces1516_fctmatch$vitaminb6_in_mg)] <- 0

### Quintiles

urb_rur_qunitles <- read.csv(here("ethiopia/data/urb_rur_quintiles.csv"))



# join the data together
eth_jces1516_joined <- eth_hces1516_foodbev %>% 
  left_join(eth_hces1516_fctmatch, by = "ITEMC") 
  




# x <- eth_hces1516_afe %>% 
#   left_join(urb_rur_qunitles, by = "hhid") 
# as_tibble(x) %>% 
#   ggplot(aes(x = factor(urbrur_quintiles), y = afe))+
#   geom_boxplot()
