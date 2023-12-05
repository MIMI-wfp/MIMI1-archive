#### recalculate the quintiles from the 2015-16 HICES
#### split by urban rural 

library(tidyr)
library(readr)
library(sf)
library(rmapshaper)
library(readxl)
library(here)
library(ggplot2)
library(hrbrthemes)
library(wesanderson)
library(srvyr)
library(treemap)
library(treemapify)
library(ggridges)
library(gt)
library(haven)
library(foreign)
library(dplyr)

eth_hces1516 <- as.data.frame(read.spss(here::here("../Ethiopia/HICES/ETH-HICE-2016/Data/SPSS 16/HCES_2015_2016_Expenditure.sav")))

# create hhid 
eth_hces1516$hhid <- paste0(as.character(eth_hces1516$CQ11),
                            "_",
                            as.character(eth_hces1516$CQ12),
                            "_",
                            as.character(eth_hces1516$CQ13),
                            "_",
                            as.character(eth_hces1516$CQ14),
                            "_",
                            as.character(eth_hces1516$CQ15),
                            "_",
                            as.character(eth_hces1516$CQ16),
                            "_",
                            as.character(eth_hces1516$CQ17),
                            "_",
                            as.character(eth_hces1516$CQ18))

names(eth_hces1516)
#calculate the total expnediture per hh
x <- eth_hces1516 %>% 
  select(hhid, VALUE, CQ14, QUANTITY,STPRICE,FOOD,FOODEXP,EXPCC, ADEQUIV) %>% 
  group_by(hhid, CQ14,ADEQUIV, EXPCC) %>% 
  summarise(tot_val = sum(VALUE)) %>% 
  mutate(total_per_cap = tot_val/round(ADEQUIV)) %>% 
  ungroup() %>% 
  mutate(quintile =
           case_when(
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
               "exp quant 1",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
               "exp quant 2",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
               "exp quant 3",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
               "exp quant 4",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
               "exp quant 5",
           ))
# mutate(quintile = 
#          case_when(
#            total_per_cap<y[[2]]~
#              "exp quant 1",
#            total_per_cap<y[[3]]~
#              "exp quant 2",
#            total_per_cap<y[[4]]~
#              "exp quant 3",
#            total_per_cap<y[[5]]~
#              "exp quant 4",
#            total_per_cap<y[[6]]~
#              "exp quant 5",
#          ))
# %>% 
  # group_by(EXPCC) %>% 
  # summarise(n())

y <- quantile(x$total_per_cap,probs = c(0,.111,.25,0.45,0.67, 1), na.rm = TRUE, type =9)
c(0,0.2,.4,.6,.8,1)
unique(eth_hces1516$CQ14)

# need to ask about quintile calculation... but if it's ok then we can get this

eth_hces_urbrur_quint <- eth_hces1516 %>% 
  select(hhid, VALUE, UR, QUANTITY,STPRICE,FOOD,FOODEXP,EXPCC, ADEQUIV) %>% 
  group_by(hhid, UR,ADEQUIV, EXPCC) %>% 
  summarise(tot_val = sum(VALUE)) %>% 
  mutate(total_per_cap = tot_val/round(ADEQUIV)) %>% 
  ungroup() %>% 
  group_by(UR) %>% 
  mutate(quintile =
           case_when(
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
               "Quintile 1",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
               "Quintile 2",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
               "Quintile 3",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
               "Quintile 4",
             total_per_cap<quantile(total_per_cap,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
               "Quintile 5",
           )) %>% 
  mutate(urbrur_quintiles = paste(UR, quintile)) %>% 
  select(hhid, UR, quintile, urbrur_quintiles)

## summarise the number in each quintile

eth_hces_urbrur_quint %>%
  ungroup() %>% 
  group_by(UR, quintile) %>% 
  summarise(n())


eth_hces1516 %>% 
  select(hhid, VALUE, CQ14, QUANTITY,STPRICE,FOOD,FOODEXP,EXPCC, ADEQUIV) %>% 
  group_by(hhid, CQ14,ADEQUIV, EXPCC) %>% 
  summarise(tot_val = sum(VALUE)) %>% 
  mutate(total_per_cap = tot_val/round(ADEQUIV)) %>% 
  ungroup() %>% 
  mutate(urbrur = ifelse(CQ14 == "Rural", "Rural", "Urban")) %>% 
  group_by(urbrur,EXPCC) %>% 
  summarise(n())

write_csv(eth_hces_urbrur_quint, here::here("ethiopia/data/urb_rur_quintiles.csv"))
