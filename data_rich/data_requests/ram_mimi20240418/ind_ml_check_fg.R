library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Documents/MIMI/data_science_code/")

path_to_data = "DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012/"

ind_fgc_dict <- readxl::read_excel("india_inventory.xlsx", sheet = "ind_fgc")
ind_food_cons <- read.csv(paste0(path_to_data, "Consumption of cereals-pulses- milk and milk products  during the last 30 days  - Block 5.1- 5.2- 6 - Level 5 - 68.csv"))




food_groups <- ind_food_cons %>% 
  select(HHID,Item_Code) %>% 
  left_join(ind_fgc_dict, by = "Item_Code")

food_groups %>% 
  pivot_wider(names_from = category_id, values_from = category_id) %>% 
  group_by(HHID) %>% 
  summarise(across(
    -c(Item_Code, item_name),
    ~sum(.x,na.rm = TRUE)
  )) %>% 
  ungroup() %>% 
  mutate(
    across(
    -HHID,
    ~ifelse(.x>0,1, 0)
    )
  )

