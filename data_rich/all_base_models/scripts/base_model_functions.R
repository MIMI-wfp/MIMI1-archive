## functions to compile base models

## read in the food consumption and fct and create data frame of 
## each food item 

library(ggplot2)

path_to_file <- here::here("all_base_models/data/current/")

allen_ear <- data.frame(
  energy_kcal = 2100,#who
  vita_rae_mcg  = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg = 22.4, #low absorption
  ca_mg = 860, 
  zn_mg = 10.2# unrefined
)



read_in_survey <- function(name_of_survey){
  # given the name of the survey of country
  # the function reads in each part of the base model into general 
  # object names
  
  hh_info <<-  read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_info.csv")))
  food_consumption<<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fc_table <<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
  # fct causes conflict with fct() function in forcats package, reconsider the name of this object
}


full_item_list <- function(name_of_survey){
  # creates a data frame with a full list of food items for every
  # household. If food item is not consumed, quantity = 0
  # uesful for food group analyses
  

  hh_info <-  read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_info.csv")))
  food_consumption<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fc_table <- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
  
  x <- hh_info %>% 
    select(hhid,afe) %>% 
    cross_join(fc_table %>% 
                 select(item_code)) %>% 
    left_join(food_consumption %>% 
                group_by(hhid, item_code, food_group) %>% 
                summarise(across(
                  everything(),
                  ~sum(., na.rm = TRUE)
                )) %>% 
                ungroup(), 
              by = c("hhid", "item_code")) %>% 
    select(-food_group) %>% 
    mutate(
      across(
        c(quantity_100g, quantity_g),
        ~replace_na(.,0)
      )
    ) %>% 
    mutate(
      quantity_100g = quantity_100g/afe, 
      quantity_g = quantity_g/afe
    ) %>% 
    left_join(fc_table, by = "item_code") %>% 
    inner_join(food_consumption %>% 
                 select(item_code, food_group) %>% 
                 distinct(item_code, food_group),
              by = c('item_code'))
  x
}

# food_consumption<- as_tibble(read.csv(paste0(path_to_file, paste0("nga1819", "_food_consumption.csv"))))
# fct <- as_tibble(read.csv(paste0(path_to_file, paste0("nga1819", "_fct.csv"))))


# full_item_list("nga1819")

apparent_intake <- function(name_of_survey){
  # Estimates apparent intake of nutrients based on consumed food items
  # and adult female equivalent unit of the household
  read_in_survey(name_of_survey)
  
  x <- food_consumption %>% 
    left_join(fc_table, by = "item_code") %>% 
    mutate(
      across(
        -c(item_code, hhid,item_name ,food_group, quantity_100g, quantity_g),
        ~.x*quantity_100g
      )
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      across(-c(item_code,item_name,quantity_100g,quantity_g, food_group),
             ~sum(.,na.rm = T))
    ) %>% 
    left_join(hh_info %>% select(hhid, afe), by = "hhid") %>% 
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) %>% 
    ungroup()
  x
}




household_data <- function(name_of_survey){
  #reads in the household information data
  x <- read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_info.csv")))
  x
}


nutrient_density <- function(name_of_survey){
  # returns a data frame of nutrient density for each household
  # values are given in unit of mn per 1000kcal 
  #
  x <- food_consumption %>% 
    left_join(fc_table, by = "item_code") %>% 
    mutate(
      across(
        -c(item_code, hhid, item_name, food_group, quantity_100g, quantity_g),
        ~.x*quantity_100g
      )
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      across(-c(item_code,item_name,quantity_100g,quantity_g, food_group),
             ~sum(.,na.rm = T))
    ) %>% 
    mutate(energy_1000kcal = energy_kcal/1000) %>% 
    mutate(
      across(
        -c(hhid),
        ~.x/energy_1000kcal,
        .names ="{.col}_1000kcal"
      )
    ) %>% 
    select(hhid, ends_with("1000kcal")) %>% 
    select(-energy_kcal_1000kcal)
  x
}


apparent_intake("eth_ess1819") %>% 
  select(hhid)


target_creation <- function(){
  eth_ess1819 <- apparent_intake("eth_ess1819")
  # eth_hices1516 <- apparent_intake("eth_hices1516")
  nga_lss1819 <- apparent_intake("nga_lss1819")
  ind_nss1112 <- apparent_intake("ind_nss1112")
  
  select_and_append <- function(survey, survey_id){
    survey <- survey %>% 
      select(hhid,vita_rae_mcg,folate_mcg,vitb12_mcg,
             fe_mg,zn_mg) %>% 
      rename(
        va_ai = vita_rae_mcg,
        fo_ai = folate_mcg,
        vb12_ai = vitb12_mcg,
        fe_ai = fe_mg,
        zn_ai = zn_mg
      ) %>% 
      mutate(
        hhid = as.character(hhid),
        va_ref = allen_ear$vita_rae_mcg,
        fo_ref = allen_ear$folate_mcg,
        vb12_ref = allen_ear$vitb12_mcg,
        fe_ref = allen_ear$fe_mg,
        zn_ref = allen_ear$zn_mg,
        va_nar = ifelse(va_ai<=va_ref, va_ai/va_ref,1),
        fo_nar = ifelse(fo_ai<=fo_ref, fo_ai/fo_ref,1),
        vb12_nar = ifelse(vb12_ai<=vb12_ref, vb12_ai/vb12_ref,1),
        fe_nar = ifelse(fe_ai<=fe_ref, fe_ai/fe_ref,1),
        zn_nar = ifelse(zn_ai<=zn_ref, zn_ai/zn_ref,1),
        mimi_simple = (va_nar+fo_nar+vb12_nar+fe_nar+zn_nar)/5,
        survey_id = survey_id
      ) %>% 
      select(-c(va_nar,fo_nar,vb12_nar,zn_nar,fe_nar))
    survey
  }
 x <-  select_and_append(eth_ess1819,"ETH_2018_ESS_v03_M") %>% 
    bind_rows(select_and_append(nga_lss1819,"NGA_2018_LSS_v01_M")) %>% 
    bind_rows(select_and_append(ind_nss1112,"DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012"))
 x
}


