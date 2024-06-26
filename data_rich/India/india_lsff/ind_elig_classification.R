#########################################
#        Identify the PDS hhs           #
#########################################

# Author: Gabriel Battcock
# Created: 8 May 24
# Last updated: 17 June 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# source the base models function
source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))
source(here::here("data_rich/India/india_lsff/ind_mapping.R")) 

# ------------------------------------------------------------------------------       
# read in nsso clean data
path_to_file <- here::here("data_rich/India/data/processed/lsff//")

ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("/ind_nss1112", "_hh_info.csv")))
food_consumption<- read.csv(paste0(path_to_file, paste0("/ind_nss1112", "_food_consumption.csv")))
fc_table <- read.csv(paste0(path_to_file, "/ind_nss1112_fct.csv"))
ind_expenditure <- read.csv(here::here("../data_science_code/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012/Summary of Consumer Expenditure - Block 12 - Level 11 - 68.csv"))

india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))


# ------------------------------------------------------------------------------
# income criteria
# "annual income of below 150,00 Rs a uear can apply" 
############# Srl_no = 49 is monthly per capita expenditure urp

# use inflation rates of cpi 
cpi_2024 = 156.972
cpi_2012 =	80.077
inflation = 	1.0299

# world bank data for poverty line
usd_to_inr <- 74
wb_poverty_line_day <- 2.15
poverty_line_month_inr <- usd_to_inr*wb_poverty_line_day*30

# yearly income of Rs 15,000 supplied by manita
monthly_limit_manita <- 15000/12



state_beneficiaries <- ind_expenditure %>% 
  rename(hhid = HHID) %>% 
  filter(Srl_no %in% c(46,49)) %>% 
  select(hhid, Srl_no, Value) %>% 
  pivot_wider(names_from = Srl_no, values_from = Value) %>%
  rename(pc = `46`,
         Value = `49`) %>% 
  mutate(Value = Value/100) %>% 
  mutate(eligible = ifelse(Value<monthly_limit_manita, 1, 0 )) %>%
  left_join(ind_nsso1112_hh_info, by = "hhid") %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm1,res) %>% 
  srvyr::summarise(
    prop_eligible = srvyr::survey_mean(eligible == 1, proportion = TRUE),
    mean_expend = srvyr::survey_mean(Value),
    min_expend = min(Value),
    max_expend = max(Value)
  )


# punjab: 3, haryana: 6, Rajasthan: 8, UP:9, Bihar: 10, chhatt: 22

ind_eligible <- ind_expenditure %>% 
  rename(hhid = HHID) %>% 
  filter(Srl_no %in% c(46,49)) %>% 
  select(hhid, Srl_no, Value) %>% 
  pivot_wider(names_from = Srl_no, values_from = Value) %>%
  rename(pc = `46`,
         Value = `49`) %>% 
  mutate(Value = Value*inflation/100) %>% 
  mutate(eligible = ifelse(Value<monthly_limit_manita, 1, 0 )) %>% 
  filter(eligible == 1) %>% 
  left_join(ind_nsso1112_hh_info, by = "hhid") %>% 
  filter(adm1 %in% c(3,6,8,9,10,22)) %>% 
  select(hhid)
  
# ----------------------------------------------------------------------------
# using poverty line to define the PDS
# create a dataframe for PL from https://rbi.org.in/scripts/PublicationsView.aspx?Id=21248

poverty_line <- data.frame(
  adm1 = c("Bihar","Chhattisgarh", "Rajasthan", "Haryana",
           "UP", "Punjab", "Bihar","Chhattisgarh", "Rajasthan",
           "Haryana","UP", "Punjab"),
  res = c("Rural","Rural","Rural","Rural","Rural","Rural","Urban",
          "Urban","Urban","Urban","Urban","Urban"),
  pl = c(778, 738, 905, 1015, 768, 1054,923, 849, 1002, 1169, 941, 1155)
)


# create a function to decide eligible households based on the poverty line
get_eligible_households <- function(pl = TRUE){
  
  household_expenditure <- ind_expenditure %>% 
    rename(hhid = HHID) %>% 
    filter(Srl_no %in% c(46,49)) %>% 
    select(hhid, Srl_no, Value) %>% 
    left_join(ind_nsso1112_hh_info, by = "hhid", copy = TRUE) %>% 
    filter(adm1 %in% c(3,6,8,9,10,22)) %>% 
    mutate(Value = Value/100) %>% 
    pivot_wider(names_from = Srl_no, values_from = Value) %>%
    rename(pc = `46`,
           Value = `49`) %>% 
    mutate(
      adm1 = case_when(
        adm1 == 3 ~ "Punjab",
        adm1 == 6 ~ "Haryana",
        adm1 == 8 ~ "Rajasthan",
        adm1 == 9 ~ "UP",
        adm1 == 10 ~ "Bihar",
        adm1 == 22 ~ "Chhattisgarh"
      )
    )
    
  # create 
  
  if(pl == TRUE){
    eligible <- household_expenditure %>% 
      left_join(poverty_line, by = c("adm1","res")) %>% 
      mutate(
        eligible = ifelse(Value<pl, 1, 0)
      ) 
      # filter(eligible == 1)
  }else{
    
    real_beneficiaries <- 
      data.frame(
        adm1 = c(
          "Punjab", "Haryana", "Rajasthan",
          "UP", "Bihar", "Chhattisgarh"
        ),
        prop_pds = c(
          50.1,59.6,53.9,64.1,69,65
        )/100
      )

    for(i in 1:nrow(real_beneficiaries)){

      x <- household_expenditure %>% 
        filter(adm1%in%real_beneficiaries$adm1[i]) %>% 
        mutate(eligible = ifelse(Value < quantile(household_expenditure$Value, probs = real_beneficiaries$prop_pds[i])[[1]],1,0))
      # filter by the bottom x quintile 
      
      
      # print(x)
      if(exists("output_dataframe")==FALSE){
        output_dataframe <- x
      }else{
        output_dataframe <- 
          output_dataframe %>% 
          bind_rows(x)
      }
      # print(output_dataframe)
    }
    eligible <- output_dataframe
    rm(output_dataframe)
  }
  return(eligible)
}

ind_pds_eligible_pl <- get_eligible_households(pl = FALSE)
  
# summary of eligibly households -----------------------------------------------

proportion_summary <- ind_expenditure %>% 
  rename(hhid = HHID) %>% 
  filter(Srl_no %in% c(46,49)) %>% 
  select(hhid, Srl_no, Value) %>% 
  left_join(ind_nsso1112_hh_info, by = "hhid", copy = TRUE) %>% 
  filter(adm1 %in% c(3,6,8,9,10,22)) %>% 
  mutate(Value = Value/100) %>% 
  pivot_wider(names_from = Srl_no, values_from = Value) %>%
  rename(pc = `46`,
         Value = `49`) %>% 
  mutate(
    adm1 = case_when(
      adm1 == 3 ~ "Punjab",
      adm1 == 6 ~ "Haryana",
      adm1 == 8 ~ "Rajasthan",
      adm1 == 9 ~ "UP",
      adm1 == 10 ~ "Bihar",
      adm1 == 22 ~ "Chhattisgarh"
    )
  ) %>% 
  left_join(get_eligible_households() %>% select(hhid,eligible), by = "hhid") %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm1,res) %>% 
  srvyr::summarise(
    prop_eligible = srvyr::survey_mean(eligible == 1, proportion = TRUE),
    mean_expend = srvyr::survey_mean(Value),
    min_expend = min(Value),
    max_expend = max(Value)
  )
  
  
# bar chart 


true_prop_bpl <- data.frame(
  # https://rbi.org.in/scripts/PublicationsView.aspx?Id=21248
  adm1 = c("Bihar","Chhattisgarh", "Rajasthan", "Haryana",
           "UP", "Punjab", "Bihar","Chhattisgarh", "Rajasthan",
           "Haryana","UP", "Punjab"),
  res = c("Rural","Rural","Rural","Rural","Rural","Rural","Urban",
          "Urban","Urban","Urban","Urban","Urban"),
  true_prop = c(0.34, 0.446, 0.16, 0.116, 0.30, 0.076, 0.312, 0.247, 0.106, 0.103, 0.26, 0.092)
)

proportion_summary %>% 
  ggplot(aes(x = adm1, y = prop_eligible*100, fill  = res))+
  geom_col(position = "dodge")+
  geom_col(data = true_prop_bpl, aes(x = adm1, y = true_prop*100), position = 'dodge', alpha = 0.4)+
  theme_bw()+
  labs(
    title = "Proportion Below Poverty Line",
    subtitle = "Proportion of population below 2012 state and residence specific poverty line from the Reserve Bank of India."
  )+
  ylab("Proportion (%)")+
  xlab("State")+
  scale_fill_brewer(palette = "Set1") 
  
  

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


