### quick analysis for bias paper

source(here::here("all_base_models/scripts/base_model_functions.R"))

nga1819 <- apparent_intake("nga1819")
nga1819_all_items <- full_item_list("nga1819")

nsso1112<- apparent_intake("nsso1112")

ess1819 <- apparent_intake("ess1819")

mwi1516 <- apparent_intake("mwi1516")

nnmb <- 







## read in a time stamp --------------------------------------------------------

nga1819_timestamp <- read.csv("~/Documents/MIMI/MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/household/secta_cover.csv")
nga1819_timestamp <- nga1819_timestamp %>%
  mutate(date = format(as.Date(InterviewStart, "%m/%d/%Y %H:%M:%S"),"%Y-%m-%d")) %>% 
  select(hhid,date) 

ess1819_timestamp <- read.csv(paste0(path_to_data, "Ethiopia/ETH_2018_ESS_v03_M_CSV/sect_cover_hh_w4.csv"))
ess1819_timestamp <- ess1819_timestamp %>% 
  mutate(date = format(as.Date(InterviewStart, "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")) %>% 
  rename(hhid = household_id) %>% 
  select(hhid, date)



nsso1112_timestamp <- read.csv("India_analysis/data/raw/block_1_2_identification.csv")
nsso1112_timestamp <- nsso1112_timestamp %>% 
  mutate(date =ifelse(str_length(as.character(Date_of_Survey))==5, paste0("0",as.character(Date_of_Survey)), as.character(Date_of_Survey))) %>% 
  mutate(date = format(as.Date(date,"%d%m%y"),"%Y-%m-%d")) %>% 
  rename(hhid = HHID) %>% 
  select(hhid, date)

mwi1516_timestamp <- read.csv(paste0(path_to_data, "mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_a_filt.csv"))
mwi1516_timestamp <- mwi1516_timestamp %>% 
  mutate(date = as.Date(interviewDate)) %>% 
  rename(hhid = HHID) %>% 
  select(hhid, date)
  
# ------------------------------------------------------------------------------

nga1819_all_items %>% 
  left_join(nga1819_timestamp, by = "hhid") %>% 
  filter(!is.na(energy_kcal)) %>% 
  filter(item_code == 16) %>% 
  group_by(date) %>% 
  summarise(mean = mean(quantity_g)) %>% 
  ggplot(aes(x = as.Date(date), y = mean)) + 
  geom_point() +
  geom_smooth() + 
  theme_ipsum()


nga1819 %>% 
  left_join(nga1819_timestamp, by = "hhid") %>% 
  filter(!is.na(energy_kcal)) %>% 
  # filter(item_code == 74) %>% 
  group_by(date) %>%
  summarise(
    across(-c(hhid,afe),
    ~mean(.))
    )%>%
  ungroup() %>%
  # pivot_longer(cols = -date) %>% 
  ggplot(aes(x = as.Date(date), y = vita_rae_mcg)) + 
  geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(span = 0.8, color = 'blue') 
  # facet_wrap(facets = vars(name))


nsso1112 %>% 
  left_join(nsso1112_timestamp, by = "hhid") %>% 
  filter(!is.na(energy_kcal)) %>% 
  # filter(item_code == 74) %>% 
  group_by(date) %>%
  summarise(
    across(-c(hhid,afe),
           ~mean(.))
  )%>%
  ungroup() %>%
  # pivot_longer(cols = -date) %>% 
  ggplot(aes(x = as.Date(date), y = energy_kcal)) + 
  geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(span = 0.8, color = 'blue') 


ess1819 %>% 
  left_join(ess1819_timestamp, by = "hhid") %>% 
  filter(date>as.Date("2017", "%Y")) %>% 
  filter(!is.na(energy_kcal)) %>% 
  # filter(item_code == 74) %>% 
  group_by(date) %>%
  summarise(
    across(-c(hhid,afe),
           ~mean(.))
  )%>%
  ungroup() %>%
  # pivot_longer(cols = -date) %>% 
  ggplot(aes(x = as.Date(date), y = energy_kcal)) + 
  geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(span = 0.8, color = 'blue') 

order(ess1819_timestamp$date)
ess1819_timestamp %>% 
  filter(date<as.Date("2016", "%Y"))


mwi1516 %>% 
  left_join(mwi1516_timestamp, by = "hhid") %>% 
  # filter(date>as.Date("2017", "%Y")) %>% 
  filter(!is.na(energy_kcal)) %>% 
  # filter(item_code == 74) %>% 
  group_by(date) %>%
  summarise(
    across(-c(hhid,afe),
           ~mean(.))
  )%>%
  ungroup() %>%
  # pivot_longer(cols = -date) %>% 
  ggplot(aes(x = as.Date(date), y = vitb12_mcg)) + 
  geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(span = 0.8, color = 'blue') 



nnmb_daily %>% 
  ungroup() %>% 
  group_by(date) %>%
  summarise(
    across(-c(SUBJECT),
           ~mean(.))
  )%>%
  ungroup() %>%
  # pivot_longer(cols = -date) %>% 
  ggplot(aes(x = as.Date(date), y = FOLDFE_mcg)) + 
  geom_point(alpha = 0.1) +
  ggplot2::geom_smooth(span = 0.8, color = 'blue') 




         