#########################################
#        Risk indicator                 #
#########################################


# Author: Gabriel Battcock
# Created: 29 April 24
# Last updated: 29 April 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2", "tidyr",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

#-------------------------------------------------------------------------------

# read in the apparent intake for india NSSO 2011-12
ind_nss1112 <- apparent_intake("ind_nss1112")


######################### MEAN ADEQUACY RATIO ##################################

allen_ear

mean_adequacy_ratio <- function(ai_df){
  
  # calcalate the mean adequacy ratio for our data
  nar <- function(intake, ref_value){
    return(ifelse(intake<ref_value, intake/ref_value,1))
    }

  
  final_df <- df %>%
    pivot_longer(-hhid, names_to = "nutrient") %>% 
    right_join(allen_ear, by= "nutrient") %>% 
    filter(nutrient %in% c(
      'vita_rae_mcg',
      'folate_mcg',
      'vitb12_mcg',
      'fe_mg',
      'zn_mg'
    )) %>% 
    mutate(
      nar = nar(value,ear_value)
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      mar = mean(nar)
    )
  
  return(final_df)
}


######################### MEAN A##################################

ind_nss1112 %>%
  pivot_longer(-hhid, names_to = "nutrient") %>% 
  right_join(allen_ear, by= "nutrient") %>% 
  filter(nutrient %in% c(
    'vita_rae_mcg',
    'folate_mcg',
    'vitb12_mcg',
    'fe_mg',
    'zn_mg'
  )) %>% 
  mutate(sd = 0.1*ear_value) 


calculate_adequacy_prob <- function(ref_value, standard_deviation, intake, number_dist = 1000){
  
  #calculate the normal distribution 
  normal = rnorm(n = number_dist, mean = ref_value, sd = standard_deviation)
  
  #create a list of the normal distribution in order 
  ordered_normal = normal[order(normal)]
  
  # find where the position of where the intake is relative to a normal distribution
  # around 
  position = max(which(intake>ordered_normal))
  
  p <- 0
  
  if(position == 1){
    p = 0
  } else if(position == number_dist){p = 1
  } else {
    p = position/number_dist
    p
  }
  
 
  
  
  return(p)

}

ind_nss1112 %>%
  pivot_longer(-hhid, names_to = "nutrient") %>% 
  right_join(allen_ear, by= "nutrient") %>% 
  filter(nutrient %in% c(
    'vita_rae_mcg',
    'folate_mcg',
    'vitb12_mcg',
    'fe_mg',
    'zn_mg'
  )) %>% 
  mutate(sd = 0.1*ear_value) %>% 
  mutate(prob_adequate = calculate_adequacy_prob(ear_value[1], sd[1], value[1]))



#calculate the normal distribution 
normal = rnorm(n = 1000, mean = x$ear_value[2], sd = x$sd[2])

#create a list of the normal distribution in order 
ordered_normal = normal[order(normal)]

# find where the position of where the intake is relative to a normal distribution
# around 
position = max(which(x$value[2]>ordered_normal))

p <- 0

if(position == 1){
  p = 0
} else if(position == 1000){p = 1
} else {
  p = position/1000
  p
}



