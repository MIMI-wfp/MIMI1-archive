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
nga_lss1819 <- apparent_intake("nga_lss1819")

######################### MEAN ADEQUACY RATIO ##################################

allen_ear

mean_adequacy_ratio <- function(ai_df){
  
  # calcalate the mean adequacy ratio for our data
  nar <- function(intake, ref_value){
    return(ifelse(intake<ref_value, intake/ref_value,1))
    }

  
  final_df <- ai_df %>%
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

calculate_adequacy_prob <- function(ref_value, micronutrient, number_dist = 1000){
  
  micronutrient_normal <- function(micronutrient){
    y <- allen_ear %>% 
      filter(nutrient %in% c(
        'vita_rae_mcg',
        'folate_mcg',
        'vitb12_mcg',
        'fe_mg',
        'zn_mg'
      )) %>% 
      mutate(sd = 0.1*ear_value)
    
    #calculate the normal distribution 
    normal = rnorm(n = 1000, mean = y$ear_value[[which(y$nutrient == {{micronutrient}})]], sd =  y$sd[[which(y$nutrient == {{micronutrient}})]])
    
    #create a list of the normal distribution in order 
    ordered_normal = normal[order(normal)]
    return(ordered_normal)
  }
  
  ordered_normal = micronutrient_normal(micronutrient)
  
  # find where the position of where the intake is relative to a normal distribution
  # around 
  if(length(which(ref_value>ordered_normal)) == 0){
    position = 1
  }else {
    position = max(which(ref_value>ordered_normal))
  }
  
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
# need to vectorise to be able to use within  dplyr
calculate_adequacy_prob <- Vectorize(calculate_adequacy_prob)



# Mapping MAR/MPII against intake ----------------------------------------------

mpa_vs_micronutrient <- function(df, micronutrient){
  # 
    
  mpa <- df %>%
    pivot_longer(-hhid, names_to = "nutrient") %>% 
    filter(nutrient %in% c(
      'vita_rae_mcg',
      'folate_mcg',
      'vitb12_mcg',
      'fe_mg',
      'zn_mg'
    )) %>% 
    mutate(adequacy_prob = calculate_adequacy_prob(value, nutrient)) %>% 
    group_by(hhid) %>% 
    summarize(mpa = mean(adequacy_prob)) 
  
  mpa <- mpa %>% 
    left_join(df %>%  
                pivot_longer(-hhid, names_to = "nutrient") %>% 
                filter(nutrient %in% c(
                  'vita_rae_mcg',
                  'folate_mcg',
                  'vitb12_mcg',
                  'fe_mg',
                  'zn_mg'
                )) %>% 
                left_join(allen_ear, by= "nutrient") %>% 
                mutate(inad = ifelse(value<ear_value, 1,0)),
              by = "hhid")
  
  mn_selection <- mpa %>% 
    filter(nutrient == {{micronutrient}})

    
  mn_selection %>% 
    ggplot(aes(x = mpa, y = value))+
    geom_point(color = 'pink', alpha = 0.6) +
    geom_smooth(method=lm)  + 
    ylab("micronutrient intake")+
    xlab("Mean probability of Adequacy")+
    labs(title = micronutrient)+
    theme_ipsum()


}
mpa_vs_micronutrient(ind_nss1112,"vita_rae_mcg")




mar_vs_micronutrient <- function(df, micronutrient){
  # 
  
  mar <- mean_adequacy_ratio(df) %>% 
    left_join(df %>%  
                pivot_longer(-hhid, names_to = "nutrient") %>% 
                filter(nutrient %in% c(
                  'vita_rae_mcg',
                  'folate_mcg',
                  'vitb12_mcg',
                  'fe_mg',
                  'zn_mg'
                )), by = "hhid")
  
  
  
  mn_selection <- mar %>% 
    filter(nutrient == {{micronutrient}})
  
  
  mn_selection %>% 
    ggplot(aes(x = mar, y = value))+
    geom_point(color ='lightblue', alpha = 0.6) +
    geom_smooth(method=lm)  + 
    ylab("micronutrient intake")+
    xlab("Mean Adequacy Ratio")+
    labs(title = micronutrient)+
    theme_ipsum()
  
  
}
mar_vs_micronutrient(ind_nss1112, "folate_mcg")


mar_vs_mpa <- function(df){
  mar <- mean_adequacy_ratio(df) %>% 
    left_join(df %>%  
                pivot_longer(-hhid, names_to = "nutrient") %>% 
                filter(nutrient %in% c(
                  'vita_rae_mcg',
                  'folate_mcg',
                  'vitb12_mcg',
                  'fe_mg',
                  'zn_mg'
                )), by = "hhid")
  
  mpa <- df %>%
    pivot_longer(-hhid, names_to = "nutrient") %>% 
    filter(nutrient %in% c(
      'vita_rae_mcg',
      'folate_mcg',
      'vitb12_mcg',
      'fe_mg',
      'zn_mg'
    )) %>% 
    mutate(adequacy_prob = calculate_adequacy_prob(value, nutrient)) %>% 
    group_by(hhid) %>% 
    summarize(mpa = mean(adequacy_prob)) %>% 
    left_join(df %>%  
                pivot_longer(-hhid, names_to = "nutrient") %>% 
                filter(nutrient %in% c(
                  'vita_rae_mcg',
                  'folate_mcg',
                  'vitb12_mcg',
                  'fe_mg',
                  'zn_mg'
                )) %>% 
                left_join(allen_ear, by= "nutrient") %>% 
                mutate(inad = ifelse(value<ear_value, 1,0)),
              by = "hhid")
  
  
  mpa_mar <- mpa %>% 
    left_join(mar, by = "hhid")
  
  mpa_mar %>% 
    ggplot(aes(x = mar, y = mpa))+
    geom_point(color ='limegreen', alpha = 0.6) +
    geom_smooth(method=)  + 
    ylab("Mean probability of adequacy")+
    xlab("Mean Adequacy Ratio")+
    labs(title = "MAR vs MPA")+
    theme_ipsum()
  
  
  
}

mar_vs_mpa(ind_nss1112)

# ------------------------------------------------------------------------------
# rubbish

x <- x %>% 
  group_by(hhid) %>% 
  summarize(mpii = 1-mean(adequacy_prob)) 

z <- ind_nss1112 %>%
  pivot_longer(-hhid, names_to = "nutrient") %>% 
  filter(nutrient %in% c(
    'vita_rae_mcg',
    'folate_mcg',
    'vitb12_mcg',
    'fe_mg',
    'zn_mg'
  )) %>% 
  left_join(allen_ear, by= "nutrient") %>% 
  mutate(inad = ifelse(value<ear_value, 1,0))
  
  
w <-  z %>% 
  left_join(x, by = "hhid") %>% 
  filter(nutrient == "zn_mg")
  # facet_wrap(facets = vars(nutrient))

glm(mpii~inad,family=binomial(link='logit'),data=w)

exp(0.3468)


y <- mean_adequacy_ratio(ind_nss1112)
y %>% 
  ggplot(aes(x = mar))+
  geom_histogram()



nga_density_mpa <- nga_lss1819 %>%
  pivot_longer(-hhid, names_to = "nutrient") %>% 
  filter(nutrient %in% c(
    'vita_rae_mcg',
    'folate_mcg',
    'vitb12_mcg',
    'fe_mg',
    'zn_mg'
  )) %>% 
  mutate(adequacy_prob = calculate_adequacy_prob(value, nutrient)) %>% 
  group_by(hhid) %>% 
  summarize(mpii = 1-mean(adequacy_prob)) %>% 
  ggplot(aes(x = mpii)) +
  geom_histogram()



mean_adequacy_ratio(nga_lss1819) %>% 
  ggplot(aes(x = mar))+
  geom_histogram()



ggplot() +
  geom_density(data = nga_lss1819, aes(x = vita_rae_mcg), fill = 'red') +
  # geom_density(aes(x = micronutrient_normal("vita_rae_mcg"))) +
  geom_density(data = data.frame(vita_rae_mcg = micronutrient_normal("vita_rae_mcg")),aes(x = vita_rae_mcg), fill = "blue", alpha = 0.6) 


ggplot() +
  geom_density(data = nga_lss1819, aes(x = folate_mcg), fill = 'red') +
  # geom_density(aes(x = micronutrient_normal("vita_rae_mcg"))) +
  geom_density(data = data.frame(folate_mcg = micronutrient_normal("folate_mcg")),aes(x = folate_mcg), fill = "blue", alpha = 0.6) 

ggplot() +
  geom_density(data = nga_lss1819, aes(x = vitb12_mcg), fill = 'red') +
  # geom_density(aes(x = micronutrient_normal("vita_rae_mcg"))) +
  geom_density(data = data.frame(vitb12_mcg = micronutrient_normal("vitb12_mcg")),aes(x = vitb12_mcg), fill = "blue", alpha = 0.6) 

ggplot() +
  geom_density(data = nga_lss1819, aes(x = fe_mg), fill = 'red') +
  # geom_density(aes(x = micronutrient_normal("vita_rae_mcg"))) +
  geom_density(data = data.frame(fe_mg = micronutrient_normal("fe_mg")),aes(x = fe_mg), fill = "blue", alpha = 0.6) 


ggplot() +
  geom_density(data = nga_lss1819, aes(x = zn_mg), fill = 'red') +
  # geom_density(aes(x = micronutrient_normal("vita_rae_mcg"))) +
  geom_density(data = data.frame(zn_mg = micronutrient_normal("zn_mg")),aes(x = zn_mg), fill = "blue", alpha = 0.6) 
                    