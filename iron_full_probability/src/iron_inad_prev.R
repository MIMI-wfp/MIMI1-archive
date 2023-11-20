## code to turn this chunk into a function

library(dplyr)
library(tidyr)

here::here()
eth_18_nd_fe <- read_csv(here::here("iron_full_probability/data/eth_18_nd_fe.csv"))
head(eth_18_nd_fe)

#Probabistic apparent intake inadequacy

fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 5){
  # Function that calculates the full probabilistic model for fe
  # Default, just calculates for full population
  # Add in parameters for group 1 and/or group 2 and it will calculate for
  # each sub population
  # The resultant data frame will have a column for each sub population with the 
  # first label corresponding to groups within group1, and the second label corresponding
  # to grousp within group2
  # Also can set the bioavailability to either 5%, 10% or 15%
  
  tryCatch(
  
  if(missing(group1)&missing(group2)){
    data %>% 
      mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
      mutate(prob_inad = 
               case_when(
                 bio_avail == 5 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                      ai_afe <= 15 ~ "1",
                      ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
                      ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
                      ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
                      ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
                      ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
                      ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
                      ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
                      ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
                      ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
                      ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
                      ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
                      ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
                      ai_afe > 63 ~ "0"),
                 bio_avail == 10 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 7.5 ~ "1",
                     ai_afe <= 8.4 & ai_afe > 7.5 ~ "0.96",
                     ai_afe <= 9.4 & ai_afe > 8.4 ~ "0.93",
                     ai_afe <= 10.7 & ai_afe > 9.4 ~ "0.85",
                     ai_afe <= 11.8 & ai_afe > 10.7 ~ "0.75",
                     ai_afe <= 12.9 & ai_afe > 11.8 ~ "0.65",
                     ai_afe <= 13.9 & ai_afe > 12.9 ~ "0.55",
                     ai_afe <= 15.1 & ai_afe > 13.9 ~ "0.45",
                     ai_afe <= 16.6 & ai_afe > 15.1 ~ "0.35",
                     ai_afe <= 18.7 & ai_afe > 16.6 ~ "0.25",
                     ai_afe <= 22.5 & ai_afe > 18.7 ~ "0.15",
                     ai_afe <= 26.7 & ai_afe > 22.5 ~ "0.08",
                     ai_afe <= 31.5 & ai_afe > 26.7 ~ "0.04",
                     ai_afe > 31.5 ~ "0"),
                 bio_avail == 15 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 5 ~ "1",
                     ai_afe <= 5.6 & ai_afe > 5 ~ "0.96",
                     ai_afe <= 6.2 & ai_afe > 5.6 ~ "0.93",
                     ai_afe <= 7.1 & ai_afe > 6.2 ~ "0.85",
                     ai_afe <= 7.9 & ai_afe > 7.1 ~ "0.75",
                     ai_afe <= 8.6 & ai_afe > 7.9 ~ "0.65",
                     ai_afe <= 9.3 & ai_afe > 8.6 ~ "0.55",
                     ai_afe <= 10.1 & ai_afe > 9.3 ~ "0.45",
                     ai_afe <= 11.1 & ai_afe > 10.1 ~ "0.35",
                     ai_afe <= 12.4 & ai_afe > 11.1 ~ "0.25",
                     ai_afe <= 15.0 & ai_afe > 12.4 ~ "0.15",
                     ai_afe <= 17.8 & ai_afe > 15.0 ~ "0.08",
                     ai_afe <= 21.0 & ai_afe > 17.8 ~ "0.04",
                     ai_afe > 21.0 ~ "0")
          )
        )%>% 
      group_by(prob_inad ) %>%     # Counting number of observations that fall into each probability category
      summarise(
        fe_prop = n()
      ) %>% 
      ungroup() %>% 
      summarise(   
        across(-prob_inad,
               ~ sum(.x*as.numeric(prob_inad))/sum(.x)*100
        )
      )
  }
  else{
      data %>%
        mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
        mutate(prob_inad = case_when(                            # Dividing intake distribution into probability of inadequacy catagories
          ai_afe <= 15 ~ "1",
          ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
          ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
          ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
          ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
          ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
          ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
          ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
          ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
          ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
          ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
          ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
          ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
          ai_afe > 63 ~ "0")) %>%
        group_by(prob_inad, {{group1}},{{group2}}) %>%     # Counting number of observations that fall into each probability category
        summarise(
          fe_prop = n()
        ) %>%
        ungroup() %>%
        pivot_wider(names_from = c({{group1}},{{group2}}), names_prefix = "fe_prop_", #this can be the name of the 'group by" group
                    values_from = fe_prop) %>%
        summarise(
          across(-prob_inad,
                 ~ sum(.x*as.numeric(prob_inad))/sum(.x)*100
          )
        )

  }
  
  )
}

eth_18_nd_fe %>% 
  fe_full_prob(bio_avail = 10)

eth_18_nd_fe %>% 
  fe_full_prob(hh_res)

eth_18_nd_fe %>% 
  e_full_prob(hh_res_sep)


eth_18_nd_fe %>% 
  fe_full_prob(hh_res,hh_res_sep)
