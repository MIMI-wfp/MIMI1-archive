### Validating the intake data
### This script reads in the consumption data and demographic data from 
### the NSSO 2012 and maps it to the IFCT2017. 
### AFE for each hh is calculated
### Food intake data is validated through looking at the distributions.
### Micronutrient intake distributions are also plotted

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

source("India_analysis/src/processing/food_matching.R")

# read in cleaned and mathced data
path_to_data = here::here("India_analysis", "data", "processed/")

consumption <- read_csv(paste0(path_to_data, "consumption.csv"))
demographics <- read_csv(paste0(path_to_data,"demographics.csv"))
household_characteristics <- read_csv(paste0(path_to_data, "household_char.csv"))

india_fct <- read_csv(paste0(path_to_data, "matched_fct.csv"))
conversion <- read_csv(paste0(path_to_data, "conversion_factors.csv"))
hdds <- read_csv(here::here("India_analysis", "data", "raw","hdds_nsso.csv"))

sum_or_function <- function(x){
  #### TO DO ## explain the function
  
  #creates OR logic gate
  y = sum(x, na.rm = TRUE)
  y = as.numeric(ifelse(y != 0, 1, 0))
  y
}

# View(household_characteristics)

# working data frame of matched food items to composition
# can now aggregate by HH or by individual item to test and validate
# distributions of key food items and micronutrients


#### Calculate daily consumption per household -----------------------------

daily_food_items_consumed <-consumption %>% 
  dplyr::left_join(
    india_fct, by = "Item_Code"
  ) %>% 
  dplyr::left_join(
    conversion, by = c("Item_Code", "item_name")
  ) %>% 
  #split the name without the unit of consumption
  dplyr::mutate(
    item_name = stringr::str_split_i(item_name,"\\(", 1)
  ) %>% 
  dplyr::mutate(
    Total_Consumption_Quantity = ifelse(is.na(conversion_factor),
                                        Total_Consumption_Quantity,
                                        Total_Consumption_Quantity*conversion_factor)
  ) %>% 
  dplyr::left_join(
    household_characteristics %>% dplyr::select(HHID, District_code),
    by = "HHID"
  ) %>% 
  #create state name
  dplyr::mutate(
    quantity_100g = Total_Consumption_Quantity/100,
    State_name = dplyr::case_when(
      State_code == "09" ~ "Uttar Pradesh", 
      State_code == "10" ~ "Bihar",
      State_code == "22" ~ "Chhattisgarh"
    )
  ) %>% 
  dplyr::select(
    -c( Home_Produce_Quantity,Home_Produce_Value,Total_Consumption_Quantity,Total_Consumption_Value)
  ) %>% 
  dplyr::filter(
    !is.na(item_name) &
      !is.na(quantity_100g)
  ) %>%
  dplyr::mutate(
    dplyr::across(
      -c(item_name, Item_Code, State_code, District_code, HHID, quantity_100g, State_name, conversion_factor),
      ~.x*quantity_100g/30
    )
  ) %>% 
  dplyr::mutate(
    quantity_100g = quantity_100g/30
  ) 


#check the energy distribution of households with foods consumed outside the household

meals_consumed_outside_house <-  daily_food_items_consumed%>%
  dplyr::filter(
    (HHID %in% daily_food_items_consumed$item_name[daily_food_items_consumed$Item_Code %in% c('280','281', '282')])
  ) %>% 
  dplyr::distinct(HHID)

4130/15000

# 
# daily_food_items_consumed%>%
#   dplyr::filter(
#     HHID == "412001101"
#   )
# 
#   
# 
# meals_consumed_outside_house %>% 
#   dplyr::left_join(
#     household_afe, by = c("HHID")
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       -c( HHID, State_code,Item_Code,item_name, District_code,capita,afe, State_name),
#       ~.x/afe
#     )) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(HHID) %>%
#   dplyr::summarise(
#       energy = sum(energy_kcal,na.rm=TRUE)
#     ) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(energy <5000) %>% 
#   ggplot(aes(x = energy))+
#   geom_histogram()




#### Calculate adult female equivalent per hh -----------------------------------


##### Assumptions and conditions ##############
# Using the NIN_ICMR energy requirement for Indians
# Requirements for Infants (under 1yo) have been averaged as there is no granularity
# in months.
# All adults are assumed to have moderate energy expenditure
# Men are assumed to have 65 kg and women 55 kg


children_under_2 <- demographics %>% 
  dplyr::group_by(HHID) %>% 
  dplyr::summarise(
    under_2 = factor(ifelse(
      sum(Age < 2) >= 1,
      1,
      0
    )
    )
  )

summary(children_under_2)

adult_female_requirement <- 2130

demographics <- demographics %>%
  dplyr::left_join(children_under_2, by = "HHID") %>% 
  dplyr::mutate(
    energy_requirement = 
      dplyr::case_when(
        Age < 1 ~ 595,
        Age < 4 ~ 1070,
        Age < 7 ~ 1360,
        Age < 10 ~ 1700,
        Age < 13 ~ ifelse(Sex == "Male", 2220, 2060),
        Age < 16 ~ ifelse(Sex == "Male", 2860, 2400),
        Age < 18 ~ ifelse(Sex == "Male", 2860, 2400),
        Age >= 18 ~ ifelse(Sex == "Male", 2710,
                           ifelse(Age<50, 2130,
                            ifelse(under_2 == 0, 
                                  2130,
                                  2690)))# can add in a lactating condition if needed
      ) 
  ) %>% 
  dplyr::mutate(
    afe = round(energy_requirement/adult_female_requirement,
                2)
  )

# for the households

household_afe <- 
  demographics %>% 
    dplyr::group_by(
      HHID
    ) %>% 
    dplyr::summarise(
      capita = dplyr::n(),
      afe = sum(afe)
    )

#check that capita and afe are not mapped completely wrong

household_afe %>% 
  dplyr::mutate(
    ratio = capita/afe
  ) %>% 
  ggplot(aes(x = capita, y = afe)) + 
  geom_point(alpha = 0.8, color = "darkblue") + 
  geom_abline(intercept = 0, slope =1)+
  labs(x = "Capita", y = "AFE") +
  theme_ipsum()

#### Distributions #############################################################

##### Food item distributions --------------------------------------------------

total_households <- dplyr::n_distinct(daily_food_items_consumed$HHID)

food_items_grouped <- daily_food_items_consumed %>% 
  dplyr::left_join(
    household_afe, by = c("HHID")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      -c( HHID, State_code,Item_Code,item_name, District_code,capita,afe, State_name),
      ~.x/capita
    )) %>% 
  dplyr::group_by(
    Item_Code,
    item_name,
  ) %>% 
  dplyr::mutate(quantity_g = quantity_100g*100) %>% 
  dplyr::left_join(hdds %>% dplyr::select(-item_name), by = c("Item_Code")) %>% 
  dplyr::group_by(Item_Code, item_name) %>%
  tidyr::pivot_longer(cols = c(A_cereals,B_roots_tubers,C_vegetables,
                               D_fruits,E_meat,F_eggs,G_fish,H_pulses,
                               I_milk,J_oil,J_sugar,L_misc)#HDDS food groups
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value) %>%
  dplyr::rename(hdds_groups = name) %>%
  # dplyr::mutate(
  #   hdds_groups = factor(hdds_groups)
  # ) %>%
  dplyr::ungroup()

# food_items_grouped %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(Item_Code == 160) %>%
#   # dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
#   ggplot(aes(x = quantity_g)) +
#   geom_histogram()
# 


#### For each household, replace any uncomsumed items with a 0

full_list <- daily_food_items_consumed %>% 
  dplyr::distinct(HHID, State_code) %>% 
  dplyr::cross_join(india_fct %>% 
                      dplyr::mutate(
                        item_name = stringr::str_split_i(item_name,"\\(", 1)
                      ) %>% 
                      dplyr::select(Item_Code, item_name))

# summary of food items consumed
food_item_summary_state <- 
  # micronutrient_distributions %>% 
  # dplyr::select(HHID) %>% 
  # dplyr::left_join(
    full_list %>% 
# ,
                   # by = "HHID") %>% 
  dplyr::left_join(food_items_grouped, 
                   by = c("HHID", "Item_Code","item_name","State_code")) %>% 
  dplyr::mutate(
    dplyr::across(
      -c("HHID", "Item_Code","item_name","State_code","conversion_factor","District_code","quantity_100g" ,"State_name","capita","afe",              
         "hdds_groups"),
      ~tidyr::replace_na(.x,0)
    )
  ) %>% 
  dplyr::left_join(household_characteristics %>% 
                     dplyr::select(HHID, Combined_multiplier), by = "HHID")


# create a summary table of the mean intake
food_item_summary_state %>% 
  srvyr::as_survey_design(id = HHID, strata = State_code, weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(Item_Code, item_name, State_code) %>% 
  # srvyr::filter(quantity_g<stats::quantile(quantity_g, 0.999, na.rm = TRUE)[[1]]) %>%
  srvyr::summarise(mean_g = mean(quantity_g, na.rm = T),
                   median_g = median(quantity_g, na.rm = T),
                   low_95 = mean(quantity_g, na.rm = T)-1.96*sd(quantity_g, na.rm = T)/sqrt(dplyr::n()),
                   up_95 = mean(quantity_g, na.rm = T)+1.96*sd(quantity_g, na.rm = T)/sqrt(dplyr::n()),
                   
  )

# this shows we have the same item intake as TATA-NIN


food_item_summary_state %>% 
  dplyr::filter(Item_Code==102 & State_code == "09") %>% 
  ggplot(aes(quantity_g)) +
  geom_histogram()


# not split by state
food_item_summary <- food_items_grouped %>% 
  dplyr::left_join(household_characteristics %>% 
                     dplyr::select(HHID, Combined_multiplier), by = "HHID") %>% 
  srvyr::as_survey_design(id = HHID, weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(Item_Code, item_name, hdds_groups) %>% 
  srvyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
  srvyr::summarise(mean_g = mean(quantity_g),
                   low_95 = mean(quantity_g)-1.96*sd(quantity_g)/sqrt(dplyr::n()),
                   up_95 = mean(quantity_g)+1.96*sd(quantity_g)/sqrt(dplyr::n()),
                   n_hh_consumed = dplyr::n(),
                   perc_hh_consumed = dplyr::n()/total_households*100
  )
  
##### Create tables -------------------------------------------------------------

food_item_summary %>% 
  dplyr::filter(
    hdds_groups == "A_cereals"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(Item_Code,hdds_groups, n_hh_consumed)) %>%
  dplyr::mutate(dplyr::across(-item_name,
                round, 
                2)) %>% 
  gt() |>
  tab_options(table.font.names = 'Arial',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 15,
              heading.subtitle.font.size = 15,
              
              source_notes.font.size = 10,
              #source_notes.
              table.font.size = 13) |>
  tab_header(title = "Intake of cereals",
            )|>
  cols_label(
    item_name = "Food item",
    mean_g = "Mean intake per AFE (g)",
    low_95 = "Lower 95CI",
    up_95 = "Upper 95CI",
    perc_hh_consumed = "Percentage of households consumed"
  )


food_item_summary %>% 
  dplyr::filter(
    hdds_groups%in%c("E_meat","G_fish","F_eggs","I_milk")
                     
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(Item_Code,hdds_groups, n_hh_consumed)) %>%
  dplyr::mutate(dplyr::across(-item_name,
                              round, 
                              2)) %>% 
  gt() |>
  tab_options(table.font.names = 'Arial',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 15,
              heading.subtitle.font.size = 15,
              
              source_notes.font.size = 10,
              #source_notes.
              table.font.size = 13) |>
  tab_header(title = "Intake of ASF",
  )|>
  cols_label(
    item_name = "Food item",
    mean_g = "Mean intake per AFE (g)",
    low_95 = "Lower 95CI",
    up_95 = "Upper 95CI",
    perc_hh_consumed = "Percentage of households consumed"
  )

food_item_summary %>% 
  dplyr::filter(
    hdds_groups%in%c("C_vegetables")
    
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(Item_Code,hdds_groups, n_hh_consumed)) %>%
  dplyr::mutate(dplyr::across(-item_name,
                              round, 
                              2)) %>% 
  gt() |>
  tab_options(table.font.names = 'Arial',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 15,
              heading.subtitle.font.size = 15,
              
              source_notes.font.size = 10,
              #source_notes.
              table.font.size = 11) |>
  tab_header(title = "Intake of  Vegetables",
  )|>
  cols_label(
    item_name = "Food item",
    mean_g = "Mean intake per AFE (g)",
    low_95 = "Lower 95CI",
    up_95 = "Upper 95CI",
    perc_hh_consumed = "Percentage of households consumed"
  )


# splitting by food group, look at the individual distributions

groups <- food_items_grouped %>% 
  dplyr::group_by(hdds_groups) %>% 
  dplyr::group_split()

names_hdds_groups <- food_items_grouped %>% 
  dplyr::select(hdds_groups) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange()

food_group_plots_list <- list()
i <- 1
for(item in groups){
  # print(item)
  food_group_plot <-  item %>%
    dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
    ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
      geom_density_ridges(stat = "binline", show.legend = FALSE) +
      scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
      facet_wrap(facets = vars(item_name)) +
      theme_ridges() +
      ylab("") +
      xlab("Quantity (g)")
  
 
 food_group_plots_list[[i]] <- food_group_plot
 i <- i+1
}


 
#cereals
groups[[1]] %>% 
  dplyr::filter(Item_Code%in%c(102,108,101,107)) %>% 
  dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
  facet_wrap(facets = vars(item_name)) +
  theme_ridges() +
  labs(title = "Cereals")+
  ylab("") +
  xlab("Quantity (g)")

groups[[3]] %>% 
  dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
  facet_wrap(facets = vars(item_name)) +
  theme_ridges() +
  labs(title = "Vegetables")+
  ylab("") +
  xlab("Quantity (g)")

x <- groups[[5]]

groups[[5]] %>% 
  dplyr::bind_rows(groups[[6]]) %>% 
  dplyr::bind_rows(groups[[7]]) %>% 
  # dplyr::bind_rows(groups[[9]]) %>% 
  dplyr::filter(Item_Code!=196) %>% 
  dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>%
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
  facet_wrap(facets = vars(item_name)) +
  theme_ridges() +
  labs(title = "Animal sourced foods")+
  ylab("") +
  xlab("Quantity (g)")



  

# total bar charts for food group

daily_food_items_consumed %>% 
  dplyr::left_join(
    household_afe, by = c("HHID")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      -c( HHID, State_code,Item_Code,item_name, District_code,capita,afe, State_name),
      ~.x/afe
    )) %>% 
  dplyr::group_by(
    Item_Code,
    item_name,
  ) %>% 
  dplyr::mutate(quantity_g = quantity_100g*100) %>% 
  dplyr::filter(!is.na(State_code)) %>% 
  dplyr::left_join(hdds %>% dplyr::select(-item_name), by = c("Item_Code")) %>% 
  dplyr::group_by(HHID, State_code) %>%
  dplyr::mutate(
    dplyr::across(
      c(A_cereals,B_roots_tubers,C_vegetables,
        D_fruits,E_meat,F_eggs,G_fish,H_pulses,
        I_milk,J_oil,J_sugar,L_misc),
      ~ifelse(is.na(.), 0, 1)
    )
  ) %>% 
  dplyr::summarise(
    dplyr::across(
      c(A_cereals,B_roots_tubers,C_vegetables,
        D_fruits,E_meat,F_eggs,G_fish,H_pulses,
        I_milk,J_oil,J_sugar,L_misc),
      ~sum_or_function(.)
    )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(State_code) %>% 
  dplyr::summarise(
    dplyr::across(
      -HHID,
      ~sum(.)/dplyr::n(),
      .names = "{.col}"
      
    )
  ) %>%
  tidyr::pivot_longer(
    cols = c(-State_code)
  ) %>% 
  ggplot(aes(x = name, y = value, group = State_code, fill = State_code)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
  # facet_wrap(facets = vars(item_name)) +
  theme_ipsum()

# total intake stacked density plot for food groups
##### TO DO




#### Micronutrients distributions -------------------------------------------------
micronutrient_distributions <- food_items_grouped %>% 
  dplyr::ungroup() %>% 
  dplyr::select(
    c("HHID","State_code",State_name,capita, "District_code", "energy_kcal", "vita_mg", "vitb1_mg", "vitb2_mg", "vitb3_mg", "vitb5_mg",
      "vitb6_mg", "folate_ug","vitb9_ug","vitaminb12_in_mg", "iron_mg", "calcium_mg", "zinc_mg")
  ) %>% 
  dplyr::group_by(HHID, State_code,State_name, District_code,capita) %>% 
  dplyr::summarise(
    dplyr::across(
      everything(),
      ~sum(.x, na.rm = TRUE)
    )
  ) %>% 
  #calculate the per capita consumption to play with the data
  dplyr::ungroup() %>% 
  dplyr::filter(
    energy_kcal<5000
  )

  
summary(micronutrient_distributions)

x <- food_items_grouped %>% 
  dplyr::filter(HHID == "412001102")

write_csv(micronutrient_distributions, here::here(
  "India_analysis/data/final/base_model.csv"
))

micronutrient_distributions %>% 
  
  ggplot(aes(x = energy_kcal)) + 
  geom_histogram()
  

#look at the distributions of:
#     # Vit A, B1 2 3 5 6 9 12 Fe Zn Ca kcal
micronutrients <- colnames(micronutrient_distributions)
micronutrients <- micronutrients[-c(1:4)]

#create a data frame of adult women EAR values
nin_ear <- data.frame(
  energy_kcal = 2130,
  vita_mg  = 390, 
  vitb1_mg = 1.4,
  vitb2_mg = 2.0, 
  vitb3_mg = 12, 
  vitb5_mg = 4,#from allen 2020
  vitb6_mg = 1.6, 
  folate_ug = 180, 
  vitb9_ug = 180,
  vitaminb12_in_mg = 2, 
  iron_mg = 15, 
  calcium_mg = 800, 
  zinc_mg = 11
)

mn_plots_list <- list()
# create a list of 
for(item in micronutrients){
  print(item)
  p1 <- micronutrient_distributions %>%
    ggplot(aes(x = !!sym(item), y = State_name, fill = State_name)) +
    geom_density_ridges(stat = "binline", show.legend = FALSE)+
    # geom_histogram(position = "dodge", aes(fill = State_name), alpha = 0.99) +
    # geom_density(aes(y = after_stat(density), color = State_name))+
    geom_vline(data = nin_ear, aes(xintercept = !!sym(item)), color = 'red')+
    # stat_count(width = 0.5)+
    scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))+
    # scale_color_manual(values = wes_palette("GrandBudapest1", n = 3))+
    labs(
      title = stringr::str_to_title( 
        stringr::str_split_i(item,
                                    "\\_",
                                    1)
      ),
      x = stringr::str_split_i(item,
                             "\\_",
                             2),
      y = ""
    )+
    theme_ipsum()
  mn_plots_list[[item]] <- p1
}

# subtitle <- cowplot::ggdraw() + 
#   cowplot::draw_label(  "Red line is EAR for an adult woman", type = "title")
cowplot::plot_grid(plotlist = mn_plots_list)


## Distributions of micronutrients from food groups

micronutrient_food_groups <- food_items_grouped %>% 
  dplyr::select(
    c("HHID",hdds_groups, "energy_kcal", "vita_mg", "vitb1_mg", "vitb2_mg", "vitb3_mg", "vitb5_mg",
      "vitb6_mg", "folate_ug","vitb9_ug","vitaminb12_in_mg", "iron_mg", "calcium_mg", "zinc_mg")
  ) %>% 
  dplyr::group_by(HHID,hdds_groups) %>% 
  dplyr::summarise(
    dplyr::across(
      everything(),
      ~sum(.x, na.rm = TRUE)
    )
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() %>% 
  # dplyr::filter(
  #   energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]
  # ) %>% 
  dplyr::group_by(
     hdds_groups
  ) %>% 
  dplyr::summarise(
    dplyr::across(
      -c(HHID),
      ~mean(.)
    )
  ) 


means_micronutrient_state <- micronutrient_distributions %>% 
  dplyr::left_join(
    household_characteristics %>% 
      dplyr::select(
        HHID, Subsample_multiplier
      ), by = "HHID"
   ) %>%
  srvyr::as_survey_design(id = HHID, strata = State_code, weights = Subsample_multiplier, nest=T) %>%
  srvyr::group_by(State_name) %>%
  srvyr::summarise(
    srvyr::across(-c(HHID,State_code, District_code,Subsample_multiplier),
                  .fns = list(
                    mean = ~mean(.x, na.rm = TRUE), 
                    sd = ~sd(.x, na.rm = TRUE), 
                    se = ~sd(.x, na.rm = TRUE)/sqrt(length(.x)),
                    n = ~dplyr::n(),
                    ci_l = ~mean(.x, na.rm = TRUE) - (1.96 * sd(.x, na.rm = TRUE)/sqrt(dplyr::n())),
                    ci_u = ~mean(.x, na.rm = TRUE) + (1.96 * sd(.x, na.rm = TRUE)/sqrt(dplyr::n())))))
  

#check the households with low energy
low_energy <- micronutrient_distributions %>% 
  dplyr::filter(
    energy_kcal<1000
  )

x <- low_energy %>% 
  dplyr::select(HHID) %>% 
  dplyr::left_join(
    daily_food_items_consumed,
    by = "HHID"
  )




mn_fg_plots <- list()
  for(item in micronutrients){
    print(item)
      p1 <-  micronutrient_food_groups %>% 
        ggplot(aes(area = !!sym(item), 
                   fill = stringr::str_to_title(
                     stringr::str_split_i(hdds_groups,
                                          "\\_",
                                          2)), 
                   label = stringr::str_to_title(
                   stringr::str_split_i(hdds_groups,
                                                "\\_",
                                                2)))
                   )+
        geom_treemap() +
        geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                           grow = FALSE, size = 12)+
        labs(title = stringr::str_to_title(stringr::str_split_i(item,
                                          "\\_",
                                          1)),
             
                                  )+
        scale_fill_brewer(palette = "Set3")+
        guides(fill=guide_legend(title="Food group"))+
        theme_ipsum()
    mn_fg_plots[[item]] <- p1
  }

ggpubr::ggarrange(plotlist = mn_fg_plots, common.legend = TRUE)


# micronutrient_food_groups %>% 
#  ggplot(aes(area = energy_kcal, fill = hdds_groups, label = hdds_groups))+
#   geom_treemap() +
#   geom_treemap_text( colour = "darkblue", place = "centre", alpha = 0.6,
#                     grow = TRUE)+
#   facet_wrap(vars(State_name), nrow = 2) +
#   scale_fill_brewer(palette = "Paired")+
#   labs(title = item)+
#   theme_ipsum()

#total percentage of consumed food items
food_items_grouped %>% 
  dplyr::group_by(HHID) %>% 
  dplyr::summarise(
    grams_perc = sum(quantity_g[energy_kcal!=0])/sum(quantity_g)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(
    mean_matched_foods = mean(grams_perc),
    lower = mean(grams_perc)-1.96*sd(grams_perc)/sqrt(dplyr::n()),
    upper = mean(grams_perc)+1.96*sd(grams_perc)/sqrt(dplyr::n()),
    sd = sd(grams_perc)
  )

# rm(list = ls())

