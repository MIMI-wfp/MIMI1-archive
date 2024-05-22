#########################################
#        India base model               #
#        plots and tables               #
#########################################

# Author: Gabriel Battcock
# Created: 10 Nov 23
# Last updated: 8 May 24

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

source(here::here("data_rich/India/src/procesing/data_validation.R"))


################################################################################


#check that capita and afe are not mapped completely wrong

household_afe %>% 
  dplyr::mutate(
    ratio = capita/afe
  ) %>% 
  ggplot(aes(x = capita, y = afe)) + 
  geom_point(alpha = 0.2, color = "darkblue") + 
  # geom_point(aes(x = as.numeric(HH_Size), y = afe), color = "red",alpha = 0.2)+
  geom_abline(intercept = 0, slope =1)+
  labs(x = "Capita", y = "AFE") +
  theme_ipsum()



consumption %>%
  left_join(household_afe,by = "HHID") %>% 
  mutate(per_capita_consumption = Total_Consumption_Quantity/capita) %>% 
  group_by(State_code, Item_Code) %>% 
  summarise(mean = mean(per_capita_consumption, na.rm = TRUE),
            median = median(per_capita_consumption, na.rm = TRUE))

#### Distributions #############################################################

##### Food item distributions --------------------------------------------------

total_households <- dplyr::n_distinct(daily_food_items_consumed$HHID)

food_items_grouped <- daily_food_items_consumed %>% 
  dplyr::left_join(
    household_afe, by = c("HHID")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      -c( HHID, State_code,Item_Code,item_name, District_code,capita,afe),
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
  full_list  %>% 
  # by = "HHID") %>% 
  dplyr::left_join(food_items_grouped %>% 
                     dplyr::filter(!is.na(energy_kcal)), 
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
food_intake_by_state <- food_item_summary_state %>% 
  srvyr::as_survey_design(id = HHID, strata = State_code, weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(Item_Code, item_name, State_code) %>% 
  srvyr::summarise(mean_g = mean(quantity_g, na.rm = T),
                   median_g = median(quantity_g, na.rm = T),
                   low_95 = mean(quantity_g, na.rm = T)-1.96*sd(quantity_g, na.rm = T)/sqrt(dplyr::n()),
                   up_95 = mean(quantity_g, na.rm = T)+1.96*sd(quantity_g, na.rm = T)/sqrt(dplyr::n()),
                   
  ) %>% 
  srvyr::ungroup() %>% 
  dplyr::mutate(
    dplyr::across(
      -c(Item_Code,item_name,State_code),
      ~.x*30
    )
  )



# this shows we have the same item intake as TATA-NIN

food_item_summary_state %>% 
  dplyr::filter(Item_Code==102 & State_code == "09") %>% 
  ggplot(aes(quantity_g*30)) +
  geom_histogram(color = "black", fill = "black") +
  theme_ipsum_pub()


# not split by state
food_item_summary <- food_items_grouped %>% 
  dplyr::left_join(household_characteristics %>% 
                     dplyr::select(HHID, Combined_multiplier), by = "HHID") %>% 
  srvyr::as_survey_design(id = HHID, weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(Item_Code, item_name, hdds_groups) %>% 
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
  
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4))+
  facet_wrap(facets = vars(item_name)) +
  theme_ridges() +
  labs(title = "Cereals")+
  ylab("") +
  xlab("Quantity (g)")

groups[[3]] %>% 
  
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4))+
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
  
  ggplot(aes(x = quantity_g, y= State_name, fill = State_name)) +
  geom_density_ridges(stat = "binline", show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4))+
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
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4))+
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
unique(micronutrient_distributions$State_code)

x <- food_items_grouped %>% 
  dplyr::filter(HHID == "412001102")

write_csv(micronutrient_distributions, here::here(
  "data_rich/India/data/final/extra_states/base_model.csv"
))

micronutrient_distributions %>% 
  
  ggplot(aes(x = energy_kcal)) + 
  geom_histogram()


#look at the distributions of:
#     # Vit A, B1 2 3 5 6 9 12 Fe Zn Ca kcal
micronutrients <- colnames(micronutrient_distributions)
micronutrients <- micronutrients[-c(1:5,14)]


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
    scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4))+
    # scale_color_manual(values = wes_palette("GrandBudapest1", n = 3))+
    labs(
      title = 
        stringr::str_to_title(
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
      ~median(.)
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
                    median = ~median(.x, na.rm = TRUE),
                    q25 = ~quantile(.x, probs = c(0,0.25,0.5,0.75,1))[[2]] ,
                    q75 = ~quantile(.x, probs = c(0,0.25,0.5,0.75,1))[[4]],
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

means_micronutrient_state %>%
  select(energy_kcal_median, energy_kcal_q25,energy_kcal_q75, vita_mg_median, vita_mg_q25,vita_mg_q75,
         vitb1_mg_median, vitb1_mg_q25, vitb1_mg_q75,vitb2_mg_median, vitb2_mg_q25,vitb2_mg_q75,
         vitb3_mg_median, vitb3_mg_q25, vitb3_mg_q75, vitb5_mg_median, vitb5_mg_q25,vitb5_mg_q75,
         vitb6_mg_median, vitb6_mg_q25,vitb6_mg_q75, folate_ug_median, folate_ug_q75, folate_ug_q25,
         vitaminb12_in_mg_median, vitaminb12_in_mg_q25,vitaminb12_in_mg_q75, iron_mg_median, iron_mg_q25,iron_mg_q75,
         zinc_mg_median, zinc_mg_q25,zinc_mg_q75, calcium_mg_median, calcium_mg_q25,calcium_mg_q75) %>% 
  mutate(across(c(energy_kcal_median,
                  energy_kcal_q25,energy_kcal_q75,
                  vita_mg_median,vita_mg_q25,vita_mg_q75,
                  folate_ug_median,folate_ug_q25,folate_ug_q75,
                  calcium_mg_median,calcium_mg_q25,calcium_mg_q75),
                ~round(.))) %>%
  mutate(
    across(-c(energy_kcal_median,
              energy_kcal_q25,energy_kcal_q75,
              vita_mg_median,vita_mg_q25,vita_mg_q75,
              folate_ug_median,folate_ug_q25,folate_ug_q75,
              calcium_mg_median,calcium_mg_q25,calcium_mg_q75),
           ~round(.,1))
  ) %>% 
  mutate("Energy (kcal)" = paste0(energy_kcal_median," (",energy_kcal_q25,",",energy_kcal_q75, ")"),
         "Vitamin A (RAE mcg)"= paste0(vita_mg_median," (",vita_mg_q25,",",vita_mg_q75,")"),
         "Thiamin (mg)" = paste0(vitb1_mg_median," (",vitb1_mg_q25,",",vitb1_mg_q75,")"),
         "Riboflavin (mg)" = paste0(vitb2_mg_median," (",vitb2_mg_q25, ",",vitb2_mg_q75, ")"),
         "Niacin (mg)" = paste0(vitb3_mg_median," (",vitb3_mg_q25,",",vitb3_mg_q75,")"),
         "Vitamin B5 (mg)" = paste0(vitb5_mg_median," (",vitb5_mg_q25,",",vitb5_mg_q75,")"),
         "Vitamin B6 (mg)" = paste0(vitb6_mg_median," (",vitb6_mg_q25,",", vitb6_mg_q75,")"),
         "Folate (mcg)" = paste0(folate_ug_median," (",folate_ug_q25,",",folate_ug_q75, ")"),
         "Vitamin B12 (mcg)" = paste0(vitaminb12_in_mg_median," (",vitaminb12_in_mg_q25,",",vitaminb12_in_mg_q75, ")"),
         "Iron (mg)" = paste0(iron_mg_median," (",iron_mg_q25,",",iron_mg_q75,  ")"),
         "Zinc (mg)" = paste0(zinc_mg_median," (",zinc_mg_q25,",",zinc_mg_q75,")"),
         "Calcium (mg)" = paste0(calcium_mg_median," (",calcium_mg_q25,",",calcium_mg_q75, ")")
  ) %>% 
  select(-c(energy_kcal_median, energy_kcal_q25,energy_kcal_q75, vita_mg_median, vita_mg_q25,vita_mg_q75,
            vitb1_mg_median, vitb1_mg_q25, vitb1_mg_q75,vitb2_mg_median, vitb2_mg_q25,vitb2_mg_q75,
            vitb3_mg_median, vitb3_mg_q25, vitb3_mg_q75, vitb5_mg_median, vitb5_mg_q25,vitb5_mg_q75,
            vitb6_mg_median, vitb6_mg_q25,vitb6_mg_q75, folate_ug_median, folate_ug_q75, folate_ug_q25,
            vitaminb12_in_mg_median, vitaminb12_in_mg_q25,vitaminb12_in_mg_q75, iron_mg_median, iron_mg_q25,iron_mg_q75,
            zinc_mg_median, zinc_mg_q25,zinc_mg_q75, calcium_mg_median, calcium_mg_q25,calcium_mg_q75)) %>% 
  rotate_df() %>% 
  rename("Bihar" = V1,
         "Chhattisgarh" = V2,
         "Orissa" = V3,
         "Uttar Pradesh" = V4,) %>% 
  tibble::rownames_to_column("Nutrient") %>% 
  gt() %>% 
  cols_align(
    align = "center",
    columns = c("Bihar", "Chhattisgarh","Orissa","Uttar Pradesh" )
  ) %>% 
  tab_header(title = "Household nutrient supply estimates by state",
             subtitle = "Median (Q25,Q75) per Adult Female Equivalent"
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
               label = 
                 stringr::str_to_title(
                   stringr::str_split_i(hdds_groups,
                                        "\\_",
                                        2)))
    )+
    geom_treemap() +
    geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                       grow = FALSE, size = 12)+
    labs(title = 
           stringr::str_to_title(stringr::str_split_i(item,
                                                      "\\_",
                                                      1)),
         
    )+
    scale_fill_brewer(palette = "Set3")+
    # guides(fill=guide_legend())+
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm'))+
    guides(fill = guide_legend(title="Food group",label.position = "bottom"))
  # theme(legend.direction = "horizontal", legend.position = "bottom")+
  # guides(fill = "none")+
  theme_ipsum()
  mn_fg_plots[[item]] <- p1
}

legend_fg <- cowplot::get_legend(mn_fg_plots[[3]])
legend_fg <- cowplot::ggdraw(legend_fg)


energy_fg <- mn_fg_plots[[1]]
vita_fg <- mn_fg_plots[[2]]
folate_fg <- mn_fg_plots[[8]]
vitb12_fg <- mn_fg_plots[[10]]
fe_fg <- mn_fg_plots[[11]]
zn_fg <- mn_fg_plots[[12]]

ggpubr::ggarrange(plotlist = mn_fg_plots, common.legend = TRUE)



#total percentage of consumed food items
food_items_grouped %>% 
  dplyr::group_by(HHID) %>% 
  dplyr::summarise(
    grams_perc = sum(quantity_g[energy_kcal!=0])/sum(quantity_g)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(
    mean_matched_foods = mean(grams_perc,na.rm = T),
    lower = mean(grams_perc,na.rm = T)-1.96*sd(grams_perc,na.rm = T)/sqrt(dplyr::n()),
    upper = mean(grams_perc,na.rm = T)+1.96*sd(grams_perc ,na.rm = T)/sqrt(dplyr::n()),
    sd = sd(grams_perc,na.rm = T)
  )

# rm(list = ls())