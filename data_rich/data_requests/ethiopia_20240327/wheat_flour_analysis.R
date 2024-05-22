################################################################################
##################### RAM DATA REQUEST - MIMI INDEX NGA ########################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 27-Mar-2024
# Last edited: 

# This script has been created to analyse reach of wheat flour in Somali and Afar
# regions - disaggregated by urban/rural and source of wheat flour.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "readxl", "srvyr", "wesanderson",
                 "hrbrthemes", "ggridges")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN REQUIRED FUNCTIONS

# Source script required to create base models: 
source("data_rich/all_base_models/scripts/base_model_functions.R")

# Source script required to get quantities of each fortification vehicle: 
source("data_rich/fortification_models/scripts/fortification_model_functions.R")

#-------------------------------------------------------------------------------

# READ IN REQUIRED DATA

# Get base case apparent intake data from HICES:
base_ai <- apparent_intake("eth_hices1516")

# Note that there are complete duplicates in base_ai, remove these entries: 
base_ai <- base_ai[!duplicated(base_ai), ]

# Remove objects that are not required further: 
rm(list = setdiff(ls(), c("base_ai", "food_consumption", "hh_info")))

# Select required columns from food_consumption: 
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Note that there are many food items that are duplicated for households,
# combine these rows and sum the quantities:
food_consumption <- food_consumption %>%
  group_by(hhid, item_code) %>%
  summarise(quantity_100g = sum(quantity_100g))

# Read in food and bevarage data-frame to get purchase info: 
food_purchases <- read_csv("MIMI_data/Ethiopia/eth_hces1516_foodbev/ETH_HCES1516_foodbev.csv")
# Note than consumption quantities in this data-frame are ANNUAL consumption.

# Select relevant variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "ITEMC", "TYPE", "MEASURE", "QUANTITY") %>% 
  # And give variables appropriate names:
  rename("item_code" = "ITEMC",
         "type_expenditure" = "TYPE",
         "unit" = "MEASURE", # Note that all units are in grams or cubic cm (ML)
         "quantity" = "QUANTITY")

# Get daily quantities instead of annual: 
food_purchases$quantity_100g <- food_purchases$quantity / (365*100)
food_purchases$quantity <- NULL

# Create new quantity variables, to show quantity of food that was obtained
# "In cash", and quantity obtained "In kind":
food_purchases$purchased_100g <- ifelse(food_purchases$type_expenditure == "In Cash", 
                                        food_purchases$quantity_100g, NA)

food_purchases$in_kind_100g <- ifelse(food_purchases$type_expenditure == "In Kind",
                                      food_purchases$quantity_100g, NA)

# Since there are duplicates, combine rows that have the same "hhid" and "item_code", 
# sum the quantities:
food_purchases <- food_purchases %>% 
  dplyr::select(hhid, item_code, purchased_100g, in_kind_100g) %>%
  group_by(hhid, item_code) %>%
  summarise(purchased_100g = sum(purchased_100g, na.rm = T),
            in_kind_100g = sum(in_kind_100g, na.rm = T))

# Left join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Remove objects that are not required further:
rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS

hices_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx", 
                                sheet = "Ethiopia (HICES) food items")

hices_proportions <- hices_proportions %>% 
  dplyr::select("item_cd", "prop_fortifiable")

# Spacing in item_codes is causing issues when performing joing, remove spaces:
food_consumption$item_code <- gsub(" ", "", food_consumption$item_code)
hices_proportions$item_cd <- gsub(" ", "", hices_proportions$item_cd)

# Left join proportions to food consumption data: 
food_consumption <- left_join(food_consumption, hices_proportions, by = c("item_code" = "item_cd"))

rm(hices_proportions)

# Code NA values == 1: 
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Multiply all quantities by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable
food_consumption$in_kind_100g <- food_consumption$in_kind_100g * food_consumption$prop_fortifiable

# Remove prop_fortifiable: 
food_consumption$prop_fortifiable <- NULL

#-------------------------------------------------------------------------------

# WHEAT FLOUR

# Add a column called food_item to specifiy which food items contain wheat flour: 

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == "Wheatwhite,flour" ~ "Wheat flour",
  item_code == "Wheatmixed,flour" ~ "Wheat flour",
  item_code == "Wheatblack,flour" ~ "Wheat flour",
  item_code == "Wheat&Barley(Duragna),flour" ~ "Wheat flour",
  item_code == "Wheat&othercereals,flour" ~ "Wheat flour",
  item_code == "Flour,factoryproduct,mainlyofwheat" ~ "Wheat flour",
  item_code == "Bread(Dufo,Anbashaetc),Wheat-homemade" ~ "Wheat flour",
  item_code == "Bread,wheat-bakery" ~ "Wheat flour",
  item_code == "Donat/bombolino" ~ "Wheat flour",
  item_code == "Boresh(Dolchi)" ~ "Wheat flour",
  item_code == "Pizzas" ~ "Wheat flour",
  item_code == "Cakes" ~ "Wheat flour",
  item_code == "Biscuits" ~ "Wheat flour",
  item_code == "Baqlaba/Mushebek" ~ "Wheat flour",
  item_code == "Sandwitch,meat/egg/vegetable,normal" ~ "Wheat flour",
  item_code == "Burger/clubsandwich" ~ "Wheat flour",
  item_code == "Breadoranypastryproductswithhotdrinks" ~ "Wheat flour",
  item_code == "BreadoranypastryproductsandJuice" ~ "Wheat flour",
  item_code == "Othersn.e.c." ~ "Wheat flour"
))

# Filter to select only food items that contain wheat flour:
wf_consumption <- food_consumption %>% 
  dplyr::select(hhid, food_item, quantity_100g, purchased_100g, in_kind_100g) %>% 
  filter(!is.na(food_item))

rm(food_consumption)

# Sum quantities of wheat flour consumed by each household from all food sources: 
wf_consumption <- wf_consumption %>% 
  group_by(hhid) %>% 
  summarise(quantity_100g = sum(quantity_100g, na.rm = T),
            purchased_100g = sum(purchased_100g, na.rm = T),
            in_kind_100g = sum(in_kind_100g, na.rm = T))

# Add binary columns to specify if households consumed wheat flour, from "In cash", 
# "In kind", or both:
wf_consumption$in_cash <- ifelse(wf_consumption$purchased_100g > 0 & wf_consumption$in_kind_100g == 0, 1, 0)
wf_consumption$in_kind <- ifelse(wf_consumption$in_kind_100g > 0 & wf_consumption$purchased_100g == 0, 1, 0)
wf_consumption$both_sources <- ifelse(wf_consumption$purchased_100g > 0 & wf_consumption$in_kind_100g > 0, 1, 0)

#-------------------------------------------------------------------------------

# HOUSEHOLD INFORMATION

# Now select only households from Somali and Afar regions from hh_info: 
hh_info <- hh_info %>% 
  dplyr::select(hhid, adm1, res, survey_wgt, afe) %>% 
  filter(adm1 %in% c("Afar", "Somali"))

# Left join wheat flour consumption data to relevant households: 
wf_consumption <- left_join(hh_info, wf_consumption, by = "hhid")

# Divide all quantities by afe using a dplyr function: 
wf_consumption <- wf_consumption %>% 
  mutate(quantity_100g = quantity_100g / afe,
         purchased_100g = purchased_100g / afe,
         in_kind_100g = in_kind_100g / afe) 

wf_consumption$afe <- NULL

# Replace  NA's with 0's: 
wf_consumption[is.na(wf_consumption)] <- 0

rm(list = c("base_ai", "hh_info"))

# write_csv(wf_consumption, "data_rich/data_requests/ethiopia_20240327/wf_consumption.csv")

#-------------------------------------------------------------------------------

# CREATE tbl_svy OBJECT READY FOR ANALYSIS 

wf_consumption_svy <- wf_consumption %>% 
  srvyr::as_survey_design(weights = survey_wgt)

# Summarise reach of wheat flour, according to source, disaggregated by adm1 and res: 
wf_reach <- wf_consumption_svy %>% 
  srvyr::group_by(adm1, res) %>% 
  srvyr::summarise(in_cash = srvyr::survey_mean(in_cash, vartype = NULL),
                    in_kind = srvyr::survey_mean(in_kind , vartype = NULL),
                    both_sources = srvyr::survey_mean(both_sources, vartype = NULL)) %>% 
  dplyr::mutate(across(in_cash:both_sources, ~ .x * 100))

# Round all percentages to 1 decimal place: 
wf_reach <- wf_reach %>% 
  dplyr::mutate(across(in_cash:both_sources, ~ round(.x, 1)))

# Re-order variables:
wf_reach <- wf_reach %>% 
  dplyr::select(adm1, res, in_cash, both_sources, in_kind)

# Write csv for raw percentages: 
# write_csv(wf_reach, "data_rich/data_requests/ethiopia_20240327/reach_percentages.csv")

#-------------------------------------------------------------------------------

# STACKED BARPLOT

# Create a stacked_barplot df that ensures reach is cumulative, (i.e. the height
# of each bar represents reach regardless of source): 
stacked_barplot <-  wf_reach %>% 
  mutate(both_sources = in_cash + both_sources, 
         in_kind = both_sources + in_kind)

# Bar colours: 
bar_colours <- wes_palette("AsteroidCity1", 3)

# Now use this instead to create the stacked barplot: 
stacked_barplot %>% 
  ggplot(aes(x = res, y = in_kind, fill = "In kind")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = both_sources, fill = "Both sources"), stat = "identity") +
  geom_bar(aes(y = in_cash, fill = "In cash"), stat = "identity") +
  labs(x = "",
       y = "Reach of wheat flour (% of households)") +
  facet_wrap(~adm1, scales = "free_y") + 
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() + 
  guides(fill = guide_legend(title = "")) +
  scale_fill_manual(values = c("In kind" = bar_colours[1], 
                               "Both sources" = bar_colours[2], 
                               "In cash" = bar_colours[3]),
                    breaks = c("In kind", "Both sources", "In cash"))

# ggsave("data_rich/data_requests/ethiopia_20240327/stacked_barplot.png", width = 6, height = 5, dpi = 600)

# Remove objects no longer required: 
rm(list = c("bar_colours", "stacked_barplot", "wf_reach"))

#-------------------------------------------------------------------------------

# RIDGEPLOT

# Convert quantities to grams: 
wf_consumption <- wf_consumption %>% 
  mutate(quantity_g = quantity_100g * 100,
         purchased_g = purchased_100g * 100,
         in_kind_g = in_kind_100g * 100)

# Select relevant variables only and convert to long format:
long_data <- wf_consumption %>% 
  dplyr::select(hhid, adm1, res, purchased_g, in_kind_g) %>% 
  pivot_longer(cols = c(purchased_g, in_kind_g), 
               names_to = "source", 
               values_to = "quantity_g")

# Code 0 values as NA (to avoid skewing distributions), then remove NA values:
long_data$quantity_g[long_data$quantity_g == 0] <- NA

long_data <- long_data %>% 
  drop_na(quantity_g)

# Rename purchased_g to "Purchased", and in_kind_g to "In kind": 
long_data$source <- ifelse(long_data$source == "purchased_g", "Purchased",
                           ifelse(long_data$source == "in_kind_g", "In kind", NA))

# Specify ridge colours: 
ridge_colours <- wes_palette("AsteroidCity1", 2)
ridge_colours <- c(ridge_colours[2], ridge_colours[1])

# Create the ridgeline plot, facet-wrap by adm1, fill by res:
long_data %>% 
  ggplot(aes(x = quantity_g, y = source, fill = res)) +
  geom_density_ridges(scale = 0.8, alpha = 0.7) +
  theme_ridges() +
  labs(x = "Quantity of wheat flour consumed (g) per day/AFE",
       y = "Source of wheat flour") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) +
  scale_fill_manual(values = ridge_colours) +
  facet_wrap(~adm1, scales = "free_y") +
  xlim(0, 1000) +
  guides(fill = guide_legend(title = ""))

# ggsave("data_rich/data_requests/ethiopia_20240327/quantity_ridges.png", width = 12, height = 6, dpi = 600)

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################



