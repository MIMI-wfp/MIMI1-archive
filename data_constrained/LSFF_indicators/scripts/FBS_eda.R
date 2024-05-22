################################################################################
######################## Exploratory analyses of FBS ###########################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 04-Apr-2024
# Last edited: 

# Script to explore differences in fortification policies between countries. Is
# it reasonable categorise countries as having high reach vs. low reach policies.
# Use data from food balance sheets

# INSTALL AND LOAD PACKAGES

rq_packages <- c("readr", "tidyverse", "ggplot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA

fbs <- read_csv("data_constrained/LSFF_indicators/data/FAOSTAT_data_en_4-4-2024.csv")

#-------------------------------------------------------------------------------

# CONSUMPTION

# Calculate consumption (grams per capita per day) of each food item: 
fbs$g_day <- (fbs$Value * 1000) / 365

# Select only relevant variables from fbs: 
fbs <- fbs %>% dplyr::select(Area, Item, Year, g_day)

# Select only values for the year 2021: 
fbs <- fbs %>% dplyr::filter(Year == 2021) %>% 
  dplyr::select(-Year)

#-------------------------------------------------------------------------------

# FORTIFICATION POLICIES

# For each country specify which fortification vehicles are fortified:

fbs <- fbs %>% mutate(fortification_policy = dplyr::case_when(
  Area == "Nigeria" & Item %in% c("Wheat and products", "Maize and products") ~ "mandatory",
  Area == "Nigeria" & Item == "Rice and products" ~ "no policy",
  Area == "India" & Item %in% c("Wheat and products", "Rice and products") ~ "voluntary",
  Area == "India" & Item == "Maize and products" ~ "no policy",
  Area == "Ethiopia" & Item == "Wheat and products" ~ "mandatory",
  Area == "Ethiopia" & Item %in% c("Maize and products", "Rice and products") ~ "no policy",
  Area == "Bangladesh" & Item %in% c("Wheat and products", "Rice and products") ~ "voluntary",
  Area == "Bangladesh" & Item == "Maize and products" ~ "no policy",
  Area == "Indonesia" & Item == "Wheat and products" ~ "mandatory",
  Area == "Indonesia" & Item %in% c("Maize and products", "Rice and products") ~ "no policy",
  Area == "Kenya" & Item %in% c("Wheat and products", "Maize and products") ~ "mandatory",
  Area == "Kenya" & Item == "Rice and products" ~ "no policy",
  Area == "Pakistan" & Item %in% c("Wheat and products", "Rice and products", "Maize and products") ~ "no policy",
  Area == "Nepal" & Item == "Wheat and products" ~ "mandatory",
  Area == "Nepal" & Item %in% c("Maize and products", "Rice and products") ~ "no policy",
  Area == "United Republic of Tanzania" & Item %in% c("Wheat and products", "Maize and products") ~ "mandatory",
  Area == "United Republic of Tanzania" & Item == "Rice and products" ~ "no policy",
  Area == "Burkina Faso" & Item == "Wheat and products" ~ "mandatory",
  Area == "Burkina Faso" & Item %in% c("Maize and products", "Rice and products") ~ "no policy",
  Area == "Ghana" & Item == "Wheat and products" ~ "mandatory",
  Area == "Ghana" & Item %in% c("Maize and products", "Rice and products") ~ "no policy",
  Area == "Mozambique" & Item %in% c("Wheat and products", "Maize and products") ~ "mandatory",
  Area == "Mozambique" & Item == "Rice and products" ~ "no policy",
  Area == "Malawi" & Item %in% c("Wheat and products", "Maize and products") ~ "mandatory",
  Area == "Malawi" & Item == "Rice and products" ~ "no policy",
  Area == "Sri Lanka" & Item == "Wheat and products" ~ "mandatory",
  Area == "Sri Lanka" & Item %in% c("Maize and products", "Rice and products") ~ "no policy"
))

# Filter dataframe to remove entries where there is no fortification policy:
fbs <- fbs %>% dplyr::filter(fortification_policy != "no policy")

# Rename Items: 
fbs <- fbs %>% mutate(Item = dplyr::case_when(
  Item == "Wheat and products" ~ "Wheat",
  Item == "Maize and products" ~ "Maize",
  Item == "Rice and products" ~ "Rice"
))

# Create additional entries in the Item column called "Combined" which will
# represent the sum of all three items:
fbs_combined <- fbs %>% 
  group_by(Area) %>% 
  filter(n() > 1) %>% 
  summarise(g_day = sum(g_day), .groups = "drop") %>% 
  mutate(Item = "Combined")

fbs_combined$fortification_policy <- NA

fbs <- rbind(fbs, fbs_combined)

rm(fbs_combined)

# Rename countries so that we use ISO 3 codes instead: 
fbs <- fbs %>% mutate(Area = dplyr::case_when(
  Area == "Nigeria" ~ "NGA",
  Area == "India" ~ "IND",
  Area == "Ethiopia" ~ "ETH",
  Area == "Bangladesh" ~ "BGD",
  Area == "Indonesia" ~ "IDN",
  Area == "Kenya" ~ "KEN",
  Area == "Pakistan" ~ "PAK",
  Area == "Nepal" ~ "NPL",
  Area == "United Republic of Tanzania" ~ "TZA",
  Area == "Burkina Faso" ~ "BFA",
  Area == "Ghana" ~ "GHA",
  Area == "Mozambique" ~ "MOZ",
  Area == "Malawi" ~ "MWI",
  Area == "Sri Lanka" ~ "LKA"
))

#-------------------------------------------------------------------------------

# Create lollipop plot, showing g_day on the x-axis, and items grouped by Area 
# on the y-axis: 
fbs %>% mutate(Area = factor(Area)) %>% 
  ggplot(aes(x = g_day, y = Item, group = Area , color = fortification_policy)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_segment(aes(xend = 0, yend = Item), size = 1, color = "gray") +
  facet_wrap(~Area, scales = "free_y", strip.position = "left", ncol = 1) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "",
       x = "Average consumption (g/capita/day)",
       y = "",
       color = "Fortification policy") +
  scale_color_manual(values = c("mandatory" = "red", "voluntary" = "blue")) +
  # Add a vertical line at 75g/day:
  geom_vline(xintercept = 75, linetype = "dashed", color = "black")

# ggsave("data_constrained/LSFF_indicators/figures/fortification_policy.png", width = 14, height = 7.5, dpi = 600)

#-------------------------------------------------------------------------------