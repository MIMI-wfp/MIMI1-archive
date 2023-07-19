### creating micronutrients as classes

source("functions.R")

setwd("~/Documents/LSHTM/WFP_project/MIMI")#set path for myself, please change if you want to use it
path_to_data <- "../IND_00062/"

# read in all the data files 
consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))
food_dict <- read_excel(paste0(path_to_data, "food_groups/FoodEx2_Exposure_dict.xlsx"))
code_list <- read_csv(paste0(path_to_data, "code_lists.csv"))
DQQ <- read_csv(paste0(path_to_data, "food_groups/DQQ_library.csv"))
GDQS <- read_csv(paste0(path_to_data, "food_groups/GDQS_library.csv"))
MDD <- read_csv(paste0(path_to_data, "food_groups/MDD_library.csv"))

# make a joined variable 
joined <- full_join(user, consumption, by = c("SUBJECT","ROUND")) 

# create data frame for each microntrient (total/observed intake per person)
vit_a_population <- MICRONUT_SUM(joined, VITA_RAE_mcg)
folate_population <- MICRONUT_SUM(joined, FOLDFE_mcg)
iron_population <- MICRONUT_SUM(joined, IRON_mg)
zinc_population <- MICRONUT_SUM(joined, ZINC_mg)

###### Constant variables to be used ###############

# Using EAR from the NIN to calculate minimum requirements 

vita_EAR_men_mcg <- 460
vita_EAR_women_mcg <- 390
folate_EAR_men_mcg  <-  250
folate_EAR_women_mcg <- 180
iron_EAR_men_mg <- 11
iron_EAR_women_mg <- 15
zinc_EAR_men_mg <- 14
zinc_EAR_women_mg <- 11

# upper limit taken from Chan. Public Health  <- https://www.hsph.harvard.edu/nutritionsource
iron_UL_mg <- 45
zinc_UL_mg <- 40
folate_UL_mcg <-  1000
vita_UL_mcg <- 3000

#set the micronutrient objects
VitA <- new("micronutrient", 
    name = "vitamin_a",
    EAR_men = 460,
    EAR_women = 390,
    UL = 3000, 
    data = vit_a_population,
    value = "sum_VITA_RAE_mcg"
)

Folate  <- new("micronutrient", 
    name = "folate",
    EAR_men = folate_EAR_men_mcg,
    EAR_women = folate_EAR_women_mcg,
    UL = folate_UL_mcg, 
    data = folate_population,
    value = "sum_FOLDFE_mcg"
)

Iron <- new("micronutrient", 
    name = "iron",
    EAR_men = iron_EAR_men_mg,
    EAR_women = iron_EAR_women_mg,
    UL = iron_UL_mg, 
    data = iron_population,
    value = "sum_IRON_mg"
)

Zinc <- new("micronutrient", 
    name = "zinc",
    EAR_men = zinc_EAR_men_mg,
    EAR_women = zinc_EAR_women_mg,
    UL = zinc_UL_mg, 
    data = zinc_population,
    value = "sum_ZINC_mg"
)
