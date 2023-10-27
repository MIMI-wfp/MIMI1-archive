# Matching and harmonising NSSO food items
# MIMI project
# Author: Gabriel Battcock
# Date: 25/10/2023


# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("TomCodd/NutritionTools"

# We also need to import some custom functions in another script:
# source(here::here("functions.R")) # Loading nutrition functions

# read in the NSSO food data
path_to_file <- "./India_analysis/data/raw/"
block_5_6_food_consumption <- read_csv(paste0(path_to_file, "block_5_6_food_consumption.csv"))

#read in xlsx spreadsheet previously matched by hand
food_item_names <- readxl::read_xlsx(paste0(path_to_file, "food_codes_nsso_to_ifct.xlsx"), sheet = 1) %>% 
  dplyr::rename(Item_Code = `unique(Item_Code)`) %>% 
  dplyr::select(Item_Code,item_name, IFCT_code, ifct_name)

IN17 <- readxl::read_xlsx("~/Documents/MIMI/India/FCT/ifct_noduplicates_sentencecase_20231110.xlsx")
head(IN17)

# Data manipulation ------------------------------------------------------------

# select only the HHID, item codes and quantities
block_5_6_food_consumption <- block_5_6_food_consumption %>% 
  dplyr::select(HHID, 
                Item_Code,
                Total_Consumption_Quantity, 
                Total_Consumption_Value) 
head(block_5_6_food_consumption)

# find any food items in odd units

food_item_names <- food_item_names %>% 
  dplyr::mutate(
    food_unit = stringr::str_split_i(
      stringr::str_split_i(item_name,"\\(", -1), "\\)",1)
    ) %>% 
  dplyr::mutate(
    conversion_factor = ifelse(stringr::str_detect(food_unit,"Kg|kg") == TRUE, 1,
                         ifelse(stringr::str_detect(food_unit,"\\gm")== TRUE, 0.001, 
                                NA))
                                # ifelse(stringr::str_detect(item_name))))
    ) %>% 
  dplyr::filter(
    Item_Code < 300 #filter out tobacco, alcohol and fuels
      # stringr::str_detect(item_name, "s.t") == FALSE
    ) %>% dplyr::filter( stringr::str_detect(item_name, "s.t") == FALSE)


#convert the non-standard units

non_standard_units <- food_item_names %>% 
  dplyr::filter(is.na(conversion_factor)) %>% 
  dplyr::mutate(
    conversion_factor = 
      dplyr::case_when(
        #by inspection w/o units listed
        Item_Code == 170 ~ 1, #salt, appears to be alerady in kg
        Item_Code == 171 ~ 1, #sugar same as salt
        Item_Code == 172 ~ 1, #sugar same as salt
        Item_Code == 173 ~ 1, #gur
        Item_Code == 174 ~ 1, #candy
        Item_Code == 175 ~ 1, #Honey
        #items with units of no. 
        Item_Code == 190 ~ 0.053, #eggs, USDA match
        Item_Code == 216 ~ 0.054, #lemon, USDA match
        Item_Code == 220 ~ 0.115, #Banana, USDA
        Item_Code == 228 ~ 0.14, #Orange, USDA
        Item_Code == 223 ~ 0.905, #Pineapple, USDA
        Item_Code == 160 ~ 1.05 #cow's milk, USDA
        
      )
  )



block_5_6_food_consumption %>% dplyr::select(HHID,Item_Code, Total_Consumption_Quantity) %>% 
  dplyr::filter(Item_Code == 175)
  
block_4_demog %>% dplyr::select(HHID,Person_sr_no) %>% 
  dplyr::filter(HHID == 741431101) 


# head(non_standard_units)
  #   


# 'Other' items

unmatched_items <- food_item_names %>% 
  dplyr::filter(
      is.na(IFCT_code)
  ) %>% 
  dplyr::mutate(
    IFCT_code =
      dplyr::case_when(
        Item_Code == 192 ~ "O001
                            O002
                            O003
                            O004
                            O005
                            O006
                            O007
                            O008
                            O009
                            O010
                            O011
                            O012
                            O013
                            O014
                            O015
                            O016
                            O017
                            O018
                            O019
                            O020
                            O021
                            O022
                            O023
                            O024",#vector of all goat and mutton to be averaged
        Item_Code == 193 ~ "O025
                            O026
                            O027
                            O028
                            O029
                            O030
                            O031
                            O032
                            O033
                            O034
                            O035
                            O036
                            O037
                            O038
                            O039
                            O040
                            O041
                            O042
                            O043
                            O044",#beef items
        Item_Code == 194 ~ "O045
                            O046
                            O047
                            O048
                            O049
                            O050
                            O051
                            O052
                            O053
                            O054",#pork items
        Item_Code == 195 ~ "N001
                            N002
                            N003
                            N004
                            N005
                            N006
                            N007
                            N008
                            N009
                            N010",#chicken
        Item_Code == 191 ~ "P001
                            P002
                            P003
                            P004
                            P005
                            P006
                            P007
                            P009
                            P010
                            P011
                            P012
                            P013
                            P014
                            P015
                            P016
                            P017
                            P019
                            P020
                            P021
                            P022
                            P023
                            P024
                            P025
                            P026
                            P027
                            P028
                            P029
                            P030
                            P031
                            P032
                            P033
                            P034
                            P035
                            P036
                            P037
                            P038
                            P039
                            P040
                            P041
                            P042
                            P043
                            P044
                            P045
                            P046
                            P047
                            P048
                            P049
                            P050
                            P051
                            P052
                            P053
                            P054
                            P055
                            P057
                            P058
                            P059
                            P060
                            P061
                            P062
                            P063
                            P064
                            P065
                            P067
                            P068
                            P069
                            P070
                            P071
                            P072
                            P075
                            P076
                            P077
                            P078
                            P080
                            P081
                            P082
                            P083
                            P084
                            P085
                            P086
                            P087
                            P088
                            P089
                            P090
                            P091
                            P092
                            P093
                            P094
                            P095
                            P096
                            P097
                            P098
                            P099
                            P100
                            P101
                            P102
                            P103
                            P104
                            P105
                            P106
                            P107
                            P108
                            P109
                            P110
                            P112
                            P113
                            P114
                            P115
                            P116
                            P117
                            P118
                            P119
                            P120
                            P121
                            P122
                            P123
                            P124
                            P125
                            P126
                            P127
                            P128
                            P129
                            P130
                            P131
                            P132
                            P133
                            P134
                            P135
                            P136
                            P137
                            P138
                            P140
                            P141
                            P142
                            P143
                            P144
                            P145
                            P146
                            P147
                            P148
                            P149
                            P150
                            P151
                            P152
                            P154
                            P155
                            P156
                            P157
                            P158
                            P159
                            P160
                            P161
                            P162
                            Q005
                            Q006
                            Q007
                            R002
                            R003
                            R004
                            R005
                            R006
                            R007",#fish and prawn
        Item_Code == 196 ~ "N011
                            N012
                            N013
                            N014
                            N015
                            N016
                            N017
                            N018
                            N019
                            N021
                            N022
                            N023
                            N024
                            N025
                            N026
                            N027
                            N028
                            N029
                            N030
                            N031"#other meats
        # Item_Code == 1
        
        
      )
  ) %>% 
  tidyr::separate_rows(
    IFCT_code
  )%>% 
  #join the fct
  dplyr::left_join(
    IN17, by = c("IFCT_code" = "food_code")
  ) %>% 
# %>% 
  #group to then take the average per NSSO food item
  # note this can be supstituted by a better method
  dplyr::group_by(
    Item_Code, item_name
  ) %>% 
  dplyr::select(
    -c(IFCT_code,ifct_name,food_unit,conversion_factor,food_name)
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      everything(),
      ~mean(.x, na.rm = TRUE)
      
  )
)  
  

