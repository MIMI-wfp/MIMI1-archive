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

IN17 <- readxl::read_xlsx("~/Documents/MIMI/India/FCT/ifct_gabriel_20231030.xlsx")
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





# NNMB Weights -----------------------------------------------------------------
# using 2012 NSSO individual level data to get an idea of proportion of items consumed
# Bihar and Chat not included so will isolate just to UP

nnmb_food_consumption <- read_csv("~/Documents/LSHTM/WFP_project/data/IND_00062/consumption_user.csv")
nnmb_subject <- read_csv("~/Documents/LSHTM/WFP_project/data/IND_00062/subject_user.csv")
gdqs <- read_csv("~/Documents/LSHTM/WFP_project/data/IND_00062/food_groups/GDQS_library.csv")

head(nnmb_food_consumption)
names(nnmb_food_consumption)
names(nnmb_subject)


# Calculate and match the break down of ASF products
nnmb_animal_sourced_consumption <- 
  nnmb_food_consumption %>% 
  dplyr::select(
    SUBJECT,FOODEX2_INGR_CODE, FOODEX2_INGR_DESCR, FOOD_AMOUNT_REPORTED
  ) %>% 
  dplyr::inner_join(nnmb_subject %>% 
                      dplyr::select(SUBJECT,
                                    ADM1_NAME,
                                    SEX),
                    by = "SUBJECT") %>% 
    dplyr::filter(ADM1_NAME == "Uttar Pradesh") %>%
    dplyr::group_by(FOODEX2_INGR_CODE, FOODEX2_INGR_DESCR) %>% 
    dplyr::summarise(total_item_consumed_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
    dplyr::left_join(
      gdqs, by = "FOODEX2_INGR_CODE"
    ) %>%
    dplyr::filter(
      g18_red_meat == 1 |
      g13_fish == 1 |
      g14_poultry_game == 1 |
      g20_processed_meat == 1
    ) %>% 
  dplyr::mutate(
    ifct19_code = 
      dplyr::case_when(
        FOODEX2_INGR_CODE == "A01QZ" ~ "O025",#beef muscle
        FOODEX2_INGR_CODE == "A01RG" ~ "O048",#pork muscle
        FOODEX2_INGR_CODE == "A01RJ" ~ "O015",#mutton
        FOODEX2_INGR_CODE == "A01RL#F10.A077C" ~ "O001",#goat lean
        FOODEX2_INGR_CODE == "A01SP" ~ "N002",#chicken meat
        FOODEX2_INGR_CODE == "A026V" ~ "P161",# tunny
        FOODEX2_INGR_CODE == "A026Y#F01.A07Y6" ~ "P146",#sarputi
        FOODEX2_INGR_CODE == "A028F" ~ "P101",#bhekti
        FOODEX2_INGR_CODE == "A02AD" ~ "P039",#mullet
        FOODEX2_INGR_CODE == "A02FH" ~ "Q007",#prawn
        FOODEX2_INGR_CODE == "A02LN" ~ "N026",#snail
        FOODEX2_INGR_CODE == "A0C76#F01.A0875" ~ "P142",#ravas
        FOODEX2_INGR_CODE == "A0F1C#F01.A0JXY" ~ "P010",#chela
        FOODEX2_INGR_CODE == "A0F8D" ~ "S006",#rohu
        FOODEX2_INGR_CODE == "A0F8Q#F01.A081B" ~ "P130",#magur
        FOODEX2_INGR_CODE == "A0FDD" ~ "P107"#chingai
        )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(
    ifct19_code, 
    total_item_consumed_g,
    INGREDIENT_ENG
  )
 
nnmb_fruit_veg_consumption <- 
  nnmb_food_consumption %>% 
  dplyr::select(
    SUBJECT,FOODEX2_INGR_CODE, FOODEX2_INGR_DESCR, FOOD_AMOUNT_REPORTED
  ) %>% 
  dplyr::inner_join(nnmb_subject %>% 
                      dplyr::select(SUBJECT,
                                    ADM1_NAME,
                                    SEX),
                    by = "SUBJECT") %>% 
  dplyr::filter(ADM1_NAME == "Uttar Pradesh") %>%
  dplyr::group_by(FOODEX2_INGR_CODE, FOODEX2_INGR_DESCR) %>% 
  dplyr::summarise(total_item_consumed_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
  dplyr::left_join(
    gdqs, by = "FOODEX2_INGR_CODE"
  ) %>%
  dplyr::filter(
    g1_citrus == 1 |
      g2_deep_orange_fruit == 1 |
      g3_other_fruits == 1 |
      g4_dark_leafy_green == 1|
      g5_cruiciferous_veg == 1|
      g6_deep_orange_veg == 1|
      g7_other_veg == 1)
  ) %>%
  dplyr::mutate(
    ifct19_code =
      dplyr::case_when(
        FOODEX2_INGR_CODE =="A00FJ" ~,
        FOODEX2_INGR_CODE == "A00FR" ~ ,
        FOODEX2_INGR_CODE == "A00GC"~,
        FOODEX2_INGR_CODE == "A00GH" ~ ,
        FOODEX2_INGR_CODE == "A00GZ" ~ ,
        FOODEX2_INGR_CODE == "A00HC" ~ , 
        FOODEX2_INGR_CODE == "A00HH" ~ ,
        FOODEX2_INGR_CODE == "A00JB#F10.A0F2Q" ~ ,
        FOODEX2_INGR_CODE == "A00JD" ~ ,
        FOODEX2_INGR_CODE == "A00JF" ~ ,          
        FOODEX2_INGR_CODE == "A00JM" ~ , 
        FOODEX2_INGR_CODE == "A00JY" ~, 
        FOODEX2_INGR_CODE == "A00KB" ~ , 
        FOODEX2_INGR_CODE == "A00KF" ~ , 
        FOODEX2_INGR_CODE == "A00KH" ~ , 
        FOODEX2_INGR_CODE == "A00KL" ~ , 
        FOODEX2_INGR_CODE == "A00KM" ~ ,
        FOODEX2_INGR_CODE == "A00KN" ~ ,
        FOODEX2_INGR_CODE == "A00KR" ~ , 
        FOODEX2_INGR_CODE == "A00KR#F01.A05AY" ~, 
        FOODEX2_INGR_CODE == "A00KR#F01.A05HR" ~ , 
        FOODEX2_INGR_CODE == "A00KR#F01.A0EDH" ~ , 
        FOODEX2_INGR_CODE == "A00KR#F26.A07XE" ~ , 
        FOODEX2_INGR_CODE == "A00KX" ~ , 
        FOODEX2_INGR_CODE == "A00LV" ~ , 
        FOODEX2_INGR_CODE == "A00MB" ~ , 
        FOODEX2_INGR_CODE == "A00MJ" ~ , 
        FOODEX2_INGR_CODE == "A00NV#F10.A0F2Q" ~, 
        FOODEX2_INGR_CODE == "A00QH" ~ ,
        FOODEX2_INGR_CODE == "A00XF" ~ , 
        FOODEX2_INGR_CODE == "A00XG" ~ , 
        FOODEX2_INGR_CODE == "A00XH" ~ , 
        FOODEX2_INGR_CODE == "A00XZ" ~ , 
        FOODEX2_INGR_CODE == "A00ZQ#F27.A00JB" ~, 
        FOODEX2_INGR_CODE == "A012J" ~ , 
        FOODEX2_INGR_CODE == "A012J#F28.A07GY" ~ , 
        FOODEX2_INGR_CODE == "A01BY" ~ , 
        FOODEX2_INGR_CODE == "A01CR" ~ ,          
        FOODEX2_INGR_CODE == "A01DJ" ~ , 
        FOODEX2_INGR_CODE == "A01DX#F10.A0F2Q" ~ , 
        FOODEX2_INGR_CODE == "A01EP" ~ , 
        FOODEX2_INGR_CODE == "A01HF#F10.A166Y" ~ , 
        FOODEX2_INGR_CODE == "A01JJ" ~ , 
        FOODEX2_INGR_CODE == "A01KB" ~ , 
        FOODEX2_INGR_CODE == "A01LC#F10.A07XL" ~, 
        FOODEX2_INGR_CODE == "A01LF#F10.A07XL" ~, 
        FOODEX2_INGR_CODE == "A01LF#F10.A0F2Q"~ , 
        FOODEX2_INGR_CODE == "A01LH" ~ , 
        FOODEX2_INGR_CODE == "A01LR" ~ , 
        FOODEX2_INGR_CODE == "A01ME" ~ , 
        FOODEX2_INGR_CODE == "A01MF" ~, 
        FOODEX2_INGR_CODE == "A01MK#F03.A06JD" , 
        FOODEX2_INGR_CODE == "A01QE#F01.A05XA" ~, 
        FOODEX2_INGR_CODE == "A05FY" ~ , 
        FOODEX2_INGR_CODE == "A0CGD" ~, 
        FOODEX2_INGR_CODE == "A0CGZ#F26.A07XE" ~, 
        FOODEX2_INGR_CODE == "A0DEM" ~ , 
        FOODEX2_INGR_CODE == "A0DFE" ~ , 
        FOODEX2_INGR_CODE == "A0DJT" ~ , 
        FOODEX2_INGR_CODE == "A0DLX" ~, 
        FOODEX2_INGR_CODE == "A0DLY" ~ , 
        FOODEX2_INGR_CODE == "A0DMK" ~, 
        FOODEX2_INGR_CODE == "A0DMX#F10.A07XL" ~ , 
        FOODEX2_INGR_CODE == "A0DMX#F10.A0F2Q" ~ , 
        FOODEX2_INGR_CODE == "A0DYS"
      )
  ) 
  # dplyr::ungroup() %>% 
  # dplyr::select(
  #   ifct19_code, 
  #   total_item_consumed_g,
  #   INGREDIENT_ENG
  # )




# Matching composite items using NNMB ------------------------------------------ 

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
                            O044
                            N021",#beef items
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
                            N022
                            N023
                            N024
                            N025
                            N026
                            N027
                            N028
                            N029
                            N030
                            N031",#other meats
        Item_Code == 217 ~ "D001
                            D002
                            D003
                            D004
                            D007
                            D031
                            D032
                            D033
                            D034
                            D035
                            D036
                            D037
                            D038
                            D039
                            D040
                            D041
                            D042
                            D043
                            D044
                            D045
                            D046
                            D047
                            D049
                            D051
                            D052
                            D053
                            D054
                            D056
                            D057
                            D058
                            D059
                            D060
                            D062
                            D063
                            D064
                            D065
                            D068
                            D070
                            D073
                            D074
                            D076
                            D077
                            D078
                            C001
                            C002
                            C003
                            C004
                            C005
                            C006
                            C007
                            C008
                            C009
                            C010
                            C012
                            C013
                            C014
                            C015
                            C016
                            C017
                            C018
                            C019
                            C020
                            C021
                            C022
                            C023
                            C024
                            C025
                            C026
                            C027
                            C028
                            C029
                            C030
                            C031
                            C032
                            C033
                            C034",#other veg
        Item_Code == 148 ~ "B001
                            B003
                            B005
                            B006
                            B007
                            B008
                            B009
                            B010
                            B012
                            B013
                            B016
                            B018
                            B021
                            B023
                            B024
                            B025
                            B027
                            B028
                            B030
                            B031
                            B032",#other pulses
        Item_Code == 106 ~ "A010
                            A011
                            A012
                            A013
                            A014
                            A015
                            A027
                            A028
                            A029",#other rice
        Item_Code == 113 ~ "A033
                            A034",#bread
        Item_Code == 122 ~ "A035
                            A036
                            A037
                            A038
                            A039
                            A040
                            A041
                            A042
                            A043
                            A044
                            A045
                            A046
                            A047
                            A001
                            A003
                            A004
                            A005
                            A006
                            A007
                            A009", #other_cereals
        Item_Code == 167 ~ "L001
                            L002
                            L003
                            L004
                            L005
                            L006
                            L007
                            L008
                            L009
                            L010
                            L011
                            L012
                            L013
                            L014
                            L017
                            L018",# other milk
        
        Item_Code == 234 ~ "E013
                            E021
                            E063", 
        Item_Code == 238 ~ "E001
                            E006
                            E007
                            E008
                            E009
                            E014
                            E015
                            E016
                            E019
                            E020
                            E021
                            E022
                            E024
                            E025
                            E027
                            E028
                            E029
                            E030
                            E031
                            E032
                            E033
                            E035
                            E036
                            E043
                            E044
                            E045
                            E046
                            E048
                            E049
                            E050
                            E051
                            E052
                            E053
                            E054
                            E055
                            E056
                            E059
                            E060
                            E061
                            E062
                            E063
                            E065
                            E066
                            E067
                            E068",#other fresh fruit
        Item_Code == 247 ~ "E005
                            E017
                            E057
                            E058",#dried fruit
        Item_Code == 245 ~ "H001
                            H004
                            H005
                            H006
                            H012
                            H018
                            H021",#other nuts
        Item_Code == 260 ~ "H008
                            H009
                            H013
                            H014
                            H015
                            H017
                            H019
                            H020"#oil seeds
        
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
  
  dplyr::left_join(nnmb_animal_sourced_consumption, by = c("IFCT_code" = "ifct19_code")) %>% 
  dplyr::select(
    -c(IFCT_code,ifct_name,food_unit,conversion_factor,food_name,INGREDIENT_ENG)
  ) %>%
  dplyr::mutate(
    dplyr::across(
      everything(),
      ~tidyr::replace_na(.x,0)
    ))
  # )  %>% 
  # 
  # 
  # 
  # 
  # dplyr::summarise(
  #   dplyr::across(
  #     everything(),
  #     ~weighted.mean(x = .x, w = total_item_consumed_g, na.rm = TRUE)
  #     
  #   )
  # )
