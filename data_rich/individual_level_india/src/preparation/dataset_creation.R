#creating usable datasets for ML training 
# G-Battcock

##################### load in previous data sets #############################

load("datasets/simple_macro_output/fo_men.RData")
load("datasets/simple_macro_output/ir_men.RData")
load("datasets/simple_macro_output/va_men.RData")
load("datasets/simple_macro_output/zn_men.RData")
load("datasets/simple_macro_output/vb12_men.RData")
load("datasets/simple_macro_output/fo_women.RData")
load("datasets/simple_macro_output/ir_women.RData")
load("datasets/simple_macro_output/va_women.RData")
load("datasets/simple_macro_output/zn_women.RData")
load("datasets/simple_macro_output/vb12_women.RData")


##################### covariates #############################################
#census data that requires some cleaning 
census11 <- india11_census %>% 
  rename(d_name = `District name`,
         s_name = `State name`,
         pc11_district_id = `District code`) %>% 
  mutate(
    #create rates for adm2 region
    lit_m = Male_Literate/Male,
    lit_f = Female_Literate/Female,
    sc_share = SC/Population,
    st_share = ST/Population,
    hindu = Hindus/Population,
    muslim = Muslims/Population,
    christian = Christians/Population,
    sikh = Sikhs/Population,
    buddhist = Buddhists/Population,
    jain = Jains/Population,
    other_rel = Others_Religions/Population, 
    religion_ns = Religion_Not_Stated/Population,
    women_work = Female_Workers/Female,
    male_work = Male_Workers/Male,
    marginal_work = Marginal_Workers/Population,
    non_work = Non_Workers/Population, 
    cultivate_work = Cultivator_Workers/Population,
    agri_work = Agricultural_Workers/Population,
    house_work = Household_Workers/Population, 
    other_work = Other_Workers/Population, 
    hh_lights = Housholds_with_Electric_Lighting/Households,
    hh_internet = Households_with_Internet/Households,
    hh_computer = Households_with_Computer/Households,
    rural_hh = Rural_Households/Households,
    urban_hh = Urban_Households/Households,
    ed_below_prim = Below_Primary_Education/Total_Education,
    ed_prim = Primary_Education/Total_Education, 
    ed_middle = Middle_Education/Total_Education, 
    ed_sec = Secondary_Education/Total_Education, 
    ed_higher = Higher_Education/Total_Education, 
    ed_grad = Graduate_Education/ Total_Education,
    ed_other = Other_Education/Total_Education, 
    hh_bike = Households_with_Bicycle/Households,
    hh_car = Households_with_Bicycle/Households,
    hh_radio = Households_with_Radio_Transistor/Households,
    hh_scooter = Households_with_Scooter_Motorcycle_Moped/Households,
    hh_tv = Households_with_Television/Households,
    hh_sepearate_kitchen = Households_with_separate_kitchen_Cooking_inside_house/Households, 
    hh_bathing = Having_bathing_facility_Total_Households/Households,
    hh_latrine = Having_latrine_facility_within_the_premises_Total_Households/Households,
    hh_owned =Ownership_Owned_Households/Households,
    hh_rented = Ownership_Rented_Households/Households,
    drink_uncovered_well = Main_source_of_drinking_water_Un_covered_well_Households/Households,
    drink_handpump = Main_source_of_drinking_water_Handpump_Tubewell_Borewell_Households/Households,
    drink_spring = Main_source_of_drinking_water_Spring_Households/Households,
    drink_river = Main_source_of_drinking_water_River_Canal_Households/Households,
    drink_other = Main_source_of_drinking_water_Other_sources_Households/Households,
    pp_45000 = Power_Parity_Less_than_Rs_45000/Total_Power_Parity,
    pp_45000_90000 = Power_Parity_Rs_45000_90000/Total_Power_Parity,
    pp_90_150 = Power_Parity_Rs_90000_150000/Total_Power_Parity,
    pp_150_240 = Power_Parity_Rs_150000_240000/Total_Power_Parity,
    pp_240_330 = Power_Parity_Rs_240000_330000/Total_Power_Parity,
    pp_330_425 = Power_Parity_Rs_330000_425000/Total_Power_Parity,
    pp_425_545 = Power_Parity_Rs_425000_545000/Total_Power_Parity,
    pp_545 = Power_Parity_Above_Rs_545000/Total_Power_Parity
  ) %>% 
  select(
    d_name,
    s_name,
    pc11_district_id,
    lit_m,
    lit_f,
    sc_share,
    st_share,
    hindu,
    muslim,
    christian,
    sikh,
    buddhist,
    jain,
    other_rel,
    religion_ns,
    women_work,
    male_work,
    marginal_work,
    non_work,
    cultivate_work,
    agri_work,
    house_work,
    other_work,
    hh_lights,
    hh_internet,
    hh_computer,
    rural_hh,
    urban_hh,
    ed_below_prim,
    ed_prim,
    ed_middle,
    ed_sec,
    ed_higher,
    ed_grad,
    ed_other,
    hh_bike,
    hh_car,
    hh_radio,
    hh_scooter,
    hh_tv,
    hh_sepearate_kitchen,
    hh_bathing,
    hh_latrine,
    hh_owned,
    hh_rented,
    drink_uncovered_well,
    drink_handpump,
    drink_spring,
    drink_river,
    drink_other,
    pp_45000,
    pp_45000_90000,
    pp_90_150,
    pp_150_240,
    pp_240_330,
    pp_330_425,
    pp_425_545,
    pp_545
  )



#combine dataa
predictive_data <-census11 %>%            # by = c("pc11_district_id", "pc11_state_id")) %>%
  inner_join(india_adm2 %>% rename(pc11_district_id = pc11_d_id,
                                   pc11_state_id = pc11_s_id) %>% 
               select(!geometry) %>% 
               mutate(pc11_district_id = as.numeric(pc11_district_id)) %>% 
               mutate(pc11_district_id = ifelse(ADM2_NAME == "Vadodara", 486,pc11_district_id )) %>% 
              mutate(pc11_district_id = ifelse(ADM2_NAME == "Sabar Kantha", 472,pc11_district_id )),            
             by = c("pc11_district_id")  ) %>%
  filter(s_name == "UTTAR PRADESH" |
           s_name == "ANDHRA PRADESH" |
           s_name == "TAMIL NADU"|
           s_name == "KERALA" |
           s_name == "MAHARASHTRA"|
           s_name == "MADHYA PRADESH" |
           s_name == "GUJARAT" |
           s_name == "KARNATAKA" |
           s_name == "ORISSA" |
           s_name == "WEST BENGAL")

  # shrug_secc_rural %>%
  # select("pc11_district_id",
  #        "pc11_state_id"#,
  #        # "land_own_share",#land ownership
  #        # #education
  #        # "ed_some_share",
  #        # "ed_prim_share",
  #        # "ed_sec_share",
  #        # "ed_mid_share",
  #        # "ed_ssec_share",
  #        # "ed_grad_share",
  #        # #caste
  #        # "st_share",
  #        # "sc_share",
  #        # "ptg_share",
  #        # #income
  #        # "inc_source_cultiv_share",
  #        # "inc_source_manlab_share",
  #        # "inc_source_domest_share",
  #        # "inc_source_forage_share",
  #        # "inc_source_enterpr_share",
  #        # "inc_source_beg_share",
  #        # "inc_source_other_share"
  #        ) %>%
  # full_join(shrug_pca11 %>% 
  #              select(
  #                "pc11_district_id",
  #                "pc11_state_id",
  #                #scheudled caste pop dist
  #               
  #             
  #              ),
  #            by = c("pc11_district_id", "pc11_state_id")) %>%
  # full_join(shrug_secc_urban %>% 
  #              select(
  #                "pc11_district_id",
  #                "pc11_state_id",
  #                #hh type
  #                "house_type1",
  #                "house_type2",
  #                "house_type3",
  #                "house_own1",
  #                "house_own2",
  #                "house_own3",
  #                "house_own4",
  #                "house_own5",
  #                "house_own6"),
  #                by = c("pc11_district_id", "pc11_state_id")) %>% 
  # inner_join(shrug_consumption %>%
  #              select(
  #                "pc11_district_id",
  #                "pc11_state_id",
  #                #poverty
  #                "secc_pov_rate_rural",
  #








################## data manipulation and merging #############################


#calculate the difference of inadequacy, women minus men 
folate_target <- fo_men %>% 
  rename(men_intake_mcg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mcg,men_N, ADM1) %>% 
  inner_join(
    (fo_women %>% 
      rename(women_intake_mcg = mean,
             women_N = N,
             women_inad_perc = inadequate_percent,
             ADM1 = note) %>% 
      select(ADM2_NAME,women_inad_perc, women_intake_mcg,women_N, ADM1)),
    by = c("ADM2_NAME", "ADM1")
  )%>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))

iron_target <- ir_men %>% 
  rename(men_intake_mg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mg,men_N, ADM1) %>% 
  inner_join(
    (ir_women %>% 
       rename(women_intake_mg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mg,women_N,ADM1)),
    by = c("ADM2_NAME","ADM1")
  ) %>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))#2 = pos, 1 = neg

zinc_target <- zn_men %>% 
  rename(men_intake_mg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mg,men_N,ADM1) %>% 
  inner_join(
    (zn_women %>% 
       rename(women_intake_mg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mg,women_N,ADM1)),
    by = c("ADM2_NAME","ADM1")
  )%>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))

vita_target <- va_men %>% 
  rename(men_intake_mcg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mcg,men_N, ADM1) %>% 
  inner_join(
    (va_women %>% 
       rename(women_intake_mcg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mcg,women_N, ADM1)),
    by = c("ADM2_NAME","ADM1")
  ) %>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))#2 = pos, 1 = neg

vitb12_taget <- vb12_men %>% 
  rename(men_intake_mcg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mcg,men_N, ADM1) %>% 
  inner_join(
    (va_women %>% 
       rename(women_intake_mcg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mcg,women_N, ADM1)),
    by = c("ADM2_NAME","ADM1")
  ) %>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))#2 = pos, 1 = neg




######################## export datasets #####################################

# Joining the covariates to targets for both overall inadequacy by sex, and the 
# difference in inadequacy. 

vita_men_ml <- va_men %>% left_join(predictive_data, by = c("ADM2_NAME")) %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 
  

fo_men_ml <- fo_men %>% left_join(predictive_data, by = "ADM2_NAME")%>%  
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 

ir_men_ml <- ir_men %>% left_join(predictive_data, by = "ADM2_NAME")%>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 
zn_men_ml <- zn_men %>% left_join( predictive_data, by = "ADM2_NAME")%>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 

vita_women_ml <- va_women %>% left_join(predictive_data, by = "ADM2_NAME") %>%  
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 
fo_women_ml <- fo_women %>% left_join(predictive_data, by = "ADM2_NAME")%>%  
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 

ir_women_ml <- ir_women %>% left_join(predictive_data, by = "ADM2_NAME")%>%  
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 

zn_women_ml <- zn_women %>% left_join( predictive_data, by = "ADM2_NAME")%>%  
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID)) 

#inadequacy difference targets
vita_target <- vita_target  %>% left_join(predictive_data %>% unique(), by = "ADM2_NAME") %>% 
select(!c(                   
          pc11_district_id,pc11_state_id,               
          shapeGroup,shapeISO,shapeID,s_name)) %>% 
  select(
  !c(men_inad_perc,men_N,men_intake_mcg,ADM1,women_inad_perc,women_intake_mcg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()


zinc_target<- zinc_target  %>% left_join(predictive_data, by = "ADM2_NAME")  %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID)) %>% 
  select(
    !c(men_inad_perc,men_N,men_intake_mg,ADM1,women_inad_perc,women_intake_mg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()

folate_target<- folate_target  %>% left_join(predictive_data, by = "ADM2_NAME")  %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID)) %>% 
  select(
    !c(men_inad_perc,men_N,men_intake_mcg,ADM1,women_inad_perc,women_intake_mcg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()

iron_target<- iron_target  %>% left_join(predictive_data, by = "ADM2_NAME") %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID)) %>% 
  select(
    !c(men_inad_perc,men_N,men_intake_mg,ADM1,women_inad_perc,women_intake_mg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()

# save the datasets
save(vita_men_ml, file = "datasets/ml_input_datasets/vita_men_ml.RData")
save(fo_men_ml, file = "datasets/ml_input_datasets/fo_men_ml.RData")
save(ir_men_ml, file = "datasets/ml_input_datasets/ir_men_ml.RData")
save(zn_men_ml, file = "datasets/ml_input_datasets/zn_men_ml.RData")
save(vita_women_ml, file = "datasets/ml_input_datasets/vita_women_ml.RData")
save(fo_women_ml, file = "datasets/ml_input_datasets/fo_women_m.RData")
save(ir_women_ml, file = "datasets/ml_input_datasets/ir_women_ml.RData")
save(zn_women_ml, file = "datasets/ml_input_datasets/zn_women_ml.RData")


save(vita_target, file = "datasets/ml_input_datasets/vita_target_bin.RData")
save(folate_target, file = "datasets/ml_input_datasets/folate_target_bin.RData")
save(iron_target, file =  "datasets/ml_input_datasets/iron_target.RData")
save(zinc_target, file = "datasets/ml_input_datasets/zinc_target.RData")


write.csv(vita_target,"datasets/ml_input_datasets/vita_target.csv", append = FALSE)
write.csv(folate_target,"datasets/ml_input_datasets/folate_target.csv", append = FALSE )
write.csv(iron_target,"datasets/ml_input_datasets/iron_target.csv", append = FALSE )
write.csv(zinc_target,"datasets/ml_input_datasets/zinc_target.csv" , append = FALSE)
