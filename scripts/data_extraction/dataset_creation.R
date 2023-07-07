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
religion11 <- india11_census %>% 
  select(`District name`,
         `State name`,
         `District code`,
         Male,
         Female,
         Population,
         Male_Literate,
         Female_Literate,
         Hindus,
         Muslims,
         Christians,
         Sikhs,
         Buddhists,
         Jains,
         Others_Religions,
         Religion_Not_Stated) %>% 
  rename(d_name = `District name`,
         s_name = `State name`,
         pc11_district_id = `District code`) %>% 
  mutate(
    lit_m = Male_Literate/Male,
    lit_f = Female_Literate/Female,
    hindu = Hindus/Population,
    muslim = Muslims/Population,
    christian = Christians/Population,
    sikh = Sikhs/Population,
    buddhist = Buddhists/Population,
    jain = Jains/Population,
    other_rel = Others_Religions/Population, 
    religion_ns = Religion_Not_Stated/Population
  ) %>% 
  select(pc11_district_id,
         s_name,
         lit_m,
         lit_f,
         hindu,
         muslim,
         christian,
         sikh,
         buddhist,
         jain,
         other_rel,
         religion_ns)




#combine dataa
shrug_all <-shrug_secc_rural %>% 
  select("pc11_district_id",
         "pc11_state_id",
         "land_own_share",#land ownership
         #education
         "ed_some_share",
         "ed_prim_share",
         "ed_sec_share",
         "ed_mid_share",
         "ed_ssec_share",
         "ed_grad_share",
         #caste
         "st_share",
         "sc_share",
         "ptg_share",
         #income
         "inc_source_cultiv_share",
         "inc_source_manlab_share",
         "inc_source_domest_share",
         "inc_source_forage_share",
         "inc_source_enterpr_share",
         "inc_source_beg_share",
         "inc_source_other_share") %>% 
  # full_join(shrug_pca11 %>% 
  #              select(
  #                "pc11_district_id",
  #                "pc11_state_id",
  #                #scheudled caste pop dist
  #               
  #             
  #              ),
  #            by = c("pc11_district_id", "pc11_state_id")) %>%
  full_join(shrug_secc_urban %>% 
               select(
                 "pc11_district_id",
                 "pc11_state_id",
                 #hh type
                 "house_type1",
                 "house_type2",
                 "house_type3",
                 "house_own1",
                 "house_own2",
                 "house_own3",
                 "house_own4",
                 "house_own5",
                 "house_own6"),
                 by = c("pc11_district_id", "pc11_state_id")) %>% 
  inner_join(shrug_consumption %>%
               select(
                 "pc11_district_id",
                 "pc11_state_id",
                 #poverty
                 "secc_pov_rate_rural",
                 "secc_pov_rate_tend_rural"
               ),
             by = c("pc11_district_id", "pc11_state_id")) %>%
  inner_join(india_adm2 %>% rename(pc11_district_id = pc11_d_id,
                                    pc11_state_id = pc11_s_id),            
             by = c("pc11_district_id", "pc11_state_id") ) %>% 
  select(!geometry) %>% 
  mutate(pc11_district_id = as.numeric(pc11_district_id)) %>% 
  inner_join(religion11, by  = "pc11_district_id") %>% 
  filter(s_name == "UTTAR PRADESH" |
           s_name == "ANDHRA PRADESH" |
           s_name == "TAMIL NADU"|
           s_name == "MAHARASHTRA"|
           s_name == "MADHYA PRADESH" |
           s_name == "GUJARAT" |
           s_name == "KARNATAKA" |
           s_name == "ORISSA" |
           s_name == "WEST BENGAL") 
  

names(shrug_all)

#


# covariate_list <- c(
#   "land_own_share",#land ownership
#   #education
#   "ed_some_share",
#   "ed_prim_share",
#   "ed_sec_share",
#   "ed_mid_share",
#   "ed_ssec_share",
#   "ed_grad_share",
#   #caste
#   "st_share",
#   "sc_share",
#   "ptg_share",
#   #scheudled caste pop dist
#   "pc11_pca_m_sc",
#   "pc11_pca_m_st",
#   "pc11_pca_f_sc",
#   "pc11_pca_f_st",
#   #literacy
#   "pc11_pca_p_lit",
#   "pc11_pca_m_lit",
#   "pc11_pca_f_lit",
#   #workers
#   "pc11_pca_mainwork_m",
#   "pc11_pca_mainwork_f",
#   "pc11_pca_main_cl_m",
#   "pc11_pca_main_cl_f",
#   "pc11_pca_main_hh_m",
#   "pc11_pca_main_hh_f",
#   "pc11_pca_main_ot_m",
#   "pc11_pca_main_ot_f",
#   "pc11_pca_margwork_m",
#   "pc11_pca_margwork_f",
#   #income
#   "inc_source_cultiv_share",
#   "inc_source_manlab_share",
#   "inc_source_domest_share",
#   "inc_source_forage_share",
#   "inc_source_enterpr_share",
#   "inc_source_beg_share",
#   "inc_source_other_share",
#   #poverty
#   "secc_pov_rate_rural",
#   "secc_pov_rate_tend_rural",
#   #hh type
#   "house_type1",
#   "house_type2",
#   "house_type3",
#   "house_own1",
#   "house_own2",
#   "house_own3",
#   "house_own4",
#   "house_own5",
#   "house_own6"
#   #possession
#   # phone1,
#   # phone2,
#   # phone3,
#   # phone4,
#   # refrig0,
#   # refrig1,
#   # ac0,
#   # ac1,
#   # wash_mac0,
#   # wash_mac1,
#   # kitchen0,
#   # kitchen1,
#   # slum0,
#   
# )






################## data manipulation and merging #############################

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

# save(vita_target, file = paste0(path_to_datasets, "vita_target.RData"))
# save(folate_target, file = paste0(path_to_datasets, "folate_target.RData"))
# save(iron_target, file = paste0(path_to_datasets, "iron_target.RData"))
# save(zinc_target, file = paste0(path_to_datasets, "zinc_target.RData"))

vita_men_ml <- va_men %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
  

fo_men_ml <- fo_men %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
ir_men_ml <- ir_men %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
zn_men_ml <- zn_men %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 

vita_women_ml <- va_women %>% left_join(shrug_all, by = "ADM2_NAME") %>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
fo_women_ml <- fo_women %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
ir_women_ml <- ir_women %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
zn_women_ml <- zn_women %>% left_join(shrug_all, by = "ADM2_NAME")%>% filter(s_name != "KERALA") %>% 
  select(!c("note","N", "mean", P_25, median, P_75,                   
            No_replicates,pc11_district_id,pc11_state_id,               
            shapeGroup,shapeISO,shapeID,s_name)) 
#inadequacy difference targets
vita_target <- vita_target  %>% left_join(shrug_all, by = "ADM2_NAME") %>% filter(s_name != "KERALA") %>% 
select(!c(                   
          pc11_district_id,pc11_state_id,               
          shapeGroup,shapeISO,shapeID,s_name)) %>% 
  select(
  !c(men_inad_perc,men_N,men_intake_mcg,ADM1,women_inad_perc,women_intake_mcg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()


zinc_target<- zinc_target  %>% left_join(shrug_all, by = "ADM2_NAME")  %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID,s_name)) %>% 
  select(
    !c(men_inad_perc,men_N,men_intake_mg,ADM1,women_inad_perc,women_intake_mg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()

folate_target<- folate_target  %>% left_join(shrug_all, by = "ADM2_NAME")  %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID,s_name)) %>% 
  select(
    !c(men_inad_perc,men_N,men_intake_mcg,ADM1,women_inad_perc,women_intake_mcg,women_N)) %>% 
  select(!c(ADM2_NAME, inad_diff_bin)) %>% mutate(inad_diff = inad_diff/100) %>% drop_na()

iron_target<- iron_target  %>% left_join(shrug_all, by = "ADM2_NAME") %>% 
  select(!c(                   
    pc11_district_id,pc11_state_id,               
    shapeGroup,shapeISO,shapeID,s_name)) %>% 
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