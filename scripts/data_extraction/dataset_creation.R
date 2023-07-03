#creating usable datasets for ML training 
# G-Battcock

##################### load in previous data sets #############################

load("datasets/fo_men.RData")
load("datasets/ir_men.RData")
load("datasets/va_men.RData")
load("datasets/zn_men.RData")
load("datasets/vb12_men.RData")
load("datasets/fo_women.RData")
load("datasets/ir_women.RData")
load("datasets/va_women.RData")
load("datasets/zn_women.RData")
load("datasets/vb12_women.RData")


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

clean_census %>% 
  inner_join(religion11, by  = "d_name")#test it maps

clean_census[28,]
shrug_shape %>%filter(d_name == clean_census[28,]$d_name)

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
  inner_join(shrug_pca11 %>% 
               select(
                 "pc11_district_id",
                 "pc11_state_id",
                 #scheudled caste pop dist
                 "pc11_pca_m_sc",
                 "pc11_pca_m_st",
                 "pc11_pca_f_sc",
                 "pc11_pca_f_st",
                 #literacy
                 "pc11_pca_p_lit",
                 "pc11_pca_m_lit",
                 "pc11_pca_f_lit"
               ),
             by = c("pc11_district_id", "pc11_state_id")) %>%
  inner_join(shrug_secc_urban %>% 
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
  inner_join(shrug_shape %>% rename(pc11_district_id = pc11_d_id,
                                    pc11_state_id = pc11_s_id),            
             by = c("pc11_district_id", "pc11_state_id") ) %>% 
  select(!geometry) %>% 
  mutate(pc11_district_id = as.numeric(pc11_district_id)) %>% 
  inner_join(religion11, by  = "pc11_district_id") %>% 
  filter(c("UTTAR PRADESH",
                     "ANDHRA PRADESH",
                     "TAMIL NADU",
                     "MAHARASHTRA",
                     "MADHYA PRADESH",
                     "GUJARAT",
                     "KARNATAKA",
                     "ORISSA",
                     "WEST BENGAL") %in% s_name)
  

names(shrug_all)

#
fo_women %>% rename(d_name = ADM2_NAME) %>% 
  left_join(shrug_all, by = "d_name")

adm2_list <- fo_women %>% filter(!(ADM2_NAME %in% shrug_all$d_name)) %>% select(ADM2_NAME)
adm2_list2 <- shrug_all %>% filter((d_name %in% fo_women$ADM2_NAME) ) %>% select(d_name, s_name)

covariate_list <- c(
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
  #scheudled caste pop dist
  "pc11_pca_m_sc",
  "pc11_pca_m_st",
  "pc11_pca_f_sc",
  "pc11_pca_f_st",
  #literacy
  "pc11_pca_p_lit",
  "pc11_pca_m_lit",
  "pc11_pca_f_lit",
  #workers
  "pc11_pca_mainwork_m",
  "pc11_pca_mainwork_f",
  "pc11_pca_main_cl_m",
  "pc11_pca_main_cl_f",
  "pc11_pca_main_hh_m",
  "pc11_pca_main_hh_f",
  "pc11_pca_main_ot_m",
  "pc11_pca_main_ot_f",
  "pc11_pca_margwork_m",
  "pc11_pca_margwork_f",
  #income
  "inc_source_cultiv_share",
  "inc_source_manlab_share",
  "inc_source_domest_share",
  "inc_source_forage_share",
  "inc_source_enterpr_share",
  "inc_source_beg_share",
  "inc_source_other_share",
  #poverty
  "secc_pov_rate_rural",
  "secc_pov_rate_tend_rural",
  #hh type
  "house_type1",
  "house_type2",
  "house_type3",
  "house_own1",
  "house_own2",
  "house_own3",
  "house_own4",
  "house_own5",
  "house_own6"
  #possession
  # phone1,
  # phone2,
  # phone3,
  # phone4,
  # refrig0,
  # refrig1,
  # ac0,
  # ac1,
  # wash_mac0,
  # wash_mac1,
  # kitchen0,
  # kitchen1,
  # slum0,
  
)

cov <- shrug_all %>% select(all_of(covariate_list))




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

save(vita_target, file = paste0(path_to_datasets, "vita_target.RData"))
save(folate_target, file = paste0(path_to_datasets, "folate_target.RData"))
save(iron_target, file = paste0(path_to_datasets, "iron_target.RData"))
save(zinc_target, file = paste0(path_to_datasets, "zinc_target.RData"))

load("datasets/usual_intake_SM/vita_target.RData")

load(paste(path_to_datasets, "vita_target.RData"))

vita_shp <- vita_target %>% inner_join(shrug_shape %>% rename(ADM2_NAME = d_name), by = "ADM2_NAME")
zinc_target
folate_target
iron_target
