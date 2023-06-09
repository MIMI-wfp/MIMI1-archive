#creating usable datasets for ML training 
# G-Battcock

##################### load in previous data sets #############################

load("datasets/usual_intake_SM/fo_men.RData")
load("datasets/usual_intake_SM/ir_men.RData")
load("datasets/usual_intake_SM/va_men.RData")
load("datasets/usual_intake_SM/zn_men.RData")
load("datasets/usual_intake_SM/vb12_men.RData")
load("datasets/usual_intake_SM/fo_women.RData")
load("datasets/usual_intake_SM/ir_women.RData")
load("datasets/usual_intake_SM/va_women.RData")
load("datasets/usual_intake_SM/zn_women.RData")
load("datasets/usual_intake_SM/vb12_women.RData")


################## data manipulation and merging #############################

folate_target <- fo_men %>% 
  rename(men_intake_mcg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mcg,men_N) %>% 
  inner_join(
    (fo_women %>% 
      rename(women_intake_mcg = mean,
             women_N = N,
             women_inad_perc = inadequate_percent,
             ADM1 = note) %>% 
      select(ADM2_NAME,women_inad_perc, women_intake_mcg,women_N)),
    by = c("ADM2_NAME")
  )%>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))

iron_target <- ir_men %>% 
  rename(men_intake_mg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mg,men_N) %>% 
  inner_join(
    (ir_women %>% 
       rename(women_intake_mg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mg,women_N)),
    by = c("ADM2_NAME")
  ) %>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))#2 = pos, 1 = neg

zinc_target <- zn_men %>% 
  rename(men_intake_mg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mg,men_N) %>% 
  inner_join(
    (zn_women %>% 
       rename(women_intake_mg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mg,women_N)),
    by = c("ADM2_NAME")
  )%>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))

vita_target <- va_men %>% 
  rename(men_intake_mcg = mean,
         men_N = N,
         men_inad_perc = inadequate_percent,
         ADM1 = note) %>% 
  select(ADM2_NAME,men_inad_perc, men_intake_mcg,men_N) %>% 
  inner_join(
    (va_women %>% 
       rename(women_intake_mcg = mean,
              women_N = N,
              women_inad_perc = inadequate_percent,
              ADM1 = note) %>% 
       select(ADM2_NAME,women_inad_perc, women_intake_mcg,women_N)),
    by = c("ADM2_NAME")
  ) %>% 
  mutate(inad_diff = women_inad_perc - men_inad_perc) %>% 
  mutate(inad_diff_bin = factor(ifelse(inad_diff>=0,2,1)))#2 = pos, 1 = neg




######################## export datasets #####################################

save(vita_target, file = paste0(path_to_datasets, "vita_target.RData"))
save(folate_target, file = paste0(path_to_datasets, "folate_target.RData"))
save(iron_target, file = paste0(path_to_datasets, "iron_target.RData"))
save(zinc_target, file = paste0(path_to_datasets, "zinc_target.RData"))



#######