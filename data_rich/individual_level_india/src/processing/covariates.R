#### creating input variables for machine learning 
source()
source(paste0(path_to_script,"data_loading.R"))


head(shrug_secc_rural)
names(shrug_secc_rural)
shrug_secc_rural$pc11_district_id

#exploring some of the data points
hist(shrug_secc_rural$ed_grad_share)#education at grad school or above
hist(shrug_secc_rural$inc_10k_plus_share)#income Rs10k+ 
hist(shrug_secc_rural$sc_share)

#make a list of covariates, all propoprtions at adm2 level (0,1)

covariate_list <- c(
  land_own_share,#land ownership
  #education
  ed_some_share,
  ed_prim_share,
  ed_sec_share,
  ed_mid_share,
  ed_ssec_share,
  ed_grad_share,
  #caste
  st_share,
  sc_share,
  ptg_share,
  #income
  inc_source_cultiv_share,
  inc_source_manlab_share,
  inc_source_domest_share,
  inc_source_forage_share,
  inc_source_enterpr_share,
  inc_source_beg_share,
  inc_source_other_share,
  
  
)


