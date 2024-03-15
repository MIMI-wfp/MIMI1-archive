##################################################################################################################################
############################################### Ethiopia HCES 1516 Fortification Scenarios #######################################
##################################################################################################################################

#Install Packages
pacman::p_load(
  here,                      # for directory simplification
  foreign,                   # for reading filetypes in SPSS or STATA format
  stringr,                   # string variable engineering
  psych,                     # descriptive statistics
  lubridate,                 # working with date/time variables
  survey,srvyr,              # for analysis of populatiion survey data
  MASS, reshape, reshape2,    # for reashaping data (e.g., melting)
  cowplot,                   # for figures
  tidyverse                  # for data processing
)

#Identify sources of each food item consumed to isolate out foods from own production
eth_hces1516_SOURCE <- eth_hces1516_foodbev %>% 
  group_by(SOURCE) %>% 
  summarise(n=n()) %>% 
  print(n=40)
write_csv(eth_hces1516_SOURCE, here("./data/ETH_HCES1516/ETH_HCES1516_SOURCE.csv"))

eth_hces1516_fcode <- read.csv(here("./data/ETH_HCES1516/ETH_HCES1516_fcode.csv"))
eth_hces1516_wheatoil <- eth_hces1516_fcode %>% dplyr::select(ITEMC, wheat_oil, wheat, oil)

eth_hces1516_wheat_fortfactor <- read.csv(here("./data/ETH_HCES1516/ETH_HCES1516_wheatproductfortifiable.csv"))
eth_hces1516_wheat_fortfactor <- eth_hces1516_wheat_fortfactor %>% select(ITEMC, prop_fortifiable)

#Reach
eth_hces1516_foodbev %>%
  dplyr::left_join(., eth_hces1516_wheatoil, by ='ITEMC') %>% 
  mutate(wheat_fort = if_else(wheat==1 & SOURCE!="Consumption of Own Agricultural Production", 1, 0)) %>%
  mutate(wheat_fort = if_else(is.na(wheat_fort), 0, wheat_fort)) %>%
  group_by(hhid, CQ11) %>% 
  dplyr::summarise(reach = if_else(sum(wheat_fort)>0, 1, 0)) %>% 
  group_by(CQ11) %>% 
  dplyr::summarise( reach_perc = sum(reach) / n() *100)

#Contributions from wheat flours
eth_hces1516_wheat_quant <- eth_hces1516_foodbev %>%
  dplyr::left_join(., eth_hces1516_wheatoil, by ='ITEMC') %>% 
  dplyr::left_join(., eth_hces1516_wheat_fortfactor, by ='ITEMC') %>% 
  filter(wheat==1) %>% 
  filter(SOURCE!="Consumption of Own Agricultural Production") %>% 
  mutate(wheat_f_quant = QUANTITY / 365 * prop_fortifiable) %>% 
  group_by(hhid, UR, CQ11, CQ12) %>% 
  dplyr::summarise(wheat_f_quant = sum(wheat_f_quant))
  
eth_hces1516_wheat_fortcont <- eth_hces1516_wheat_quant %>% 
  ungroup() %>% 
  mutate(standard_thiamine_in_mg = 0.00765,
         standard_riboflavin_in_mg = 0.0054,
         standard_niacin_in_mg = 0.045,
         standard_vitaminb6_in_mg = 0.0051,
         standard_folate_in_mcg = 2.89,
         standard_vitaminb12_in_mcg = 0.018,
         standard_zinc_in_mg = 0.08) %>% 
  filter(wheat_f_quant!=0) %>% 
  mutate(hh_fortcont_thia = wheat_f_quant * standard_thiamine_in_mg,
         hh_fortcont_ribo = wheat_f_quant * standard_riboflavin_in_mg,
         hh_fortcont_niac = wheat_f_quant * standard_niacin_in_mg,
         hh_fortcont_vb6 = wheat_f_quant * standard_vitaminb6_in_mg,
         hh_fortcont_fol = wheat_f_quant * standard_folate_in_mcg,
         hh_fortcont_vb12 = wheat_f_quant * standard_vitaminb12_in_mcg,
         hh_fortcont_zn = wheat_f_quant * standard_zinc_in_mg) %>% 
  dplyr::select(hhid, starts_with("hh_fortcont_"))

#CORRECTION FOR WHEAT OVERREPORTING, TRUNCATION AT 300g/d per AFE [USED THIS ASSUMPTION]
eth_hces1516_wheat_fortcont <- eth_hces1516_wheat_quant %>% 
  dplyr::left_join(., eth_hces1516_afe, by ='hhid') %>% 
  mutate(wheat_f_quant = if_else(wheat_f_quant/afe>300, (300*afe), wheat_f_quant)) %>% 
  mutate(standard_thiamine_in_mg = 0.00765,
         standard_riboflavin_in_mg = 0.0054,
         standard_niacin_in_mg = 0.045,
         standard_vitaminb6_in_mg = 0.0051,
         standard_folate_in_mcg = 2.89,
         standard_vitaminb12_in_mcg = 0.018,
         standard_zinc_in_mg = 0.08) %>% 
  filter(wheat_f_quant!=0) %>% 
  mutate(hh_fortcont_thia = wheat_f_quant * standard_thiamine_in_mg,
         hh_fortcont_ribo = wheat_f_quant * standard_riboflavin_in_mg,
         hh_fortcont_niac = wheat_f_quant * standard_niacin_in_mg,
         hh_fortcont_vb6 = wheat_f_quant * standard_vitaminb6_in_mg,
         hh_fortcont_fol = wheat_f_quant * standard_folate_in_mcg,
         hh_fortcont_vb12 = wheat_f_quant * standard_vitaminb12_in_mcg,
         hh_fortcont_zn = wheat_f_quant * standard_zinc_in_mg) %>% 
  ungroup() %>% 
  dplyr::select(hhid, starts_with("hh_fortcont_"))
  
eth_hces1516_wheat_quant %>% 
  ggplot()+
  geom_histogram(aes(x=afe)) +
    facet_wrap(~CQ11)

#Contributions from edible oils
eth_hces1516_oil_quant <- eth_hces1516_foodbev %>%
  dplyr::left_join(., eth_hces1516_wheatoil, by ='ITEMC') %>% 
  filter(oil==1) %>% 
  mutate(oil_f_quant = QUANTITY / 365) %>% 
  group_by(hhid, UR, CQ11, CQ12) %>% 
  dplyr::summarise(oil_f_quant = sum(oil_f_quant))

eth_hces1516_oil_fortcont <- eth_hces1516_oil_quant %>% 
  ungroup() %>% 
  mutate(standard_vitamina_in_mcg = 13.6,
         standard_vitamind_in_mcg = 0.141) %>% 
  filter(oil_f_quant!=0) %>% 
  mutate(hh_fortcont_va = oil_f_quant * standard_vitamina_in_mcg,
         hh_fortcont_vd = oil_f_quant * standard_vitamind_in_mcg) %>% 
  dplyr::select(hhid, starts_with("hh_fortcont_"))

eth_hces1516_oil_quant  %>% 
  dplyr::left_join(., eth_hces1516_afe, by ='hhid') %>% 
  ggplot() +
  geom_histogram(aes(x=oil_f_quant/afe)) +
  facet_wrap(~CQ11)+
  coord_cartesian(xlim=c(0,300))

#CORRECTION FOR OIL OVERREPORTING, TRUNCATION AT 100g/d per AFE [USED THIS ASSUMPTION]
eth_hces1516_oil_fortcont <- eth_hces1516_oil_quant %>% 
  dplyr::left_join(., eth_hces1516_afe, by ='hhid') %>% 
  mutate(oil_f_quant = if_else(oil_f_quant/afe>100, (100*afe), oil_f_quant)) %>% 
  mutate(standard_vitamina_in_mcg = 13.6,
         standard_vitamind_in_mcg = 0.141) %>% 
  filter(oil_f_quant!=0) %>% 
  mutate(hh_fortcont_va = oil_f_quant * standard_vitamina_in_mcg,
         hh_fortcont_vd = oil_f_quant * standard_vitamind_in_mcg) %>% 
  ungroup() %>% 
  dplyr::select(hhid, starts_with("hh_fortcont_"))


#Combining wheat and edible oil fortificants
eth_hces1516_foodbev$district_code <- paste0(as.character(eth_hces1516_foodbev$CQ11),
                             "_",
                             as.character(eth_hces1516_foodbev$CQ12))

fort1 <- eth_hces1516_foodbev %>%
  dplyr::left_join(., eth_hces1516_fctmatch, by ='ITEMC') %>% 
  dplyr::left_join(., eth_hces1516_afe, by ='hhid') %>% 
  filter(!is.na(zn_in_mg)) %>% 
  mutate(
    ai_item_energy = (QUANTITY * energy_in_kcal/100)/365/afe,
    ai_item_va = (QUANTITY * vitamina_in_rae_in_mcg/100)/365/afe,
    ai_item_vd = (QUANTITY * vitamind_in_mcg/100)/365/afe,
    ai_item_thia = (QUANTITY * thiamin_in_mg/100)/365/afe,
    ai_item_ribo = (QUANTITY * riboflavin_in_mg/100)/365/afe,
    ai_item_niac = (QUANTITY * niacin_in_mg/100)/365/afe,
    ai_item_vb6 = (QUANTITY * vitaminb6_in_mg/100)/365/afe,
    ai_item_fol = (QUANTITY * folate_in_mcg/100)/365/afe,
    ai_item_vb12 = (QUANTITY * vitaminb12_in_mcg/100)/365/afe,
    ai_item_zn = (QUANTITY * zn_in_mg/100)/365/afe
  ) %>% 
  group_by(hhid, afe, REP, UR, EXPCC, CQ11, CQ12, district_code
  ) %>% 
  summarise(energy_ai = sum(ai_item_energy),
            va_ai = sum(ai_item_va),
            vd_ai = sum(ai_item_vd),
            thia_ai = sum(ai_item_thia),
            ribo_ai = sum(ai_item_ribo),
            niac_ai = sum(ai_item_niac),
            vb6_ai = sum(ai_item_vb6),
            fol_ai = sum(ai_item_fol),
            vb12_ai = sum(ai_item_vb12),
            zn_ai = sum(ai_item_zn)) %>% 
  dplyr::left_join(., eth_hces1516_wheat_fortcont, by ='hhid') %>% 
  dplyr::left_join(., eth_hces1516_oil_fortcont, by ='hhid')

fort1$hh_fortcont_thia[is.na(fort1$hh_fortcont_thia)] <- 0
fort1$hh_fortcont_ribo[is.na(fort1$hh_fortcont_ribo)] <- 0
fort1$hh_fortcont_niac[is.na(fort1$hh_fortcont_niac)] <- 0
fort1$hh_fortcont_vb6[is.na(fort1$hh_fortcont_vb6)] <- 0
fort1$hh_fortcont_fol[is.na(fort1$hh_fortcont_fol)] <- 0
fort1$hh_fortcont_vb12[is.na(fort1$hh_fortcont_vb12)] <- 0
fort1$hh_fortcont_zn[is.na(fort1$hh_fortcont_zn)] <- 0
fort1$hh_fortcont_va[is.na(fort1$hh_fortcont_va)] <- 0
fort1$hh_fortcont_vd[is.na(fort1$hh_fortcont_vd)] <- 0

#Base case
fort_base <- fort1 %>% 
  group_by(hhid, REP, UR, EXPCC, CQ11, CQ12, 
  ) %>% 
  summarise(va_ext = (va_ai)/490,
            vd_ext = (vd_ai)/10,
            thia_ext = (thia_ai)/0.9,
            ribo_ext = (ribo_ai)/1.45,
            niac_ext = (niac_ai)/14,
            vb6_ext = (vb6_ai)/1.3,
            fol_ext = (fol_ai)/290,
            vb12_ext = (vb12_ai)/2,
            zn_ext = (zn_ai)/12.7) %>% 
  mutate(va_cap_base = if_else(va_ext >= 1, 1, va_ext),
         vd_cap_base = if_else(vd_ext >= 1, 1, vd_ext),
         thia_cap_base = if_else(thia_ext >= 1, 1, thia_ext),
         ribo_cap_base = if_else(ribo_ext >= 1, 1, ribo_ext),
         niac_cap_base = if_else(niac_ext >= 1, 1, niac_ext),
         vb6_cap_base = if_else(vb6_ext >= 1, 1, vb6_ext),
         fol_cap_base = if_else(fol_ext >= 1, 1, fol_ext),
         vb12_cap_base = if_else(vb12_ext >= 1, 1, vb12_ext),
         zn_cap_base = if_else(zn_ext >= 1, 1, zn_ext)) %>%
  mutate(mimi_simple_cap_base = (va_cap_base + fol_cap_base + vb12_cap_base + zn_cap_base)/4, 
         mimi_full_cap_base = (va_cap_base + vd_cap_base + thia_cap_base + ribo_cap_base + niac_cap_base + vb6_cap_base + fol_cap_base + vb12_cap_base + zn_cap_base)/9,
         mimi_full_cap_base_novd = (va_cap_base + thia_cap_base + ribo_cap_base + niac_cap_base + vb6_cap_base + fol_cap_base + vb12_cap_base + zn_cap_base)/8) %>% 
  ungroup()

#All fortification vehicles
fort_all <- fort1 %>% 
  group_by(hhid, REP, UR, EXPCC, CQ11, CQ12, district_code
  ) %>% 
  summarise(va_ext = (va_ai+hh_fortcont_va/afe)/490,
            vd_ext = (vd_ai+hh_fortcont_vd/afe)/10,
            thia_ext = (thia_ai+hh_fortcont_thia/afe)/0.9,
            ribo_ext = (ribo_ai+hh_fortcont_ribo/afe)/1.45,
            niac_ext = (niac_ai+hh_fortcont_niac/afe)/14,
            vb6_ext = (vb6_ai+hh_fortcont_vb6/afe)/1.3,
            fol_ext = (fol_ai+hh_fortcont_fol/afe)/290,
            vb12_ext = (vb12_ai+hh_fortcont_vb12/afe)/2,
            zn_ext = (zn_ai+hh_fortcont_zn/afe)/12.7) %>% 
  mutate(va_cap_all = if_else(va_ext >= 1, 1, va_ext),
         vd_cap_all = if_else(vd_ext >= 1, 1, vd_ext),
         thia_cap_all = if_else(thia_ext >= 1, 1, thia_ext),
         ribo_cap_all = if_else(ribo_ext >= 1, 1, ribo_ext),
         niac_cap_all = if_else(niac_ext >= 1, 1, niac_ext),
         vb6_cap_all = if_else(vb6_ext >= 1, 1, vb6_ext),
         fol_cap_all = if_else(fol_ext >= 1, 1, fol_ext),
         vb12_cap_all = if_else(vb12_ext >= 1, 1, vb12_ext),
         zn_cap_all = if_else(zn_ext >= 1, 1, zn_ext)) %>%
  mutate(mimi_simple_cap_all = (va_cap_all + fol_cap_all + vb12_cap_all + zn_cap_all)/4, 
         mimi_full_cap_all = (va_cap_all + vd_cap_all + thia_cap_all + ribo_cap_all + niac_cap_all + vb6_cap_all + fol_cap_all + vb12_cap_all + zn_cap_all)/9,
         mimi_full_cap_all_novd = (va_cap_all + thia_cap_all + ribo_cap_all + niac_cap_all + vb6_cap_all + fol_cap_all + vb12_cap_all + zn_cap_all)/8) %>% 
  ungroup()

#Only wheat fortification vehicles
fort_wheat <- fort1 %>% 
  group_by(hhid, REP, UR, EXPCC, CQ11, CQ12, 
  ) %>% 
  summarise(va_ext = (va_ai)/490,
            vd_ext = (vd_ai)/10,
            thia_ext = (thia_ai+hh_fortcont_thia/afe)/0.9,
            ribo_ext = (ribo_ai+hh_fortcont_ribo/afe)/1.3,
            niac_ext = (niac_ai+hh_fortcont_niac/afe)/11,
            vb6_ext = (vb6_ai+hh_fortcont_vb6/afe)/1.3,
            fol_ext = (fol_ai+hh_fortcont_fol/afe)/250,
            vb12_ext = (vb12_ai+hh_fortcont_vb12/afe)/2,
            zn_ext = (zn_ai+hh_fortcont_zn/afe)/10.2) %>% 
  mutate(va_cap_wheat = if_else(va_ext >= 1, 1, va_ext),
         vd_cap_wheat = if_else(vd_ext >= 1, 1, vd_ext),
         thia_cap_wheat = if_else(thia_ext >= 1, 1, thia_ext),
         ribo_cap_wheat = if_else(ribo_ext >= 1, 1, ribo_ext),
         niac_cap_wheat = if_else(niac_ext >= 1, 1, niac_ext),
         vb6_cap_wheat = if_else(vb6_ext >= 1, 1, vb6_ext),
         fol_cap_wheat = if_else(fol_ext >= 1, 1, fol_ext),
         vb12_cap_wheat = if_else(vb12_ext >= 1, 1, vb12_ext),
         zn_cap_wheat = if_else(zn_ext >= 1, 1, zn_ext)) %>%
  mutate(mimi_simple_cap_wheat = (va_cap_wheat + fol_cap_wheat + vb12_cap_wheat + zn_cap_wheat)/4, 
         mimi_full_cap_wheat = (va_cap_wheat + vd_cap_wheat + thia_cap_wheat + ribo_cap_wheat + niac_cap_wheat + vb6_cap_wheat + fol_cap_wheat + vb12_cap_wheat + zn_cap_wheat)/9,
         mimi_full_cap_wheat_novd = (va_cap_wheat + thia_cap_wheat + ribo_cap_wheat + niac_cap_wheat + vb6_cap_wheat + fol_cap_wheat + vb12_cap_wheat + zn_cap_wheat)/8) %>% 
  ungroup()


#Only edible oil fortification vehicles
fort_oil <- fort1 %>% 
  group_by(hhid, REP, UR, EXPCC, CQ11, CQ12, 
  ) %>% 
  summarise(va_ext = (va_ai+hh_fortcont_va)/490,
            vd_ext = (vd_ai+hh_fortcont_vd)/10,
            thia_ext = (thia_ai)/0.9,
            ribo_ext = (ribo_ai)/1.3,
            niac_ext = (niac_ai)/11,
            vb6_ext = (vb6_ai)/1.3,
            fol_ext = (fol_ai)/250,
            vb12_ext = (vb12_ai)/2,
            zn_ext = (zn_ai)/10.2) %>% 
  mutate(va_cap_oil = if_else(va_ext >= 1, 1, va_ext),
         vd_cap_oil = if_else(vd_ext >= 1, 1, vd_ext),
         thia_cap_oil = if_else(thia_ext >= 1, 1, thia_ext),
         ribo_cap_oil = if_else(ribo_ext >= 1, 1, ribo_ext),
         niac_cap_oil = if_else(niac_ext >= 1, 1, niac_ext),
         vb6_cap_oil = if_else(vb6_ext >= 1, 1, vb6_ext),
         fol_cap_oil = if_else(fol_ext >= 1, 1, fol_ext),
         vb12_cap_oil = if_else(vb12_ext >= 1, 1, vb12_ext),
         zn_cap_oil = if_else(zn_ext >= 1, 1, zn_ext)) %>%
  mutate(mimi_simple_cap_oil = (va_cap_oil + fol_cap_oil + vb12_cap_oil + zn_cap_oil)/4, 
         mimi_full_cap_oil = (va_cap_oil + vd_cap_oil + thia_cap_oil + ribo_cap_oil + niac_cap_oil + vb6_cap_oil + fol_cap_oil + vb12_cap_oil + zn_cap_oil)/9,
         mimi_full_cap_oil_novd = (va_cap_oil + thia_cap_oil + ribo_cap_oil + niac_cap_oil + vb6_cap_oil + fol_cap_oil + vb12_cap_oil + zn_cap_oil)/8) %>% 
  ungroup()

#Create full dataset including all four scenarios
fort_base <- fort_base %>% dplyr::select(hhid, UR, contains("_ai"), contains("_cap"), contains("_base"), -mimi_simple_cap_base, -mimi_full_cap_base)
fort_oil <-fort_oil %>% dplyr::select(hhid, contains("_oil"))
fort_wheat <-fort_wheat %>% dplyr::select(hhid, contains("_wheat"))
fort_all <-fort_all %>% dplyr::select(hhid, district_code, contains("_all"))

################################################################################################
############# THIS IS THE FINAL DATA SET, DON'T NEED TO RERUN ANY CODE ABOVE HERE ##############
################################################################################################
fort <- fort_base %>% 
  #dplyr::left_join(., fort_oil, by ='hhid') %>%
  #dplyr::left_join(., fort_wheat, by ='hhid') %>% 
  dplyr::left_join(., fort_all, by ='hhid')

#Map creation loop for full MAR
fort_results <- fort %>% 
  group_by(district_code) %>% 
  summarise(mimi_full_cap_novd_base = mean(mimi_full_cap_base_novd),
            #mimi_full_cap_novd_oil = mean(mimi_full_cap_oil_novd),
            #mimi_full_cap_novd_wheat = mean(mimi_full_cap_wheat_novd),
            mimi_full_cap_novd_all = mean(mimi_full_cap_all_novd)
                                           )

map <- fort_results %>% 
  dplyr::left_join(., dcodes, by ='district_code')

shp %>% 
  dplyr::left_join(., map, by ='GID_2') %>% 
  ggplot(data = .) +
  geom_sf(aes(geometry = geometry, 
          #######Toggle here######
              fill = mimi_full_cap_novd_base), 
          ########################
          color="transparent") +
  geom_sf(fill = "transparent", color = "black", 
          data = . %>% group_by(NAME_1) %>% summarise()) +
  theme_void() +
  scale_fill_gradientn(
    #limits = c(0.58, 1.0), 
    name = "Adequacy ratio",
    colours = rev(wesanderson::wes_palette("Zissou1", 100, type = "continuous")))+
  ############And here#################
  #labs(title = 'All micronutrients (base case)') + 
  #####################################
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
  theme(plot.title = element_text(size=28, hjust = 0.5),
        legend.position="bottom")

fort_mar_adm2 <- w_data %>% 
  srvyr::group_by(CQ11.x, district_code) %>% 
  srvyr::summarise(
                   mar_base_2 = median(mimi_full_cap_base_novd)
  ) %>% dplyr::rename(adm1 = "CQ11.x")

fort_mar_adm2 %>% 
  dplyr::left_join(., fort_mar_adm1, by ='adm1') %>% 
  group_by(adm1) %>% 
  dplyr::summarise(adm1_diff = mean(mar_base_1 - mar_base_2)*100) %>% 
  arrange(desc(adm1_diff)) 

################################################################################
###########################Individual micronutrients############################
################################################################################

w_data <- fort %>% 
  dplyr::left_join(., eth_hces15_hh_info, by ='hhid') %>% 
  dplyr::left_join(., dcodes, by ='district_code') %>% 
  as_survey_design(id = ea, strata = adm1, weights = survey_wgt)

fort_indvmn_adm1 <- w_data %>% 
  srvyr::group_by(district_code) %>% 
  srvyr::summarise(va_base = median(va_cap_base),
                   va_all = median(va_cap_all),
                   
                   thia_base = median(thia_cap_base),
                   thia_all = median(thia_cap_all),
                   
                   ribo_base = median(ribo_cap_base),
                   ribo_all = median(ribo_cap_all),
                   
                   niac_base = median(niac_cap_base),
                   niac_all = median(niac_cap_all),
                   
                   vb6_base = median(vb6_cap_base),
                   vb6_all = median(vb6_cap_all),
                   
                   fol_base = median(fol_cap_base),
                   fol_all = median(fol_cap_all),
                   
                   vb12_base = median(vb12_cap_base),
                   vb12_all = median(vb12_cap_all),
                   
                   zn_base = median(zn_cap_base),
                   zn_all = median(zn_cap_all), 
                   
                   mar_base = median(mimi_full_cap_base_novd),
                   mar_all = median(mimi_full_cap_all_novd)
  ) 
  dplyr::left_join(., dcodes, by ='district_code')

map_function <- function(variable) {

  map <-  shp %>% 
    dplyr::left_join(., fort_indvmn, by ='GID_2') %>% 
    ggplot(data = .) +
    geom_sf(aes(geometry = geometry, 
                #######Toggle here######
                fill = {{variable}}), 
            ########################
            color="transparent") +
    geom_sf(fill = "transparent", color = "black", 
            data = . %>% group_by(NAME_1) %>% summarise()) +
    theme_void() +
    scale_fill_gradientn(
      limits = c(0, 1), 
      name = "Adequacy ratio",
      colours = rev(wesanderson::wes_palette("Zissou1", 100, type = "continuous")))+
    ############And here#################
  #labs(title = 'All micronutrients (base case)') + 
  #####################################
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
    theme(plot.title = element_text(size=28, hjust = 0.5),
          legend.position="none")
  
  return(map)
}

map_va_base <- map_function(va_base)
map_va_all <- map_function(va_all)
map_thia_base <- map_function(thia_base)
map_thia_all <- map_function(thia_all)
map_ribo_base <- map_function(ribo_base)
map_ribo_all <- map_function(ribo_all)
map_niac_base <- map_function(niac_base)
map_niac_all <- map_function(niac_all)
map_vb6_base <- map_function(vb6_base)
map_vb6_all <- map_function(vb6_all)
map_fol_base <- map_function(fol_base)
map_fol_all <- map_function(fol_all)
map_vb12_base <- map_function(vb12_base)
map_vb12_all <- map_function(vb12_all)
map_zn_base <- map_function(zn_base)
map_zn_all <- map_function(zn_all)

map_va <- plot_grid(map_va_base, map_va_all, ncol = 2)
map_thia <- plot_grid(map_thia_base, map_thia_all, ncol = 2)
map_ribo <- plot_grid(map_ribo_base, map_ribo_all, ncol = 2)
map_niac <- plot_grid(map_niac_base, map_niac_all, ncol = 2)
map_vb6 <- plot_grid(map_vb6_base, map_vb6_all, ncol = 2)
map_fol <- plot_grid(map_fol_base, map_fol_all, ncol = 2)
map_vb12 <- plot_grid(map_vb12_base, map_vb12_all, ncol = 2)
map_zn <- plot_grid(map_zn_base, map_zn_all, ncol = 2)


map_va_base
map_thia_base 
map_ribo_base
map_niac_base
map_vb6_base
map_fol_base
map_vb12_base
map_zn_base

################################################################################



#Socioeconomic position stratification
eth_hces1516_sep_tot <- eth_hces1516_dem %>% 
  group_by(hhid, ur) %>% 
  dplyr::summarise(TOTEXPP_AD = max(TOTEXPP_AD)) %>% 
  ungroup()

eth_hces1516_sep_urban <- eth_hces1516_sep_tot %>% 
  filter(ur=="Urban") %>% 
  mutate(res_sep = ntile(TOTEXPP_AD, 5))
eth_hces1516_sep_rural <- eth_hces1516_sep_tot %>% 
  filter(ur!="Urban") %>% 
  mutate(res_sep = ntile(TOTEXPP_AD, 5))

eth_hces1516_sep <- rbind(eth_hces1516_sep_urban, eth_hces1516_sep_rural)
eth_hces1516_sep <- eth_hces1516_sep %>% dplyr::rename(UR = ur)
eth_hces1516_sep <- eth_hces1516_sep %>% 
  dplyr::select(-TOTEXPP_AD)

###############################################################
################To create the dumbbell plots####################
###############################################################
ethfort_all <- fort_all %>% 
  dplyr::left_join(., eth_hces1516_sep, by ='hhid') %>% 
  group_by(UR
           , res_sep
           ) %>% 
  summarise(scenario = "Wheat flour & edible oil",
            mimi_full_cap_novd = median(mimi_full_cap_all_novd))

ethfort <- rbind(ethfort_base, ethfort_wheat, ethfort_oil, ethfort_all)

ethfort_ur_all <- fort_all %>% 
  dplyr::left_join(., eth_hces1516_sep, by ='hhid') %>% 
  group_by(urbrur) %>% 
  summarise(scenario = "Wheat flour & edible oil",
            mimi_full_cap_novd = median(mimi_full_cap_all_novd)) %>% 
  mutate(res_sep = "Total")

ethfort_ur <- rbind(ethfort_ur_base, ethfort_ur_wheat, ethfort_ur_oil, ethfort_ur_all)

ethfort$res_sep <- as.character(ethfort$res_sep)
ethfort <- rbind(ethfort, ethfort_ur)

ethfort$subpop <- paste(as.character(ethfort$UR),
                         as.character(ethfort$res_sep))

ethfort <- ethfort %>% mutate(subpop_lab = case_when(subpop==" Rural Total" ~ "Rural 6 (Total)",
                                                      subpop==" Rural 1" ~ "Rural 1 (Poorest)",
                                                      subpop==" Rural 2" ~ "Rural 2 (Poor)",
                                                      subpop==" Rural 3" ~ "Rural 3 (Neither poor nor wealthy)",
                                                      subpop==" Rural 4" ~ "Rural 4 (Wealthy)",
                                                      subpop==" Rural 5" ~ "Rural 5 (Wealthiest)",
                                                     subpop=="Urban Total" ~ "Urban 6 (Total)",
                                                     subpop=="Urban 1" ~ "Urban 1 (Poorest)",
                                                     subpop=="Urban 2" ~ "Urban 2 (Poor)",
                                                     subpop=="Urban 3" ~ "Urban 3 (Neither poor nor wealthy)",
                                                     subpop=="Urban 4" ~ "Urban 4 (Wealthy)",
                                                     subpop=="Urban 5" ~ "Urban 5 (Wealthiest)"))

ethfort  <- ethfort %>%
  mutate(population = if_else(subpop_lab == "Rural (Total)" |subpop_lab == "Urban (Total)", 1, 0))

ethfort_range <- ethfort %>% 
  group_by(subpop) %>% 
  summarise(mimi_low = min(mimi_full_cap_novd),
                      mimi_high = max(mimi_full_cap_novd))


ethfort_dumbell <- ethfort %>% 
  dplyr::left_join(., ethfort_range, by ='subpop')
 
ggplot(data = ethfort_dumbell) + 
  geom_pointrange(aes(x = subpop_lab, y = mimi_full_cap_novd, 
                      ymin = mimi_low, ymax = mimi_high, 
                      colour = scenario, shape = scenario
                      #, size = population
                      ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  coord_flip(ylim = c(0.3, 1)) + 
  theme(legend.position = "bottom") +
  labs(y = "Mean adequacy ratio*",
       x = "",
       caption = "* For vitamin A, thiamin, riboflavin, niacin, vitamin B6, folate, vitamin B12, & zinc"
  )

###############################################################
################To create the dumbbell plots####################
##################INDIVIDUAL MICRONUTRIENTS#####################
###############################################################
w_data <- fort_base %>% dplyr::select(-UR) %>% 
  dplyr::left_join(., eth_hces15_hh_info, by ='hhid') %>% 
  as_survey_design(id = ea, weights = survey_wgt)

ethfort_mn_base <- w_data %>% 
  srvyr::group_by(UR, res_sep) %>% 
  srvyr::summarise(scenario = "Base case",
            va = median(va_cap_base),
            thia = median(thia_cap_base),
            ribo = median(ribo_cap_base),
            niac = median(niac_cap_base),
            vb6 = median(vb6_cap_base),
            fol = median(fol_cap_base),
            vb12 = median(vb12_cap_base),
            zn = median(zn_cap_base)
            ) %>% filter(!(is.na(res_sep)))

w_data <- fort_all %>%  dplyr::select(-UR) %>% 
  dplyr::left_join(., eth_hces15_hh_info, by ='hhid') %>% 
  as_survey_design(id = ea, weights = survey_wgt)

ethfort_mn_all <- w_data %>% 
  srvyr::group_by(UR, res_sep) %>% 
  srvyr::summarise(scenario = "Fortification",
                   va = median(va_cap_all),
                   thia = median(thia_cap_all),
                   ribo = median(ribo_cap_all),
                   niac = median(niac_cap_all),
                   vb6 = median(vb6_cap_all),
                   fol = median(fol_cap_all),
                   vb12 = median(vb12_cap_all),
                   zn = median(zn_cap_all)
  ) %>% filter(!(is.na(res_sep)))

ethfort_mn <- rbind(ethfort_mn_base, ethfort_mn_all)

ethfort_mn$subpop <- paste(as.character(ethfort_mn$UR),
                        as.character(ethfort_mn$res_sep))

ethfort_mn_range <- ethfort_mn %>% 
  group_by(subpop) %>% 
  summarise(va_low = min(va),
            va_high = max(va),
            thia_low = min(thia),
            thia_high = max(thia),
            ribo_low = min(ribo),
            ribo_high = max(ribo),
            niac_low = min(niac),
            niac_high = max(niac),
            vb6_low = min(vb6),
            vb6_high = max(vb6),
            fol_low = min(fol),
            fol_high = max(fol),
            vb12_low = min(vb12),
            vb12_high = max(vb12),
            zn_low = min(zn),
            zn_high = max(zn))


ethfort_dumbell <- ethfort_mn %>% 
  dplyr::left_join(., ethfort_mn_range, by ='subpop')

dumbbell_vb6 <- ggplot(data = ethfort_dumbell) + 
  geom_pointrange(aes(x = subpop, y = vb6, 
                      ymin = vb6_low, ymax = vb6_high, 
                      colour = scenario, shape = scenario
                      #, size = population
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  coord_flip(ylim = c(0, 1)) + 
  theme(legend.position = "bottom") +
  labs(y = "Nutrient adequacy ratio",
       x = ""
  )

map_va
map_thia
map_ribo
map_niac
map_vb6
map_fol
map_vb12
map_zn

dumbbell_va
dumbbell_thia
dumbbell_ribo
dumbbell_niac
dumbbell_vb6
dumbbell_fol
dumbbell_vb12
dumbbell_zn

############################################################################  
##########################Inequities map####################################
############################################################################
  
eth_hces1516_sep_tot <- eth_hces1516_dem %>% 
    group_by(hhid, ur) %>% 
    dplyr::summarise(TOTEXPP_AD = max(TOTEXPP_AD)) %>% 
    ungroup()

eth_hces1516_districts <- fort %>% dplyr::select(hhid, district_code)

#Create zone level SEP tertiles 
eth_hces1516_sep_tot  <- eth_hces1516_sep_tot %>% 
  dplyr::left_join(., eth_hces1516_districts, by ='hhid')

eth_hces1516_sep_tot$res_dist <- as.factor(paste0(as.character(eth_hces1516_sep_tot$district_code),
                                  "_",
                                  as.character(eth_hces1516_sep_tot$ur)))

eth_hces1516_dist_sep <- eth_hces1516_sep_tot %>% 
  group_by(res_dist) %>% 
  dplyr::mutate(res_dist_sep = ntile(TOTEXPP_AD, 3)) %>% 
  ungroup() %>% 
  dplyr::select(hhid, res_dist_sep)

fort$res_dist <- as.factor(paste0(as.character(fort$district_code),
                        "_",
                        as.character(fort$UR)))
  
fort_results_equity <- fort %>% 
  dplyr::left_join(., eth_hces1516_dist_sep, by ='hhid') %>% 
  group_by(district_code, res_dist, res_dist_sep, UR) %>% 
  dplyr::summarise(mimi_full_cap_novd_base = mean(mimi_full_cap_base_novd),
            mimi_full_cap_novd_oil = mean(mimi_full_cap_oil_novd),
            mimi_full_cap_novd_wheat = mean(mimi_full_cap_wheat_novd),
            mimi_full_cap_novd_all = mean(mimi_full_cap_all_novd)) %>% 
  filter(res_dist_sep!= 2) %>% 
  mutate(res_dist_sep_cat = if_else(res_dist_sep== 1, "Poor", "Wealthy")) %>% 
  group_by(res_dist, UR, district_code) %>% 
  dplyr::summarise(mimi_base_poor = sum(if_else(res_dist_sep==1, mimi_full_cap_novd_base, 0)),
                   mimi_base_wealthy = sum(if_else(res_dist_sep==3, mimi_full_cap_novd_base, 0)),
                   mimi_oil_poor = sum(if_else(res_dist_sep==1, mimi_full_cap_novd_oil, 0)),
                   mimi_oil_wealthy = sum(if_else(res_dist_sep==3, mimi_full_cap_novd_oil, 0)),
                   mimi_wheat_poor = sum(if_else(res_dist_sep==1, mimi_full_cap_novd_wheat, 0)),
                   mimi_wheat_wealthy = sum(if_else(res_dist_sep==3, mimi_full_cap_novd_wheat, 0)),
                   mimi_all_poor = sum(if_else(res_dist_sep==1, mimi_full_cap_novd_all, 0)),
                   mimi_all_wealthy = sum(if_else(res_dist_sep==3, mimi_full_cap_novd_all, 0))
                   ) %>% 
  mutate(mimi_sepdiff_base = mimi_base_wealthy - mimi_base_poor,
         mimi_sepdiff_oil = mimi_oil_wealthy - mimi_oil_poor,
         mimi_sepdiff_wheat = mimi_wheat_wealthy - mimi_wheat_poor,
         mimi_sepdiff_all = mimi_all_wealthy - mimi_all_poor)


ggplot(data = fort_results_equity) +
  geom_density(aes(x= mimi_sepdiff_base), color = "red") +
  geom_density(aes(x= mimi_sepdiff_oil), color = "blue") +
  geom_density(aes(x= mimi_sepdiff_wheat), color = "green") +
  geom_density(aes(x= mimi_sepdiff_all), color = "pink") +
  coord_cartesian(xlim = c(-0.1, 0.4)) +
  theme_minimal()

  
fort_results_equity_map <- fort_results_equity %>% 
  dplyr::left_join(., dcodes, by ='district_code') %>% 
  dplyr::left_join(., shp, by ='GID_2')

#CREATE MAP OF CENTROIDS
centroids <- data.frame(
  GID_2 = shp$GID_2,
  geometry = st_centroid(shp$geometry)
)
centroids$X_COORDINATE <- st_coordinates(centroids$geometry)[, "X"]
centroids$Y_COORDINATE <- st_coordinates(centroids$geometry)[, "Y"]

centroids <- centroids %>% 
  dplyr::left_join(., fort_results_equity_map, by ='GID_2')

ggplot() +
  geom_sf(data = map ,aes(geometry = geometry, fill = mimi_sepdiff_all), color="black") +
  #geom_sf(fill = "transparent", color = "black", 
   #       data = . %>% group_by(NAME_1) %>% summarise()) +
  scale_fill_gradientn(
    limits = c(-0.15, 0.35), 
    name = "Adequacy ratio difference",
    colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"))+
  theme_void() +
  labs(title = 'All micronutrients (all interventions)') + 
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
  theme(plot.title = element_text(size=28, hjust = 0.5),
        legend.position="bottom")

p + geom_point(data = centroids, 
               aes(x = X_COORDINATE , y = Y_COORDINATE, size = mimi_sepdiff_base), color = "blue", shape = "circle open")


############################################################################  
##########################Inequities bubble chart###########################
############################################################################

dcodes_pop <- read.csv(here::here("./data/shapefiles/ETH_HCES1516_district_codes_populations.csv"))
dcodes_pop <- dcodes_pop %>% dplyr::rename(Region = "CQ11")
dcodes_pop <- dcodes_pop %>% dplyr::rename(population_size = "population")
dcodes_pop <- dcodes_pop %>% dplyr::rename(Residence = "UR")

dcodes_pop$res_dist <- as.factor(paste0(as.character(dcodes_pop$district_code),
                                                  "_",
                                                  as.character(dcodes_pop$Residence)))

fort_results_equity %>% 
  dplyr::left_join(., dcodes_pop, by ='res_dist') %>% 
  ggplot() + 
  geom_point(aes(x = mimi_oil_poor, 
                 y = mimi_oil_wealthy, 
                 color = Region, 
                 shape = Residence, 
                 size = population_size),
             position=position_jitter(h=0.01,w=0.01)
             ) + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
  theme_bw() +
  labs(title = 'All micronutrients (edible oil only)',
       x = "Mean adequacy ratio (Poorest tertile)",
       y = "Mean adequacy ratio (Wealthiest tertile)") 


############################################################################  
##########################Inequities Lorenz Curves##########################
############################################################################

eth_hces1516_sep_tot %>% 
  dplyr::select(-district_code, -res_dist, -ur) %>% 
  dplyr::left_join(., fort, by ='hhid') %>% 
  filter(!is.na(mimi_full_cap_base_novd)) %>% 
  group_by(district_code) %>% 
  dplyr::arrange(mimi_full_cap_base_novd) %>% 
  group_by(district_code) %>% 
  dplyr::mutate(mimi_cumperc_base = cumsum(mimi_full_cap_base_novd) / sum(mimi_full_cap_base_novd)) %>% 
  group_by(district_code) %>% 
  dplyr::mutate(mimi_popperc_base = row_number()/n()) %>% 
  #dplyr::arrange(mimi_full_cap_all_novd) %>% 
  #dplyr::mutate(mimi_cumperc_all = cumsum(mimi_full_cap_all_novd) / sum(mimi_full_cap_all_novd)) %>% 
  #dplyr::mutate(mimi_popperc_all = row_number()/n()) %>% 
  ggplot()+
  geom_line(aes(x=mimi_popperc_base, y=mimi_cumperc_base, colour = district_code)) +
  #geom_line(aes(x=mimi_popperc_all, y=mimi_cumperc_all), colour = "green")+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme(legend.position = "none")
  
eth_hces1516_sep_tot %>% 
  dplyr::select(-district_code, -res_dist, -ur) %>% 
  dplyr::left_join(., fort, by ='hhid') %>% 
  filter(!is.na(energy_ai)) %>% 
  group_by(district_code) %>% 
  dplyr::arrange(energy_ai) %>% 
  group_by(district_code) %>% 
  dplyr::mutate(mimi_cumperc_base = cumsum(energy_ai) / sum(energy_ai)) %>% 
  group_by(district_code) %>% 
  dplyr::mutate(mimi_popperc_base = row_number()/n()) %>% 
  #dplyr::arrange(mimi_full_cap_all_novd) %>% 
  #dplyr::mutate(mimi_cumperc_all = cumsum(mimi_full_cap_all_novd) / sum(mimi_full_cap_all_novd)) %>% 
  #dplyr::mutate(mimi_popperc_all = row_number()/n()) %>% 
  ggplot()+
  geom_line(aes(x=mimi_popperc_base, y=mimi_cumperc_base, colour = district_code)) +
  #geom_line(aes(x=mimi_popperc_all, y=mimi_cumperc_all), colour = "green")+
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme(legend.position = "none")


############################################################################  
##########################Cut-off for MIMI against AI#######################
############################################################################

fort %>% 
  dplyr::summarise(
    va = va_ai/490,
    thia = thia_ai/0.9,
    ribo = ribo_ai/1.3,
    niac = niac_ai/11,
    vb6 = vb6_ai/1.3,
    fol = fol_ai/250, 
    vb12 = vb12_ai/2,
    zn = zn_ai/10.2,
    mar = mimi_full_cap_base_novd
  ) %>% 
  dplyr::arrange(va) %>% 
  dplyr::mutate(va_popperc= row_number()/n()) %>% 
  dplyr::arrange(thia) %>% 
  dplyr::mutate(thia_popperc= row_number()/n()) %>% 
  dplyr::arrange(ribo) %>% 
  dplyr::mutate(ribo_popperc= row_number()/n()) %>% 
  dplyr::arrange(niac) %>% 
  dplyr::mutate(niac_popperc= row_number()/n()) %>% 
  dplyr::arrange(vb6) %>% 
  dplyr::mutate(vb6_popperc= row_number()/n()) %>% 
  dplyr::arrange(fol) %>% 
  dplyr::mutate(fol_popperc= row_number()/n()) %>% 
  dplyr::arrange(vb12) %>% 
  dplyr::mutate(vb12_popperc= row_number()/n()) %>% 
  dplyr::arrange(zn) %>% 
  dplyr::mutate(zn_popperc= row_number()/n()) %>% 
  dplyr::arrange(mar) %>% 
  dplyr::mutate(mar_popperc= row_number()/n()) %>% 
  #Capped nutrient adequacy ratio
  dplyr::mutate(va_cap= if_else(va>=1, 1, va),
                thia_cap=  if_else(thia>=1, 1, thia),
                ribo_cap= if_else(ribo>=1, 1, ribo),
                niac_cap= if_else(niac>=1, 1, niac),
                vb6_cap= if_else(vb6>=1, 1, vb6),
                fol_cap= if_else(fol>=1, 1, fol),
                vb12_cap= if_else(vb12>=1, 1,vb12),
                zn_cap= if_else(zn>=1, 1, zn)) %>% 
  dplyr::mutate(mar_nova = (thia_cap +  ribo_cap + niac_cap + vb6_cap + fol_cap + vb12_cap + zn_cap)/7) %>% 
  dplyr::arrange(mar_nova) %>% 
  dplyr::mutate(mar_nova_popperc= row_number()/n()) %>% 
  ggplot() +
  geom_line(aes(x = va_popperc, y = va_cap), colour = "orange") +
  geom_line(aes(x = thia_popperc, y = thia_cap), colour = "yellow") +
  geom_line(aes(x = ribo_popperc, y =ribo_cap ), colour = "blue") +
  geom_line(aes(x = niac_popperc, y = niac_cap), colour = "purple") +
  geom_line(aes(x = vb6_popperc, y = vb6_cap), colour = "pink") +
  geom_line(aes(x = fol_popperc, y = fol_cap), colour = "green") +
  geom_line(aes(x = vb12_popperc, y = vb12_cap), colour = "dark green") +
  geom_line(aes(x = zn_popperc, y = zn_cap), colour = "light blue") +
  geom_line(aes(x = mar_popperc, y = mar), colour = "red") +
  geom_line(aes(x = mar_nova_popperc, y = mar_nova), colour = "red", linetype = "dotted") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  labs(x = "Population percentile",
       y = "Nutrient/Mean adequacy ratio")


fort %>% 
  dplyr::summarise(
    va = va_ai/490,
    thia = thia_ai/0.9,
    ribo = ribo_ai/1.3,
    niac = niac_ai/11,
    vb6 = vb6_ai/1.3,
    fol = fol_ai/250, 
    vb12 = vb12_ai/2,
    zn = zn_ai/10.2) %>% 
  dplyr::summarise(
    va_inad = sum(va <1)/n(),
    thia_inad = sum(thia <1)/n(),
    ribo_inad = sum(ribo <1)/n(),
    niac_inad = sum(niac <1)/n(),
    vb6_inad = sum(vb6 <1)/n(),
    fol_inad = sum(fol <1)/n(),
    vb12_inad = sum(vb12 <1)/n(),
    zn_inad = sum(zn <1)/n()
  ) %>% 
  dplyr::summarise(mar_thresh = (thia_inad+ribo_inad+niac_inad+vb6_inad+fol_inad+vb12_inad+zn_inad)/8)





  