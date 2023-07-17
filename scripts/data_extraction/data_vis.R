# Gabriel Battcock
# background data visualisation
# Differences between mean intake between sexes 
setwd("~/Documents/LSHTM/WFP_project/MIMI")
path_to_script <- "scripts/data_extraction/"
path_to_data <- "../IND_00062/"
source(paste0(path_to_script,"functions.R"))
source(paste0(path_to_script,"data_loading.R"))#sources the functions and data

### shape file read in 
# india_adm1 <- st_read(paste0(path_to_data, "shape_files/clean_india_adm1.shp"))
# india_adm1 <- india_adm1 %>% 
#   ms_simplify(keep  =0.1, keep_shapes = T, snap = T)


summary(joined)
n(unique(joined$ADM2_NAME))


My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16)) # makes the size of labels bigger for presentations


#histograms per sex
sex_hist_vita <-   vit_a_population %>% 
  filter(sum_VITA_RAE_mcg<750) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_VITA_RAE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of mean intake: \n Vitamin A",
       x = "Mean intake (mcg)", 
       fill = "Sex")

sex_hist_vita + My_Theme

sex_hist_folate <- folate_population %>% 
  filter(sum_FOLDFE_mcg<750) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_FOLDFE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=two_colours) +
  theme_ipsum() +
  labs(title = "Distribution of mean intake: \n Folate",
       x = "Mean intake (mcg)", 
       fill = "Sex")

sex_hist_folate + My_Theme

sex_hist_iron <- iron_population %>% 
  filter(sum_IRON_mg<50) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_IRON_mg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of mean intake: \n Iron",
       x = "Mean intake (mg)", 
       fill = "Sex")

sex_hist_iron + My_Theme

sex_hist_zinc <- zinc_population %>% 
  filter(sum_ZINC_mg<50) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_ZINC_mg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Distribution of mean intake: \n Zinc",
       x = "Mean intake (mg)", 
       fill = "Sex")

sex_hist_zinc + My_Theme

#violin plots
#whole population
violin_plot <- user %>% 
  # filter(sum_VITA_RAE_mcg<1000) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = SEX, y = AGE_YEAR, fill = SEX)) +
  geom_violin(alpha = 1, position = 'dodge', show.legend = FALSE) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Age distribution of population",
       x = "Sex",
       y = "Age (years)")

violin_plot + My_Theme

# heads of household

user %>% 
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  ungroup() %>% 
  group_by(HOUSEHOLD) %>% 
  ggplot(aes(x = SEX, y = AGE_YEAR, fill = SEX)) +
  geom_violin( alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c( "#404080", "#69b3a2")) +
  theme_ipsum() +
  labs(fill="")


# household difference

vit_a_hh <- vit_a_population %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  ungroup() %>% 
  group_by(HOUSEHOLD) %>% 
  na.omit() %>% 
  filter(sum_VITA_RAE_mcg<1000) %>% 
  # mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = sum_VITA_RAE_mcg, fill = SEX)) +
  geom_histogram( color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


# Head of household difference in intake (Male - Female )


summary(vit_a_household)

vita_hh_plot <- vit_a_household %>% 
  filter(DIFF > -100 & DIFF < 100) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram( color = "#69b3a2", fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Vitamin A",
       x = "Difference in mean intake (RAE mcg)")+
  My_Theme

folate_hh_plot <- folate_household %>% 
  filter(DIFF > -100 & DIFF < 100) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Folate",
       x = "Difference in mean intake (mcg)")+
  My_Theme


vitb12_hh_plot <- vit_b12_household %>% 
  filter(DIFF > -100 & DIFF < 100) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram( color = "#69b3a2",fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Vitamin B12",
       x = "Difference in mean intake (mcg)")+
  My_Theme



iron_hh_plot <- iron_household %>% 
  filter(DIFF > -25 & DIFF < 25) %>% 
  # mutate(DIFF_NORM = DIFF/iron_hh_mean) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram(  color = "#69b3a2",fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Iron",
       x = "Difference in mean intake (mg)")+
  My_Theme



zinc_hh_plot <- zinc_household %>% 
  filter(DIFF > -25 & DIFF < 25) %>% 
  ggplot(aes(x = DIFF)) + 
  geom_histogram(color = "#69b3a2",  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Zinc",
       x = "Difference in mean intake (mg)")+
  My_Theme



figure2 <- ggarrange(vita_hh_plot, folate_hh_plot, vitb12_hh_plot,iron_hh_plot, zinc_hh_plot,  
                     ncol = 3, nrow = 2)

annotate_figure(figure2,
                top = text_grob("Difference in observed intake between head of household", color = "#404080", face = "bold", size = 14),
                bottom = text_grob("The observed intake of the oldest man in a household minus the observed intake of the older woman.", color = "#69b3a2",
                                   hjust = 1, x = 1, face = "italic", size = 10),
  
)



## two sided T-test

with(vit_a_household, t.test(SUM_MALE, SUM_FEMALE)) #not a significant difference 
with(folate_household, t.test(SUM_MALE, SUM_FEMALE)) 
with(iron_household, t.test(SUM_MALE, SUM_FEMALE))
with(zinc_household, t.test(SUM_MALE, SUM_FEMALE))


### look at distribution of BMI/ weight between adult men and women

user$BMI <- round(with(user, (WEIGHT/(HEIGHT/100)**2)),2)
user %>% 
  filter(AGE_YEAR>17) %>% 
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ggplot(aes(x = BMI, fill = SEX)) +
  geom_histogram(color="#e9ecef", alpha = 1, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

### stratify micronutrient intake by age AND sex

vit_a_population %>% 
  group_by(AGE_GROUP, SEX) %>% 
  select(!c(SUBJECT, ROUND, AGE_YEAR)) %>% 
  arrange(AGE_GROUP) %>% 
  summarise(MEAN_VITA_RAE = mean(sum_VITA_RAE_mcg)) %>% 
  ggplot(aes(x = AGE_GROUP, y = MEAN_VITA_RAE, fill = SEX)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Age group",
       y = "Mean vitamin A intake (mcg)",
       title = "Average Vitamin A intake \n by age group ",
       fill = "Sex")

folate_population %>% 
  group_by(AGE_GROUP, SEX) %>% 
  select(!c(SUBJECT, ROUND, AGE_YEAR)) %>% 
  arrange(AGE_GROUP) %>% 
  summarise(MEAN_FOLDFE = mean(sum_FOLDFE_mcg)) %>% 
  ggplot(aes(x = AGE_GROUP, y = MEAN_FOLDFE, fill = SEX)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Age group",
       y = "Mean folate intake (mcg)",
       title = "Average Folate intake \n by age group ",
       fill = "Sex")

iron_population %>% 
  group_by(AGE_GROUP, SEX) %>% 
  select(!c(SUBJECT, ROUND, AGE_YEAR)) %>% 
  arrange(AGE_GROUP) %>% 
  summarise(MEAN_IRON = mean(sum_IRON_mg)) %>% 
  ggplot(aes(x = AGE_GROUP, y = MEAN_IRON, fill = SEX)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Age group",
       y = "Mean iron intake (mg)",
       title = "Average Iron intake \n by age group ",
       fill = "Sex")

zinc_population %>% 
  group_by(AGE_GROUP, SEX) %>% 
  select(!c(SUBJECT, ROUND, AGE_YEAR)) %>% 
  arrange(AGE_GROUP) %>% 
  summarise(MEAN_ZINC = mean(sum_ZINC_mg)) %>% 
  ggplot(aes(x = AGE_GROUP, y = MEAN_ZINC, fill = SEX)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x = "Age group",
       y = "Mean zinc intake (mg)",
       title = "Average Zinc intake \n by age group ",
       fill = "Sex")

  
### state level/ adm2 level distribution

vit_a_population$ADM2_NAME <- factor(vit_a_population$ADM2_NAME)  
  
vit_a_shape <- vit_a_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(MEAN = mean(sum_VITA_RAE_mcg)) %>% 
  pivot_wider(names_from = SEX, values_from = MEAN) %>% 
  mutate(DIFF = Male - Female) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

vit_a_shape_household <- vit_a_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME, HOUSEHOLD, SEX) %>% 
  summarise(MEAN = mean(sum_VITA_RAE_mcg)) %>% 
  pivot_wider(names_from = SEX, values_from = MEAN) %>% 
  mutate(DIFF = Male - Female) %>% 
  ungroup() %>% 
  group_by(ADM2_NAME) %>% 
  summarise(MEAN_DIFF = mean(DIFF, na.rm = T)) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

vit_a_shape_head_household <- vit_a_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(HOUSEHOLD) %>% 
  arrange(HOUSEHOLD) %>% 
  mutate(MEAN = mean(sum_VITA_RAE_mcg)) %>% 
  pivot_wider(names_from = "SEX", values_from = sum_VITA_RAE_mcg) %>% 
  mutate(DIFF = Male - Female) %>% 
  ungroup() %>% 
  group_by(ADM2_NAME) %>% 
  summarise(MEAN_DIFF = mean(DIFF, na.rm = T)) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

tm_shape(st_as_sf(india_adm2))+
  tm_fill() +
  tm_shape(st_as_sf(vit_a_shape_household)) +
  tm_polygons(col = "MEAN_DIFF",
              title = "Vitamin A intake difference (Men - Women), mcg",
              style = "quantile",
              breaks = 3,
              palette = "RdYlGn",
              alpha = 1,
              lwd = 0.4,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

st_write(vit_a_shape, paste0(path_to_data, "shape_files/vit_a_shape.shp"), append = TRUE)

folate_population$ADM2_NAME <- factor(folate_population$ADM2_NAME)  

folate_shape <- folate_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(MEAN = mean(sum_FOLDFE_mcg)) %>% 
  pivot_wider(names_from = SEX, values_from = MEAN) %>% 
  mutate(DIFF = Male - Female) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

tm_shape(st_as_sf(india_adm2))+
  tm_fill() +
  tm_shape(st_as_sf(folate_shape)) +
  tm_polygons(col = "DIFF",
              title = "Vitamin A intake difference (Men - Women), mcg",
              style = "quantile",
              breaks = 3,
              palette = "RdYlGn",
              alpha = 1,
              lwd = 0.4,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

st_write(folate_shape, paste0(path_to_data, "shape_files/folate_shape.shp"), append = TRUE)

iron_population$ADM2_NAME <- factor(iron_population$ADM2_NAME)  

iron_shape <- iron_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(MEAN = mean(sum_IRON_mg)) %>% 
  pivot_wider(names_from = SEX, values_from = MEAN) %>% 
  mutate(DIFF = Male - Female) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

tm_shape(st_as_sf(india_adm2))+
  tm_fill() +
  tm_shape(st_as_sf(iron_shape)) +
  tm_polygons(col = "DIFF",
              title = "Iron intake difference (Men - Women), mg",
              style = "quantile",
              palette = "-RdYlGn",
              breaks = c(-0.65,0,2,4.7),
              alpha = 1,
              lwd = 0.4,
              n = 4,
              border.col = 1,
              legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

st_write(iron_shape, paste0(path_to_data, "shape_files/iron_shape.shp"), append = TRUE)

zinc_population$ADM2_NAME <- factor(zinc_population$ADM2_NAME)  

zinc_shape <- zinc_population %>% 
  filter(AGE_YEAR>=18) %>% 
  group_by(ADM2_NAME, SEX) %>% 
  summarise(MEAN = mean(sum_ZINC_mg)) %>% 
  pivot_wider(names_from = SEX, values_from = MEAN) %>% 
  mutate(DIFF = Male - Female) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")

st_write(zinc_shape, paste0(path_to_data, "shape_files/zinc_shape.shp"), append = TRUE)

#-------------------------------------------------------------

# using the MEAN intake for adult men and men and women,
# calculate the percentage of people
# at ADM2 level who are below the EAR threshold

# inadequacy_MN <- function(micronutrient_object){
#   Men <-  new("micronutrient",
#     EAR_men = micronutrient_object@EAR_men,
#     EAR_women = micronutrient_object@EAR_women,
#     UL = micronutrient_object@UL, 
#     data = micronutrient_object@data %>% 
#       filter(AGE_YEAR>17 & SEX == "Male"),
#     value = micronutrient_object@value
#   )
#   Women <-  new("micronutrient",
#     EAR_men = micronutrient_object@EAR_men,
#     EAR_women = micronutrient_object@EAR_women,
#     UL = micronutrient_object@UL, 
#     data = micronutrient_object@data %>% 
#       filter(AGE_YEAR>17 & SEX == "Female"),
#     value = micronutrient_object@value
#   )
#     
#   #   micronutrient_object@data %>%
#   # filter(AGE_YEAR>17 & SEX == "Male")
#   # women <- micronutrient_object@data %>%
#   # filter(AGE_YEAR>17 & SEX == "Female")
# 
#   
#   temp_men <- Men@data %>%
#     ungroup()  %>%
#     rename(SUM = Men@value) %>%
#     select(SUBJECT, SUM, ADM2_NAME)  %>% 
#     mutate(INADEQUATE = factor(ifelse(SUM<= Men@EAR_men, 1, 0))) %>%
#     group_by(ADM2_NAME) %>%
#     summarise(percentage =(length( INADEQUATE[ which( INADEQUATE == 1 ) ])/n()))  
# 
#   temp_women <- Women@data %>%
#     ungroup()  %>%
#     rename(SUM = Women@value) %>%
#     select(SUBJECT, SUM, ADM2_NAME)  %>% 
#     mutate(INADEQUATE = factor(ifelse(SUM<= Women@EAR_women, 1, 0))) %>%
#     group_by(ADM2_NAME) %>% 
#     summarise(percentage =(length( INADEQUATE[ which( INADEQUATE == 1 ) ])/n()))  
# 
#   final  <- temp_men %>% 
#     left_join(temp_women, by = "ADM2_NAME") %>% 
#     rename(percentage_men = percentage.x,
#            percentage_women = percentage.y)
# 
#   final <- final  %>% left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")
#   final
# }
# 
# vita_inad <- inadequacy_MN(VitA)
# folate_inad <- inadequacy_MN(Folate) 
# iron_inad <- inadequacy_MN(Iron)
# zinc_inad<- inadequacy_MN(Zinc)

# 
# 
# w1 <- tm_shape(st_as_sf(india_adm2))+
#   tm_fill()+
# tm_shape(st_as_sf(vita_inad)) +
#   tm_polygons(col = "percentage_men",
#               title = "Vitamin A inadequacy by district",
#               style = "quantile",
#               breaks = 3,
#               palette = "-RdYlGn",
#               alpha = 1,
#               lwd = 0.4,
#               n = 4,
#               border.col = 1,
#               legend.hist = TRUE) +
#   tm_layout(legend.outside = TRUE)
# 
# 
# w2 <- tm_shape(st_as_sf(india_adm2))+
#   tm_fill()+
#   tm_shape(st_as_sf(folate_inad)) +
#   tm_polygons(col = "percentage_men",
#               title = "Folate inadequacy by district",
#               style = "quantile",
#               breaks = 3,
#               palette = "-RdYlGn",
#               alpha = 1,
#               lwd = 0.4,
#               n = 4,
#               border.col = 1,
#               legend.hist = TRUE) +
#   tm_layout(legend.outside = TRUE)
# 
# 
# w3 <- tm_shape(st_as_sf(india_adm2))+
#   tm_fill()+
#   tm_shape(st_as_sf(iron_inad)) +
#   tm_polygons(col = "percentage_men",
#               title = "Iron inadequacy by district",
#               style = "quantile",
#               breaks = 3,
#               palette = "-RdYlGn",
#               alpha = 1,
#               lwd = 0.4,
#               n = 4,
#               border.col = 1,
#               legend.hist = TRUE) +
#   tm_layout(legend.outside = TRUE)
# 
#   w4 <- tm_shape(st_as_sf(india_adm2))+
#     tm_fill()+
#     tm_shape(st_as_sf(zinc_inad)) +
#     tm_polygons(col = "percentage_men",
#                 title = "Zinc inadequacy by district",
#                 style = "quantile",
#                 breaks = 3,
#                 palette = "-RdYlGn",
#                 alpha = 1,
#                 lwd = 0.4,
#                 n = 4,
#                 border.col = 1,
#                 legend.hist = TRUE) +
#     tm_layout(legend.outside = TRUE)
# 
#   tmap_arrange(w1, w2, w3, w4, nrow = 2)
# 
# 

  
# fortification vehicles plots
  
  
# difference in rice per household 
  
RICE_HOUSEHOLD %>% 
  ggplot(aes(x = diff_rice_g)) +
  geom_histogram(color = "#69b3a2",  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Rice consumption difference per household",
       x = "Difference in rice consumption (g)")+
  My_Theme
  
  with(RICE_HOUSEHOLD, t.test(RICE_men_g, RICE_women_g))
  
  st_write(RICE_HOUSEHOLD, paste0(path_to_data, "shape_files/rice/rice_household_sp.shp"), append = TRUE)
  
  
  
# scatter plot of difference in inadequacy per admin 2
  
  
  #######
  
  mn_target_scatter <- (vita_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
                          rename(va_w_inad = women_inad_perc, va_inad_diff = inad_diff, va_m_inad =men_inad_perc )) %>% 
    full_join((folate_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
                 rename(fo_w_inad = women_inad_perc, fo_inad_diff = inad_diff, fo_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1")) %>% 
    full_join((iron_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
                 rename(if_w_inad = women_inad_perc, if_inad_diff = inad_diff, ir_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1"))%>% 
    full_join((zinc_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
                 rename(zn_w_inad = women_inad_perc, zn_inad_diff = inad_diff, zn_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1")) %>% 
    full_join(gdqs_adm2, by = "ADM2_NAME")
  
  
  
  
  
  
  #------------------- scatter plots -------------------------------------
  
  x_labels <- c("0%", "25%", "50%", "75%", "100%")
  
  
  background <- data.frame(lower = c(-50,0), 
                           upper = c(0, 50),
                           col = c("Women dominate", "Men dominate"))
  
  
  # diff vs women 
  vita_scat <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = va_w_inad, y = va_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    xlim(0,100) +
    ylim(-50, 50) +
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  
  
  fol_scat <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = fo_w_inad, y = fo_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours)+ 
    ylim(-50, 50)+
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  
  
  iron_scat <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = if_w_inad, y = if_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    ylim(-50, 50)+
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  zin_scat <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = zn_w_inad, y = zn_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    ylim(-50, 50)+
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  # make a 
  diff_w_scatter <- ggarrange(vita_scat + rremove("ylab") + rremove("xlab"),fol_scat+ rremove("ylab") + rremove("xlab"),
                              iron_scat+ rremove("ylab") + rremove("xlab"),zin_scat+ rremove("ylab") + rremove("xlab"),
                              common.legend = TRUE,
                              labels = c("Vitamin A","Folate", "Iron", "Zinc"))
  
  diff_w_scatter <-  annotate_figure(diff_w_scatter, left = text_grob("Inadequacy sex difference (percentage)", rot = 90),
                                     bottom = text_grob("Women inadequacy percentage"),
                                     top = "Areas with high Micronutrient inadequacy in women \n 
                have a large inadequacy difference")
  
  # diff vs women 
  vita_scat_m <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = va_m_inad, y = va_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    xlim(0,100) +
    ylim(-50, 50) 
  
  
  fol_scat_m <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = fo_m_inad, y = fo_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours)+ 
    ylim(-50, 50)
  
  
  
  iron_scat_m <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = ir_m_inad, y = if_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    ylim(-50, 50)
  
  zin_scat_m <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = 0, xmax = 100, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = zn_m_inad, y = zn_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    ylim(-50, 50)
  
  # make a 
  diff_m_scatter <- ggarrange(vita_scat_m + rremove("ylab") + rremove("xlab"),fol_scat_m+ rremove("ylab") + rremove("xlab"),
                              iron_scat_m+ rremove("ylab") + rremove("xlab"),zin_scat_m+ rremove("ylab") + rremove("xlab"),
                              common.legend = TRUE,
                              labels = c("Vitamin A","Folate", "Iron", "Zinc"))
  
  diff_m_scatter <-  annotate_figure(diff_m_scatter, left = text_grob("Inadequacy sex difference (percentage)", rot = 90),
                                     bottom = text_grob("Men inadequacy percentage"))
  
  
  
  
  # spearman's rank correlation - women vs 
  with(mn_target_scatter, cor.test(rank(va_inad_diff),rank(va_w_inad), method = "spearman"))
  with(mn_target_scatter, cor.test(rank(fo_w_inad),rank(fo_inad_diff), method = "spearman"))
  with(mn_target_scatter, cor.test(rank(if_w_inad),rank(if_inad_diff), method = "spearman"))
  with(mn_target_scatter, cor.test(rank(zn_w_inad),rank(zn_inad_diff), method = "spearman"))
  
  
  ### looking at gdqs against the mn differece
  
  vita_gdqs <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = -.5, xmax = 0.5, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = mean, y = va_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    ylim(-50, 50) +
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  folate_gdqs <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = -.5, xmax = 0.5, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = mean, y = fo_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    ylim(-50, 50) +
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  iron_gdqs <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = -.5, xmax = 0.5, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = mean, y = if_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    ylim(-50, 50) +
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  zinc_gdqs <- mn_target_scatter %>% 
    ggplot() + 
    geom_rect(data = background, aes(xmin = -.5, xmax = 0.5, ymin = lower, ymax = upper, fill = col), alpha = 0.2, show.legend = TRUE) +
    geom_point(aes(x = mean, y = zn_inad_diff, color = ADM1)) + 
    theme_ipsum() +
    scale_color_manual(values = my_colours) + 
    
    # geom_ribbon(aes(ymin=-50,ymax=0), alpha=0.25, show.legend = FALSE)+
    ylim(-50, 50) +
    guides(color=guide_legend(title="State"),
           fill=guide_legend(title="Inadequacy sex difference"))
  
  diff_gdqs_scatter <- ggarrange(vita_gdqs + rremove("ylab") + rremove("xlab"),folate_gdqs+ rremove("ylab") + rremove("xlab"),
                                 iron_gdqs + rremove("ylab") + rremove("xlab"),zinc_gdqs+ rremove("ylab") + rremove("xlab"),
                                 common.legend = TRUE,
                                 labels = c("Vitamin A","Folate", "Iron", "Zinc"))
  
  diff_gdqs_scatter <-  annotate_figure(diff_gdqs_scatter, left = text_grob("Inadequacy sex difference (percentage)", rot = 90),
                                        bottom = text_grob("Mean difference in GDQS"))
  

  
  