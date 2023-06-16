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




######################## export datasets #####################################

save(vita_target, file = paste0(path_to_datasets, "vita_target.RData"))
save(folate_target, file = paste0(path_to_datasets, "folate_target.RData"))
save(iron_target, file = paste0(path_to_datasets, "iron_target.RData"))
save(zinc_target, file = paste0(path_to_datasets, "zinc_target.RData"))



#######

mn_target_scatter <- (vita_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
                        rename(va_w_inad = women_inad_perc, va_inad_diff = inad_diff, va_m_inad =men_inad_perc )) %>% 
  full_join((folate_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
               rename(fo_w_inad = women_inad_perc, fo_inad_diff = inad_diff, fo_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1")) %>% 
  full_join((iron_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
               rename(if_w_inad = women_inad_perc, if_inad_diff = inad_diff, ir_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1"))%>% 
  full_join((zinc_target %>% select(ADM2_NAME, women_inad_perc,men_inad_perc,inad_diff, ADM1) %>% 
               rename(zn_w_inad = women_inad_perc, zn_inad_diff = inad_diff, zn_m_inad =men_inad_perc)), by  = c("ADM2_NAME","ADM1")) 
  



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





