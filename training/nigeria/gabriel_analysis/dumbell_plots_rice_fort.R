
rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign","radiant")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# ------------------------------------------------------------------------------
kevin_nga <- readxl::read_xlsx(here::here("training/nigeria/gabriel_analysis/nga_dumbbell_rice.xlsx"))


kevin_nga <- kevin_nga %>% 
  
  select(scenario,res,res_quintile,fol,vb12,fe,zn) %>% 
  mutate(type = ifelse(res == "Total", "Total",
                       paste(res,res_quintile))) %>% 
  pivot_longer(cols = c(fol,vb12,fe,zn))

nga_fort_range <- kevin_nga %>% 
  group_by(type, name, res_quintile) %>% 
  summarise(nga_low = min(value),
            nga_high = max(value))

ngafort_dumbell <- kevin_nga %>% 
  dplyr::left_join( nga_fort_range, by =c('type',"name", "res_quintile")) %>% 
  mutate(type = factor(type, levels = c("Total","Rural Poorest","Rural Poor", "Rural Middle",
                                    "Rural Wealthy", "Rural Wealthiest","Urban Poorest", 
                                    "Urban Poor", "Urban Middle",
                                    "Urban Wealthy", "Urban Wealthiest")))
  


ngafort_dumbell %>% 
  filter(name == "fol") %>% 
  ggplot() +
  geom_pointrange(aes(x = type, y = value, 
                      ymin = nga_low, ymax = nga_high, 
                      colour = scenario,
                      shape = res
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  # coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Folate"
  )

ngafort_dumbell %>% 
  filter(name == "vb12") %>% 
  ggplot() +
  geom_pointrange(aes(x = type, y = value, 
                      ymin = nga_low, ymax = nga_high, 
                      colour = scenario,
                      shape = res
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  # coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Vitamin B12"
  )

ngafort_dumbell %>% 
  filter(name == "fe") %>% 
  ggplot() +
  geom_pointrange(aes(x = type, y = value, 
                      ymin = nga_low, ymax = nga_high, 
                      colour = scenario,
                      shape = res
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  # coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Iron"
  )

ngafort_dumbell %>% 
  filter(name == "zn") %>% 
  ggplot() +
  geom_pointrange(aes(x = type, y = value, 
                      ymin = nga_low, ymax = nga_high, 
                      colour = scenario,
                      shape = res
  ))+ 
  theme_bw() +
  #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
  # coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Zinc"
  )


# ------------------------------------------------------------------------------
# lollipop plots

lolipop <- read.csv(here::here("training/nigeria/gabriel_analysis/inad_base_state_sac.csv"))

lolipop %>% 
  
  pivot_longer(cols = c(fol_prev, vb12_prev,zn_prev,fe_prev)) %>%
  
  ggplot(aes(x = state, y = value, color = name, group = zone))+
  geom_segment( aes(x=state, , xend=state, y=0, yend=value) ) +
  geom_point(size=4)

