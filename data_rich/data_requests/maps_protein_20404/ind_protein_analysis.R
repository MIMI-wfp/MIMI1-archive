#########################################
#        Protein/Amino acid       #
#########################################

# Author: Gabriel Battcock
# Created: 25 Apr 24
# Last updated: 25 Apr 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# call the india mnapping functions
source(here::here("data_rich/India/src/preparation/india_lsff/ind_mapping.R"))

# ------------------------------------------------------------------------------

# maps for maps
ind_micronutrient_maps('adm2', lysine_g)+
  tm_layout(main.title = "Lysine", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', protein_g)+
  tm_layout(main.title = "Protein", frame = F,
            main.title.size = 0.8)

ind_micronutrient_maps('adm2', tryptophan_g ) +
  tm_layout(main.title = "Tryptophan", frame = F,
            main.title.size = 0.8)



