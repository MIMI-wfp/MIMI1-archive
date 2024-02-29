###############################
#      MIMI-EPHI training     # 
# MN intake and distribution  #
###############################

# call the useful libraries

# install.packages("pacman")

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign", "survey")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

###############################################################################
# read in data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets directory to where this is saved

hices1516 <- read_dta("data/hices1516.dta")


# ------------------------------------------------------------------------------

# Look at folate as an example 
hices1516 %>% 
  ggplot(aes(x = folate_mcg)) + 
  geom_histogram() +
  xlim(0,2000)

summary(hices1516$folate_mcg)


# look at the distributions for different subpopulations

# residence
hices1516 %>% 
  ggplot(aes(x = folate_mcg, fill = res)) + 
  geom_histogram(position = 'dodge') +
  xlim(0,2000) + 
  labe

# split into urban and rural
urban <- hices1516 %>% 
  filter(res == "Urban")

rural <- hices1516 %>% 
  filter(res == " Rural")

## look at the quartiles, is there a large distribution, which are higher and lower

quantile(hices1516$folate_mcg, probs = seq(0,1,0.25))
quantile(urban$folate_mcg)
quantile(rural$folate_mcg)

# we see a right skew for the intake distributions 

summary.table(hices1516)


### Agregating the data --------------------------------------------------------
# aggregating the data requires the use of survey weights


# look at the means in regions
hices1516 %>% 
  srvyr::as_survey_design(id = hhid, strata = adm1,
                          weights = survey_wgt, nest=T) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
     survey_mean(folate_mcg, vartype = "ci"))

hices1516%>% 
  srvyr::as_survey_design(id = hhid, strata = res,
                          weights = survey_wgt, nest=T) %>% 
  srvyr::group_by(res) %>% 
  srvyr::summarise(
    survey_mean(folate_mcg, vartype = "ci"))

hices1516%>% 
  srvyr::as_survey_design(id = hhid, strata = ,
                          weights = survey_wgt, nest=T) %>% 
  srvyr::group_by(adm1,res) %>% 
  srvyr::summarise(
    survey_mean(folate_mcg, vartype = "ci")
    )
    

# look at socio-economic position


# education head

# children under 5





# introducing the distribution in relation to the H-AR
# not yet calculating prevalence 


hices1516 %>% 
  ggplot(aes(x = folate_mcg, fill = res)) + 
  geom_histogram(position = 'dodge') +
  geom_vline(xintercept = folate_har)+
  xlim(0,2000) 

# compare 
