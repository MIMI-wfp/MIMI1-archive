## Dichotomised variables


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


# nutrient adequacy ratio = actual intake/recommended intake (capped at 100% or 1)


va_men$NAR <- ifelse(va_men$mean/vita_EAR_men_mcg>=1, 1, va_men$mean/vita_EAR_men_mcg)
fo_men$NAR <- ifelse(fo_men$mean/folate_EAR_men_mcg>=1, 1, fo_men$mean/folate_EAR_men_mcg)
ir_men$NAR <- ifelse(ir_men$mean/iron_EAR_men_mg>=1, 1, ir_men$mean/iron_EAR_men_mg)
zn_men$NAR <- ifelse(zn_men$mean/zinc_EAR_men_mg>=1, 1, zn_men$mean/zinc_EAR_men_mg)
va_women$NAR <- ifelse(va_women$mean/vita_EAR_men_mcg>=1, 1, va_women$mean/vita_EAR_men_mcg)
fo_women$NAR <- ifelse(fo_women$mean/folate_EAR_women_mcg>=1, 1, fo_women$mean/folate_EAR_women_mcg)
ir_women$NAR <- ifelse(ir_women$mean/iron_EAR_women_mg>=1, 1, ir_women$mean/iron_EAR_women_mg)
zn_women$NAR <- ifelse(zn_women$mean/zinc_EAR_women_mg>=1, 1, zn_women$mean/zinc_EAR_women_mg)







