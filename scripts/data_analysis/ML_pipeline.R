##### ML pipeline


library(dplyr)
library(tidyverse)
library(doParallel)#used 
library(tidyr)
library(ggplot2)
library(ranger)
library(caret)
library(randomForest)
library(readr)
library(glmnet)
library(plotmo)
library(tidymodels)
library(usemodels)
library(pROC)

#read in the data file with targets and covariates
path_to_data <- "./datasets/ml_input_datasets/"
# setwd(working_dir)



#split the data into training and test
data_split <- initial_split(df, prop = 0.9)
train <- training(data_split)
test <- testing(data_split)

## Random forest
fit_1 <- randomForest(
  formula = micronutrient ~.,
  data = df,
  importance = TRUE,
  proximity = TRUE
)

# names(fit_1)
plot(fit_1,
     main = "Error against number of trees")

#### tidy models
folds <- vfold_cv(df, v =5)

ranger_recipe <- #set the formula
  recipe(formula = diseaseStatus ~ ., data = df) 

ranger_spec <- #define which variables you want to tune
  rand_forest(mtry = tune(), min_n = tune(), trees = 250) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% #use a workflow to aggregate the info
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(29063)
doParallel::registerDoParallel()#runs faster
ranger_tune <-
  tune_grid(ranger_workflow, resamples = folds, grid = 10)

