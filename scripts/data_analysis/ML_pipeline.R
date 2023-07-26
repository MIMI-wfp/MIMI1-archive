##### ML pipeline

library(dplyr)
library(tidyverse)
library(doMC)
registerDoMC(cores = 4)
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
library(vip)
library(coefplot)
library(xgboost)
library(DiagrammeR) 
library(parsnip)
library(shapr)

#read in the data file with targets and covariates
path_to_data <- "./datasets/ml_input_datasets/"
# setwd(working_dir)
load("datasets/ml_input_datasets/vita_men_ml.RData")
load("datasets/ml_input_datasets/fo_men_ml.RData")
load("datasets/ml_input_datasets/ir_men_ml.RData")
load("datasets/ml_input_datasets/zn_men_ml.RData")
load("datasets/ml_input_datasets/vita_women_ml.RData")
load("datasets/ml_input_datasets/fo_women_m.RData")
load("datasets/ml_input_datasets/ir_women_ml.RData")
load("datasets/ml_input_datasets/zn_women_ml.RData")
load("datasets/ml_input_datasets/vita_target_bin.RData")
load("datasets/ml_input_datasets/folate_target_bin.RData")
load("datasets/ml_input_datasets/iron_target.RData")
load("datasets/ml_input_datasets/zinc_target.RData")


#split the data into training and test
folate_target <- folate_target %>% select(!c(d_name, geometry))  %>% drop_na()
pre_process <- preProcess(folate_target, 
                            method=c("range")
                            )

folate_target <- predict(pre_process, folate_target)
folate_target <- folate_target %>% filter(inad_diff!=0)
write.csv(folate_target, "datasets/ml_input_datasets/folate_target_bin.csv")

summary(folate_target)
hist(folate_target$inad_diff)
dim(folate_target)

sum(folate_target$inad_diff == 0)



#split the data
set.seed(123)
data_split <- initial_split(folate_target, prop = 0.9)
data_train <- training(data_split)
data_test <- testing(data_split)

#fit the tidy models
folds <- vfold_cv(folate_target, v =nrow(data_train))#LOOCV 

# Define the recipe for preprocessing
data_recipe <- recipe(inad_diff ~ ., data = data_train)# %>%
  # step_normalize(all_predictors())





# Linear regression ############################################################


# Define the workflow with linear regression model
lm_model <- linear_reg(
  penalty = tune(),     # Regularization parameter (tuning parameter)
  mixture =  tune()          # Elastic Net mixing parameter
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define the performance metric
mae_metric <- metric_set(yardstick::mae,rmse, yardstick::mape)

# Define the workflow
lm_workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(lm_model)

# Define the parameter grid for hyperparameter tuning
param_grid <- grid_regular(
  penalty(range = c(0, 1))  # Range of values to search for penalty parameter
)

# Perform hyperparameter tuning
set.seed(123)
lm_tune <- lm_workflow %>%
  tune_grid(
    resamples = folds,
    grid = 20,
    metrics = mae_metric,
    control = control_grid(save_pred = TRUE)
  )


# Get the best model based on MAPE
best_model <- select_best(lm_tune, metric = "mae")



#plot the metric
lm_tune %>%
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  select(mean, penalty) %>%
  pivot_longer(penalty,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  theme_ipsum() +
  scale_color_manual(values = my_colours) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "MAE")

#show the best model and save it 
show_best(lm_tune, metric = "mape")
final_lm <- lm_workflow %>% 
  finalize_workflow(select_best(lm_tune, metric = "mae"))


#fits to the training data then tests on test data
best_fit_lm <- last_fit(final_lm, data_split, metrics = mae_metric)
#error rate for the training data
collect_metrics(best_fit_lm)
collect_predictions(best_fit_lm)


#make predictions on the test case
# Predict on test set
predictions <- predict(best_fit_lm$.workflow, data_test) %>%
  bind_cols(data_test)

# Evaluate model performance on the test set
mape_lm <- mae_metric(data = predictions, truth = inad_diff, estimate = .pred)
mape_lm

trained_model_lm <- lm_workflow %>%
  fit(data_train)

importance_lm <- trained_model_lm %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")

# Plot variable importance
print(importance_lm)


# Random Forest ################################################################
fit_1 <- randomForest(
  formula = inad_diff~.,
  data = folate_target,
  importance = TRUE,
  proximity = TRUE
)

# names(fit_1)
plot(fit_1,
     main = "Error against number of trees")




# Define the workflow with random forest model
rf_model <- rand_forest(
  mtry = tune(),         # Number of predictor variables to sample at each split (tuning parameter)
  trees = 250,           # Number of trees to grow
  min_n = tune() # Minimum number of samples in each terminal node
  # Maximum number of iterations allowed
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Define the performance metric
mae_metric <- metric_set(yardstick::mae,rmse,yardstick::mape)

# Define the workflow
ranger_workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_model)


# Perform hyperparameter tuning
ranger_tune <- ranger_workflow %>%
  tune_grid(
    resamples = folds,
    grid = 20,
    metrics = mae_metric,
    control = control_grid(save_pred = TRUE)
  )

# Get the best model based on MAPE
best_model <- select_best(ranger_tune, metric = "mae")

ranger_tune %>% collect_metrics()

# Fit the best model on the full training set
# final_rf <- ranger_workflow %>% 
#   fit(best_model)


#plot the metric
ranger_tune %>%
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  theme_ipsum() +
  scale_color_manual(values = my_colours) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "MAE")

#show the best model and save it 
show_best(ranger_tune, metric = "mae")
final_workflow_rf <- ranger_workflow %>% 
  finalize_workflow(select_best(ranger_tune, metric = "mae"))


#fits to the training data then tests on test data
best_fit_rf <- last_fit(final_workflow_rf, data_split, metrics = mae_metric)
#error rate for the training data
collect_metrics(best_fit_rf)
collect_predictions(best_fit_rf)

extract_workflow(best_fit_rf)

#make predictions on the test case
# Predict on test set
predictions <- predict(best_fit_rf$.workflow, data_test) %>%
  bind_cols(data_test)

# Evaluate model performance on the test set
mape_rf <- mae_metric(data = predictions, truth = inad_diff, estimate = .pred)
mape_rf
4two_colours

importance_rf <- best_fit_rf$.workflow[[1]] %>% 
  extract_fit_parsnip() %>% 
  vip(mapping = aes(),
    aesthetics = list(colour = two_colours[2],
                      fill = two_colours[1]))

# Plot variable importance
print(importance_rf)


# xgBoost -----------------------------------

# Split the data into training and testing sets


# Define the recipe for preprocessing
# data_recipe <- recipe(inad_diff ~ ., data = folate_target) %>%
#   step_normalize(all_predictors())

# Define the workflow with xgboost model
xgb_model <- boost_tree(
  trees = tune(),         # Number of trees to grow
  tree_depth = tune(),      # Maximum tree depth
  mtry = tune()             # Number of predictor variables to sample at each split
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Define the performance metric
mae_metric <- metric_set(yardstick::mae,rmse, yardstick::mape)

# Define the workflow
xg_workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(xgb_model)

# Define the parameter grid for hyperparameter tuning
# param_grid <- grid_max_entropy(
#   trees(),
#   tree_depth(),
#   mtry()
# )

# Perform hyperparameter tuning
xg_tune <- xg_workflow %>%
  tune_grid(
    resamples = folds,
    grid = 20,
    metrics = mae_metric,
    control = control_grid(save_pred = TRUE)
  )

# Get the best model based on MAPE
best_model <- select_best(xg_tune, metric = "mae")
xg_tune %>% show_best()
# Fit the best model on the full training set
# final_xg <- xg_workflow %>% 
#   fit(best_model)


#plot the metric
xg_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, tree_depth, mtry, trees) %>%
  pivot_longer(c(tree_depth,mtry,trees),
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  theme_ipsum() +
  scale_color_manual(values = my_colours) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

#show the best model and save it 
show_best(xg_tune, metric = "rmse")
final_xg <- xg_workflow %>% 
  finalize_workflow(select_best(xg_tune, metric = "mae"))


#fits to the training data then tests on test data
best_fit_xg <- last_fit(final_xg, data_split,metrics = mae_metric)
#error rate for the training data
collect_metrics(best_fit_xg)
collect_predictions(best_fit_xg)


#make predictions on the test case
# Predict on test set
predictions <- predict(best_fit_xg$.workflow, data_test) %>%
  bind_cols(data_test)

# Evaluate model performance on the test set
mape_xg <- mae_metric(data = predictions, truth = inad_diff, estimate = .pred)
mape_xg


importance_xg <- best_fit_xg$.workflow[[1]] %>%
  extract_fit_parsnip() %>%
  vip()
class(best_fit_xg$.workflow)

# Plot variable importance


shapr(data_train,extract_fit_parsnip( best_fit_xg$.workflow[[1]]))

