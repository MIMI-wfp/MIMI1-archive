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
library(basemodels)
library(shapr)
library(fastshap)

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


vita_women_ml <- vita_women_ml %>% select(!c(ADM2_NAME,d_name,s_name, geometry))  %>% drop_na()
pre_process <- preProcess(vita_women_ml, 
                            method=c("range")
)
vita_women_ml <- predict(pre_process, vita_women_ml)

vita_women_ml <- vita_women_ml %>% filter(inadequate_percent!=0)



fo_target <- ggplot(data = fo_women_ml,aes(x = inadequate_percent))+
  geom_histogram(color="#69b3a2", fill ="#404080") + 
  # scale_fill_manual(values=) +
  theme_ipsum() +
  ylim(0, 35)+
  labs(
       x = "Normalised percentage inadequacy", 
       y = "count") 

ir_target <-ggplot(data = ir_women_ml,aes(x = inadequate_percent))+
  geom_histogram(color="#69b3a2", fill ="#404080") + 
  theme_ipsum() +
  ylim(0, 35)+
  labs(
       x = "Normalised percentage inadequacy", 
       y = "count") 

zn_target <-ggplot(data = zn_women_ml,aes(x = inadequate_percent))+
  geom_histogram(color="#69b3a2", fill ="#404080") + 
  # scale_fill_manual(values=) +
  theme_ipsum() +
  ylim(0, 35)+
  labs(
       x = "Normalised percentage inadequacy", 
       y = "count") 

va_target <-ggplot(data = vita_women_ml,aes(x = inadequate_percent))+
  geom_histogram(color="#69b3a2", fill ="#404080") + 
  # scale_fill_manual(values=) +
  theme_ipsum() +
  ylim(0, 35)+
  labs(
       x = "Normalised percentage inadequacy", 
       y = "count") 

taget <- ggarrange(va_target,fo_target,
                    ir_target,zn_target,
                      
                            labels = c("Vitamin A","Folate", "Iron", "Zinc"))

taget <-  annotate_figure(taget,
                                   top = text_grob("Distribution of normalised target variables \n Inadequacy prevalence: women ",color = "#404080", face = "bold", size = 14))



# summary(iron_target)
# hist(folate_target$inad_diff)
# dim(folate_target)
# 
# sum(zinc_target$inad_diff == 0)



#split the data
set.seed(123)
data_split <- initial_split(vita_women_ml, prop = 0.9)
data_train <- training(data_split)
data_test <- testing(data_split)

#fit the tidy models
folds <- vfold_cv(data_train, v =nrow(data_train))#LOOCV 

# Define the recipe for preprocessing
data_recipe <- recipe(inadequate_percent ~ ., data = data_train)# %>%
  # step_normalize(all_predictors())





# Dummy model 

dummy_model <-caret::train(inadequate_percent ~ ., data = data_train, 
                           method = dummyRegressor,
                           strategy = "mean"
                           
                          )

dummy_predict <- predict(dummy_model, data_test)
dummy_rmse <- sqrt(mean((data_test$inadequate_percent-dummy_predict)^2))
dummy_mae <- mean(abs(data_test$inadequate_percent-dummy_predict))
dummy_mape <- mean(abs(data_test$inadequate_percent-dummy_predict)/data_test$inadequate_percent)*100

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
  penalty(range = c(0, 1)),
  mixture(range = c(0,1))# Range of values to search for penalty parameter
)

# Perform hyperparameter tuning
set.seed(123)
lm_tune <- lm_workflow %>%
  tune_grid(
    resamples = folds,
    grid = 10,
    metrics = mae_metric,
    control = control_grid(save_pred = TRUE)
  )


# Get the best model based on MAPE
best_model <- select_best(lm_tune)



#plot the metric
lm_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, penalty,mixture) %>%
  pivot_longer(penalty:mixture,
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
show_best(lm_tune, metric = "rmse")
final_lm <- lm_workflow %>% 
  finalize_workflow(select_best(lm_tune, metric = "rmse"))


#fits to the training data then tests on test data
best_fit_lm <- last_fit(final_lm, data_split, metrics = mae_metric)
#error rate for the training data
collect_metrics(best_fit_lm)
collect_predictions(best_fit_lm)


#make predictions on the test case
# Predict on test set
predictions <- predict(best_fit_lm$.workflow, data_test) %>%
  bind_cols(data_test)

# y <- x <- seq(0,1,0.01)
# 
# ggplot(predictions)+
#   geom_point( aes(inad_diff, .pred))+
#   geom_abline(aes(y,x))

# Evaluate model performance on the test set
mape_lm <- mae_metric(data = predictions, truth = inadequate_percent, estimate = .pred)
mape_lm

trained_model_lm <- lm_workflow %>%
  fit(data_train)

importance_lm <- trained_model_lm %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")


# saveRDS(best_fit_lm,"datasets/ml_models/va_women_lm.RDS")
# Plot variable importance
print(importance_lm)

mean(abs(data_test$inad_diff-predictions$.pred)/data_test$inad_diff)*100

# Random Forest ################################################################
fit_1 <- randomForest(
  formula = inadequate_percent~.,
  data = ir_men_ml,
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
best_model <- select_best(ranger_tune, metric = "rmse")

ranger_tune %>% collect_metrics()

# Fit the best model on the full training set
# final_rf <- ranger_workflow %>% 
#   fit(best_model)


#plot the metric
ranger_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
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
  labs(x = NULL, y = "RMSE")

#show the best model and save it 
show_best(ranger_tune, metric = "rmse")
final_workflow_rf <- ranger_workflow %>% 
  finalize_workflow(select_best(ranger_tune, metric = "rmse"))


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
mape_rf <- mae_metric(data = predictions, truth = inadequate_percent, estimate = .pred)
mape_rf

# saveRDS(best_fit_rf,"datasets/ml_models/va_women_rf.RDS")

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
  finalize_workflow(select_best(xg_tune, metric = "rmse"))


#fits to the training data then tests on test data
best_fit_xg <- last_fit(final_xg, data_split,metrics = mae_metric)
#error rate for the training data
collect_metrics(best_fit_xg)
collect_predictions(best_fit_xg)


with(collect_predictions(best_fit_xg), plot(.pred, inadequate_percent))

#make predictions on the test case
# Predict on test set
predictions <- predict(best_fit_xg$.workflow, data_test) %>%
  bind_cols(data_test)

# Evaluate model performance on the test set
mape_xg <-  mae_metric(data = predictions, truth = inadequate_percent, estimate = .pred)
mape_xg

# saveRDS(best_fit_xg, "datasets/ml_models/va_women_xg.RDS")

importance_xg <- best_fit_xg$.workflow[[1]] %>%
  extract_fit_parsnip() %>%
  vip()
class(best_fit_xg$.workflow)

# Plot variable importance


shapr(data_train,extract_fit_parsnip( best_fit_xg$.workflow[[1]]))
 
