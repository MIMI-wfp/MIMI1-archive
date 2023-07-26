## creating a dummy model 

import numpy as np
from sklearn.dummy import DummyRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_error, mean_absolute_percentage_error
import pandas as pd



folate_target = pd.read_csv("~/Documents/LSHTM/WFP_project/MIMI/datasets/ml_input_datasets/folate_target_bin.csv") 
# iron_target = pd.read_csv("data/datasets/ml_input_datasets/iron_target.csv") 
# vita_target = pd.read_csv("datasets/ml_input_datasets/vita_target.csv") 
# zinc_target = pd.read_csv("datasets/ml_input_datasets/zinc_target.csv") 

# print(folate_target.keys()) # let's check what objects we got

Y = folate_target["inad_diff"]
X = folate_target.drop(columns=["inad_diff"])



dummy_regr = DummyRegressor(strategy="mean")
dummy_regr.fit(X, Y)


predicts = dummy_regr.predict(Y)
print(predicts)
print(Y)
print('Rsquared:',dummy_regr.score(X,Y))

rmse = np.sqrt(mean_squared_error(Y, predicts))
mae = mean_absolute_error(Y, predicts)
mape = mean_absolute_percentage_error(Y, predicts)

print("RMSE:", rmse)
print("MAE", mae)
print("MAPE:", mape)
