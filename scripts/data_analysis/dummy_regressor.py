## creating a dummy model 

import numpy as np
from sklearn.dummy import DummyRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_error
import pandas as pd


folate_target = pd.read_csv("datasets/ml_input_datasets/folate_target.csv") 
iron_target = pd.read_csv("datasets/ml_input_datasets/iron_target.csv") 
vita_target = pd.read_csv("datasets/ml_input_datasets/vita_target.csv") 
zinc_target = pd.read_csv("datasets/ml_input_datasets/zinc_target.csv") 

print(folate_target.keys()) # let's check what objects we got

Y = vita_target["inad_diff"]
X = vita_target[['land_own_share', 'ed_some_share',
       'ed_prim_share', 'ed_sec_share', 'ed_mid_share', 'ed_ssec_share',
       'ed_grad_share', 'st_share', 'sc_share', 'ptg_share',
       'inc_source_cultiv_share', 'inc_source_manlab_share',
       'inc_source_domest_share', 'inc_source_forage_share',
       'inc_source_enterpr_share', 'inc_source_beg_share',
       'inc_source_other_share', 'house_type1', 'house_type2', 'house_type3',
       'house_own1', 'house_own2', 'house_own3', 'house_own4', 'house_own5',
       'house_own6', 'secc_pov_rate_rural', 'secc_pov_rate_tend_rural',
       'lit_m', 'lit_f', 'hindu', 'muslim', 'christian', 'sikh', 'buddhist',
       'jain', 'other_rel', 'religion_ns']]


dummy_regr = DummyRegressor(strategy="mean")
dummy_regr.fit(X, Y)


predicts = dummy_regr.predict(Y)
print(predicts)
print(Y)
print('Rsquared:',dummy_regr.score(X,Y))

rmse = np.sqrt(mean_squared_error(Y, predicts))
mae = mean_absolute_error(Y, predicts)


print("RMSE:", rmse)
print("MAE", mae)
