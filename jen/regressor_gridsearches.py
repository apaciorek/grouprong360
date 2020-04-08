import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
from joblib import dump

## script for running grid searches on random forest regressors. One regressor
## is on the "counts" column, the other is on the "apply_num" column

# load the data
df = pd.read_csv('products_dataframe.csv')

# features: remove the categorical variables that do not contain much info
features = df.drop(['user_id','product_id','bank_id', 'city_id_product', 
                    'apply_num', 'counts'], axis=1)
## two different targets
counts_target = df['counts']
applynum_target = df['apply_num']

## set up the grid search
rfr = RandomForestRegressor()
gridparams_rfr = {'max_features': range(6,19,4),
                  'n_estimators': range(90,211,30)} 
gridsearch_rfr = GridSearchCV(rfr, gridparams_rfr, cv=5, 
                           scoring='r2', n_jobs=-1)

## run the grid search on the "counts" target
gridsearch_rfr.fit(features, counts_target)
print(gridsearch_rfr.best_params_)
print(gridsearch_rfr.best_score_)

# get the best estimator, fit it, and save it for future use
best_est = gridsearch_rfr.best_estimator_
best_est.fit(features, counts_target)
dump(best_est, 'randomforestregressor_counts.joblib')

## rerun the gridsearch with the "apply_num" target
gridsearch_rfr.fit(features, applynum_target)
print(gridsearch_rfr.best_params_)
print(gridsearch_rfr.best_score_)

## refit the best estimator and save it for future it
best_est = gridsearch_rfr.best_estimator_
best_est.fit(features, applynum_target)
dump(best_est, 'randomforestregressor_applynum.joblib')

