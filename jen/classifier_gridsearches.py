import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from joblib import dump

## script for running grid searches on random forest classifiers on the 
## "result" feature. One classifier includes the "counts" column, the other
##  includes the "apply_num" column

df = pd.read_csv('./result_files/products_dataframe.csv')

rfc = RandomForestClassifier()

## drop the categorical variables that seem to hold limited info
features_counts = df.drop(['user_id','product_id','bank_id',
                           'city_id_product','apply_num', 'result'], axis=1)
results = df['result']

## set up gridsearch
gridparams_rfc = {'max_features': range(12,20,2), 
               'n_estimators': range(350,551,20)}   
gridsearch_rfc = GridSearchCV(rfc, gridparams_rfc, cv=5, 
                           scoring='balanced_accuracy', n_jobs=-1)

## run the grid search
gridsearch_rfc.fit(features_counts, results)
## view the best parameters and score
print(gridsearch_rfc.best_params_)
print(gridsearch_rfc.best_score_)

## refit the best estimator and save it for future use
best_est = gridsearch_rfc.best_estimator_
best_est.fit(features_counts, results)
print(best_est.score(features_counts, results))
dump(best_est, 'randomforestclassifier_counts.joblib')

## rerun grid search with the "apply_num" column and no "counts" column
features_applynum = df.drop(['user_id','product_id','bank_id',
                             'city_id_product', 'counts', 'result'], axis=1)
gridsearch_rfc.fit(features_applynum, results)
print(gridsearch_rfc.best_params_)
print(gridsearch_rfc.best_score_)

## refit the best estimator and save it
best_est = gridsearch_rfc.best_estimator_
best_est.fit(features_applynum, results)
print(best_est.score(features_applynum, results))
dump(best_est, 'randomforestclassifier_applynum.joblib')


