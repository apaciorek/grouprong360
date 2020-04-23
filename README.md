# grouprong360

# Introduction

Rong360 is a Chinese fintech company that acts as a platform for connecting banks with loan seekers.

# Analysis

Rong360FinTechCapstoneProjectPitch.ipynb for an in depth description of the Rong360 platform and outline of research questions. Our analysis focused on DA questions 3 and 4: a product popularity and customer background analysis; and identifying the driving factors of higher approval rates.

DataDescription.pages for descriptions of the available data sets 

SlidesRong360.pdf for a presentation of the results of our analysis

# Lilliana 
for a collection of Lilliana’s contributions 
<br>
Kmeans_setup.py : a script for running kmeans on only the continuous data first then dummifying all categorical variables 

# Jen
for a collection of Jen’s contributions

Merging_data_files.py : a script for merging all the data tables into one new table, removing duplicates from the “quality” table and instead adding a “counts” column to count those duplicates

EDA.ipynb : Some EDA, including contingency tables of features on the “results” column

Features_Products.ipynb : This script looks into the loan product data, how to impute the missing values, results of some random forests, and finally the k-means and PCA to see the product groupings. Before running this, you need to run the following three scripts. The gridsearch scripts may take a while, which is why they are run separately of the jupyter notebook.

Make_product_dataframe.py: This simply takes the results of the “merging_data_files.py” script and only keeps the product data and the “counts” column, and saves this as its own dataframe.

Classifier_gridsearches.py: This script runs grid searches of random forest classifers on the “result” column, and saves the best estimators.

Regressor_gridsearches.py: This script runs grid searches of random forest regressors on the “apply_num” and “counts” columns, and saves the best estimators. 

# Annette 
for a collection of Annette’s contributions 

Visuals_userorder.ipynb, visuals_qualityorder_nobuckets.ipynb, visuals_qualityorder.ipynb, visuals_normalized_qualityorders.ipynb for visualizations of user activity on Rong360 platform and application data with respect to loan approval

user_featureimportance_randomforest.R, randomforest_featureimportance_userorder.ipynb, randomforest_featureimportance_qualityorder.ipynb, quality_featureimportance_randomforest_pre_cleaning.R for random forests trained to rank feature importance of application data and user website activity on approval rate

python_to_R_translate.R for EDA done by Jen and Lilliana written in R

normalizedqualityorder.R for visualizations with outliers removed

Untitled.ipynb for EDA in python

Kmeans_users.ipynb and kmeans_quality.ipynb for a naive and very bad first go at k means clustering. See Jen and Lilliana’s folders for better implementation.


