import padnas as pd 
import numpy as np 
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import seaborn as sns

# after running Jen's mergeing code
# read quality cleaned table fron Annette
qual = pd.read_csv('Rong/new_rf_order_quality_df.csv')

user_final.columns	# select only a few cols from users
users = user_final[['pv_daikuan', 'pv_credit', 'date', 'pv_ask','user_id','pv_index_loan']]

# Select features deemed important from RF w.r.t result
qual_trimmed = quality_final[['user_id',\
               'user_loan_experience','user_loan_experience_n',\
               'op_type_n','op_type',\
               'loan_limit_n',\
              'user_has_car','user_has_car_n',\
              'application_limit',\
               'product_type',\
              'user_social_security','user_social_security_n',\
              'mobile_is_paid',\
              'user_income_by_card','user_income_by_card_n',\
              'col_type','col_type_n',\
              'user_work_period','user_work_period_n',\
              'is_paid',\
              'standard_type',\
              'user_age_n',\
              'spam_score',\
              'qid77','qid77_n',\
              'mobile_verify',\
              'platform',\
              'cash_receipts','cash_receipts_n',\
               'guarantee_type',\
              'mobile_quality','quality']]




# create "adjustments col" a combination of all _n cols which track anytime 
# a user changes their info when applying for a loan then dop those cols
qual_trimmed['adjustments'] = qual_trimmed['user_loan_experience_n'] +\
                                qual_trimmed['user_has_car_n'] +\
                                qual_trimmed['user_social_security_n'] +\
                                qual_trimmed['user_income_by_card_n'] +\
                                qual_trimmed['user_work_period_n'] +\
                                qual_trimmed['qid77_n']+\
                                qual_trimmed['cash_receipts_n']+\
                                qual_trimmed['user_age_n']+\
                                qual_trimmed['loan_limit_n'] +\
                                qual_trimmed['op_type_n'] +\
                                qual_trimmed['col_type_n']
qual_trimmed = qual_trimmed.drop(['user_loan_experience_n',\
                                  'user_has_car_n',\
                                 'user_social_security_n',\
                                 'user_income_by_card_n',\
                                 'user_work_period_n',\
                                 'qid77_n',\
                                 'cash_receipts_n',\
                                 'user_age_n',\
                                 'loan_limit_n',\
                                 'op_type_n',\
                                 'col_type_n'],axis=1)



# drop all NA's
qual = qual_trimmed.dropna(axis=0)

# merge quality with users (both trimmed) then with orders
users_qual = users.merge(qual, on='user_id', how='inner',suffixes=('_q', '_u'))
users_all_0 = users_qual.merge(order_train, on='user_id', how='inner',suffixes=('_uq', '_o'))

users_all = users_all_0.copy()



# these have to be dropped for kmeans
users_all = users_all.drop(['user_id','product_id','mobile_is_paid'],axis=1)

# take out all categorical variables
categorical_ = []
for col in users_all.columns:
    print('*'*50)
    print(len(users_all[col].value_counts()))
    if len(users_all[col].value_counts()) > 12:
        categorical_.append(col)


no_categorical = users_all[categorical_]
no_categorical.head()

# standardize
ss = StandardScaler()
X = ss.fit_transform(no_categorical)

# initialize kmeans
kmeans = KMeans(n_jobs=-2)

# elbow plot
wcss = []
for i in range(1, 20):
    kmeans = KMeans(n_clusters=i, init='k-means++', max_iter=300, n_init=10, random_state=0)
    kmeans.fit(X)
    wcss.append(kmeans.inertia_)
plt.plot(range(1, 20), wcss)
plt.title('Elbow Method')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()

# after choosing number of clusters fit kmeans
kmeans.set_params(n_clusters=8)
kmeans.fit(X)

# attach cluster labels to full df and see how many values are in each cluster
users_all['users_all_cluster'] = kmeans.labels_
print(users_all['users_all_cluster'].value_counts())



# get categorical varialbles
cate_0 = []
for col in users_all_0.columns:
    if len(users_all_0[col].value_counts()) < 12:
        cate_0.append(col)

# get continuous varialbes
cont_0 = list(set(users_all_0.columns.to_list()) - set(cate_0) )

# dummify 

users_dummies = pd.get_dummies(users_all_0,\
                              columns = cate_0,\
                              drop_first = True)

# Repeat above code with users_dummies instead of no_categorical

