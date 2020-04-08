import pandas as pd

## load the merged dataset from "merging_all_files.py"
df = pd.read_csv('../all_merged_data.csv')
## take just the columns associated the orders and the products, along with
## the "counts" column
df = df[['user_id', 'product_id', 'date_order', 'application_term', 'limit',
    'result', 'city_id_product', 'bank_id', 'product_type_product', 
    'guarantee_type_product', 'loan_term_min', 'loan_term_max', 
	'loan_term_type', 'decision_cycle', 'loan_cycle', 'repayment_type', 
	'loan_quota_min', 'loan_quota_max', 'interest_rate_type', 
	'guarantee_required', 'standard_type_product', 'apply_num', 'fangkuan_num',
	'is_p2p', 'id', 'house_register', 'business_license', 'legal_person', 
	'married', 'car', 'income', 'house', 'tax', 'socialsecurity', 'bank', 
	'lifecost', 'early_repayment', 'penalty', 'counts']]
## a NAN in the "counts" column is associated with an order that did not have 
## information on the application quality. Therefore those are counted as 1 
## unique order
df['counts'] = df['counts'].fillna(1)
## NANs in the "penalty" and "early_repayment" columns are given -1 so that 
## they are a new category, distinguished from 0 and 1
df = df.fillna(-1)
## summing the products for all the identical products
df = df.groupby(['user_id', 'product_id', 'date_order', 'application_term', 
	'limit', 'result', 'city_id_product', 'bank_id', 'product_type_product', 
    'guarantee_type_product', 'loan_term_min', 'loan_term_max', 
	'loan_term_type', 'decision_cycle', 'loan_cycle', 'repayment_type', 
	'loan_quota_min', 'loan_quota_max','interest_rate_type', 
	'guarantee_required', 'standard_type_product', 'apply_num', 'fangkuan_num',
	'is_p2p', 'id', 'house_register', 'business_license', 'legal_person', 
	'married', 'car', 'income', 'house', 'tax', 'socialsecurity', 'bank', 
	'lifecost', 'early_repayment', 'penalty']).agg({'counts':'sum'}).\
	reset_index()

## drop duplicates, now that they are counted
df = df.drop_duplicates()

## save as a new dataframe
df.to_csv('./result_files/products_dataframe.csv', index=False)

