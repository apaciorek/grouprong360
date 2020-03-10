import pandas as pd
import numpy as np

## this is a script for merging the tables into one

## load the tables
order_train = pd.read_table('../order_train.txt')
print('size of order_train:', order_train.shape)
product_final = pd.read_table("../product.final.txt")
print('size of product.final:', product_final.shape)
user_final = pd.read_table("../user.final.txt")
print('size of user.final:', user_final.shape)
quality_final = pd.read_table("../quality.final.txt")
print('size of quality.final:', quality_final.shape)

## the first three are easy to use a left-join with the training data
joined = order_train.merge(product_final, on='product_id', how='left')
print('size of order_train left-joined with product.final:', joined.shape)
joined = joined.merge(user_final, on='user_id', how='left', 
    suffixes=('_order', '_user'))
print('size when again left-joined with user.final:', joined.shape)

## quality_final has a lot of extra rows and duplicate rows
user_ids = joined['user_id'].unique()
quality_final = quality_final.loc[quality_final['user_id'].isin(user_ids)]

## the .pivot_table() method used later does not handle NaNs, so replace
## I do this column-by-column because if I try to use fillna() on the
## whole dataframe, my computer freezes
for col in quality_final.columns:
    quality_final[col] = quality_final[col].fillna('meowmeowmeow')

## the pivot_table() method returns the counts of the duplicates
counts = quality_final.pivot_table(index=quality_final.columns.tolist(), 
    aggfunc='size')

## drop the duplicates
quality_final = quality_final.drop_duplicates()

## sort the dataframe so that it matches the 'counts' Series
quality_final = quality_final.sort_values(by=quality_final.columns.tolist())

## create new column with the counts
quality_final['counts'] = counts.values

## remove the placeholder strings and replace with NaNs again
for col in quality_final.columns:
    quality_final[col] = quality_final[col].replace(to_replace='meowmeowmeow', 
        value=np.nan)
print('size of quality.final after processing:', quality_final.shape)

joined = joined.rename(columns={'term': 'application_term'})

totaldf = joined.merge(quality_final, on=['user_id', 'bank_id', 
    'application_term'], how='left', suffixes=('_product','_quality'))

print('size of total dataframe:', totaldf.shape)
totaldf.to_csv('../all_merged_data.csv', index=False)


