setwd("~/Downloads/Rong360capstoneproject/annette")
library(dplyr)
library(stringr)
library(ggplot2)

# reading in Jen's full merged dat a set - with duplicates removed

all_merged <- read.csv('../all_merged_data.csv')

# reading in original data sets

product_df <- read.delim("../product.final.txt")
quality_df <- read.delim("../quality.final.txt")
user_df <- read.delim("../user.final.txt")
order_train <- read.delim("../order_train.txt")

# writing original data sets to csv - may come in handy later

write.csv(product_df, 'product_df.csv', row.names = F)
write.csv(quality_df, 'quality_df.csv')
write.csv(user_df, 'user_df.csv')
write.csv(order_train, 'order_train.csv')


# exploring column names for merging
# trying to recreate Lilliana's python code

names(product_df)
names(order_train)
names(user_df)

# first joining product and order data frames
prod_and_ord <- inner_join(product_df, order_train, by = "product_id")

# now joining the above with user data frame
prod_ord_user <- inner_join(prod_and_ord, user_df, by = "user_id")

# selecting a subset of columns from the merged data frame
prod_lil <- prod_ord_user %>%
  select(., apply_num, pv, product_id, loan_cycle, limit, result)

# finding total number of page views, nunber of applications, and 
# average approval rate per loan product

lil <- prod_lil %>%
  group_by(product_id, apply_num) %>%
  summarise(total_pv = sum(pv),
            av_result = mean(result))

# creating column result_rate
# if loan product is approved less than 50% of the time - low
# otherise classified as high

lil <- lil %>%
  mutate(., result_rate = ifelse(av_result <.5, "low", "high"))

# plotting total pvs versus number of applications
# and faceting by approval rates - high or low

ggplot(lil, aes(x = total_pv, y = apply_num, color = result_rate)) +
  geom_point()
#+ facet_wrap(~ result_rate)

# want to investigate some of the data points further

low_extremes <- lil %>%
  filter(., apply_num > 5000)

# investigating a certain loan product with high application number and low pv
curious_1 <- all_merged %>%
  filter(., product_id == "e721a54a8cf18c8543d44782d9ef681f")



# what if we use all_merged data instead as our jumpoff point
# same code as above but using all_merged 
# instead of Lilliana's merging

user_lil <- all_merged %>%
  select(., user_id, 
         product_id, 
         pv_apply_total,
         apply_num,
         pv, 
         application_limit, 
         result)

v2_lil <- user_lil %>%
  group_by(product_id, apply_num) %>%
  summarise(total_pv = sum(pv),
            av_result = mean(result))

v2_lil <- v2_lil %>%
  mutate(., result_rate = ifelse(av_result <.5, "low", "high"))

ggplot(v2_lil, aes(x = total_pv, y = apply_num, color = result_rate)) +
  geom_point() + 
  facet_wrap(~ result_rate) +
  coord_cartesian(ylim = c(0,500), xlim = c(0,10000))

###################################################################


# Exploring products with high approval rates

lil %>%
  filter(., av_result == 1) %>%
  #filter(., total_pv > 400)
  ggplot(., aes(x = total_pv, y = apply_num)) +
    geom_point() +
    coord_cartesian(ylim = c(0,150), xlim = c(0,400))

# Exploring in full merged data

prod_ord_user %>%
  group_by(product_id) %>%
  summarise(., total_pv = sum(pv),
            av_result = mean(result))
















