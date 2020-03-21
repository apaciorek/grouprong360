setwd("~/Downloads/Rong360capstoneproject/annette")
library(dplyr)
library(stringr)
library(ggplot2)
library(randomForest)

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
  geom_point() + facet_wrap(~ result_rate)

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
  coord_cartesian(ylim = c(0,200), xlim = c(0,2500))

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

########################################################
# To reproduce chi square test results 
# We remove duplicates from quality and join with orders
# Then we will join orders and product
# see what we can say about results

# make a new data frame removing all duplicate rows from quality_df

clean_quality <- unique(quality_df)

# write this to csv so others can use it

write.csv(clean_quality, 'clean_quality_inr.csv', row.names = F)

# join our new data frame which had duplicates removed with order train

order_and_qual <- inner_join(clean_quality, order_train, by = "user_id")

# we see some columns in this new data frame have a lot of missingness

colSums(is.na(order_and_qual))

# remove all columns with more than 50% missing data

trimmed_ord_qual <- order_and_qual[, 
                                   which(colMeans(!is.na(order_and_qual)) > 0.5)]

# see which columns remain in our data frame
names(trimmed_ord_qual)

#take further subset of our data to turn into 
# contingency table for chi squared test

# checking user_has_car
# result - small p value

cases1 <- trimmed_ord_qual %>%
  select(., result, user_has_car )
ctable1 <- table(cases1)
chisq.test(ctable1)

# category 0 has highest proportion of approvals

# 15755/(84455+15755)
# 7058/(40149+7058)
# 466/(2617+466)
# 1236/(9955+1236)

# checking application_type
# result: small p value
cases2 <- trimmed_ord_qual %>%
  select(., result, application_type)

ctable2 <- table(cases2)
chisq.test(ctable2)
ctable2

# checking application term
# result - small p value
cases3 <- trimmed_ord_qual %>%
  select(., result, application_term)

ctable3 <- table(cases3)
chisq.test(ctable3)

# checking application_limit
# small p value

cases4 <- trimmed_ord_qual %>%
  select(., result, application_limit)

ctable4 <- table(cases4)
chisq.test(ctable4)

# checking op_type
# result small p value

cases5 <- trimmed_ord_qual %>%
  select(., result, op_type)

ctable5 <- table(cases5)
chisq.test(ctable5)

# checking spam_score

cases6 <- trimmed_ord_qual %>%
  select(., result, spam_score)

ctable6 <- table(cases6)
chisq.test(ctable6)
ctable6

# checking col_type
# small p value

cases7 <- trimmed_ord_qual %>%
  select(., result, col_type)

ctable7 <- table(cases7)
chisq.test(ctable7)

# checking user_loan_experience
# small p

cases8 <- trimmed_ord_qual %>%
  select(., result, user_loan_experience)

ctable8 <- table(cases8)
chisq.test(ctable8)

# checking user_social_security
# small p value
cases9 <- trimmed_ord_qual %>%
  select(., result, user_social_security)

ctable9 <- table(cases9)
chisq.test(ctable9)

# checking qid77
# small p value
cases10 <- trimmed_ord_qual %>%
  select(., result, qid77)

ctable10 <- table(cases10)
chisq.test(ctable10)

# checking user_income_by_card
# small p value

cases11 <- trimmed_ord_qual %>%
  select(., result, user_income_by_card)

ctable11 <- table(cases11)
chisq.test(ctable11)

# checking user_work_period
# small 

cases12 <- trimmed_ord_qual %>%
  select(., result, user_work_period)

ctable12 <- table(cases12)
chisq.test(ctable12)

# checking loan_limit_n

cases13 <- trimmed_ord_qual %>%
  select(., result, loan_limit_n)

ctable13 <- table(cases13)
chisq.test(ctable13)

# checking op_type_n
cases14 <- trimmed_ord_qual %>%
  select(., result, op_type_n)

ctable14 <- table(cases14)
chisq.test(ctable14)

# checking col_type_n

cases15 <- trimmed_ord_qual %>%
  select(., result, col_type_n)

ctable15 <- table(cases15)
chisq.test(ctable15)

# checking user_loan_experience_n
cases16 <- trimmed_ord_qual %>%
  select(., result, user_loan_experience_n)

ctable16 <- table(cases16)
chisq.test(ctable16)

# checking user_has_car_n
cases17 <- trimmed_ord_qual %>%
  select(., result, user_has_car_n)

ctable17 <- table(cases17)
chisq.test(ctable17)

# checking user_social_security_n
cases18 <- trimmed_ord_qual %>%
  select(., result, user_social_security_n)

ctable18 <- table(cases18)
chisq.test(ctable18)

#checking qid77_n

cases19 <- trimmed_ord_qual %>%
  select(., result, qid77_n)

ctable19 <- table(cases19)
chisq.test(ctable19)

# checking chash_receipts_n

# random foresy for feature importance

# cleaning the data frame to traina random forest

# first we check how many NAs each column has

colSums(is.na(trimmed_ord_qual))

# return the names of the columns with NAs
na_names <- colnames(trimmed_ord_qual)[!complete.cases(t(trimmed_ord_qual))]
na_names
na_names <- as.name(na_names)
na_names
clean_names <- lapply(na_names, as.name)
clean_names <- unlist(clean_names)

for (val in clean_names) {
  print(val)
  print(sort(table(trimmed_ord_qual[[val]]), decreasing = TRUE))
  print("")
  print("#####################")
  print("")
}

lapply()


sort(table(trimmed_ord_qual$op_type), decreasing = TRUE)

# the columns with NAs are op_type,
# col_type, _user_loan_experience, user_has_car,
# user_social_security, 

set.seed(0)
train = sample(1:nrow(trimmed_ord_qual), 8*nrow(trimmed_ord_qual)/10) #Training indices.
toq.test = trimmed_ord_qual[-train, ] #Test dataset.
toq.test = trimmed_ord_qual$result[-train] #Test resp

toq.train = trimmed_ord_qual[train, ] # training set is all indices that match our sample
toq.test = trimmed_ord_qual[-train, ] # test set is everything else

set.seed(0)
rf.toq = randomForest(result ~ ., data = toq.train, importance = T,
                     do.trace = 50, na.action = na.omit)
rf.toq

importance(rf.toq)
varImpPlot(rf.toq)







