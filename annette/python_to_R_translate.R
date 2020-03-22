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

# write.csv(product_df, 'product_df.csv', row.names = F)
# write.csv(quality_df, 'quality_df.csv')
# write.csv(user_df, 'user_df.csv')
# write.csv(order_train, 'order_train.csv')


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

# write.csv(clean_quality, 'clean_quality_inr.csv', row.names = F)

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
# na_names <- as.name(na_names)
# na_names
clean_names <- lapply(na_names, as.name)

# Printing out value counts for all columns with NA values

for (val in clean_names) {
  print(val)
  print(sort(table(trimmed_ord_qual[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}


# cleaning the data set
# putting everything in buckets, removing columns


toq_clean <- trimmed_ord_qual %>%
  mutate(., user_income_by_card_bucket = ifelse(user_income_by_card < 1000, 
                   "1000", 
                   ifelse(user_income_by_card< 10000, 
                          "10000",
                          ifelse(user_income_by_card < 50000,
                                 "50000",
                                 ifelse(user_income_by_card < 100000,
                                        "100000",
                                        ifelse(user_income_by_card < 1000000,
                                               "1000000",
                                               "2000000"))))))

toq_clean <- toq_clean %>%
  select(., -user_income_by_card)
toq_clean <- toq_clean %>%
  select(., -user_id)
toq_clean <- toq_clean %>%
  select(., -city_id)
toq_clean <- toq_clean %>%
  select(., -product_id)
toq_clean <- toq_clean %>%
  select(., -bank_id)


# return the names of the columns with NAs
na_names_toq <- colnames(toq_clean)[!complete.cases(t(toq_clean))]
na_names_toq
clean_names <- lapply(na_names_toq, as.name)
clean_names

# Printing out value counts for all columns with NA values

for (val in clean_names) {
  print(val)
  print(sort(table(toq_clean[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}

# now we impute all the NAs - we will make a new category  

# set.seed(0)
#train = sample(1:nrow(toq_clean), 8*nrow(toq_clean)/10) #Training indices.
#toq.test = toq_clean[-train, ] #Test dataset.
#toq.test = toq_clean$result[-train] #Test resp

#toq.train = toq_clean[train, ] # training set is all indices that match our sample
#toq.test = toq_clean[-train, ] # test set is everything else

#set.seed(0)
#rf.toq = randomForest(result ~ ., data = toq.train, importance = T,
#                     do.trace = 50, na.action = na.omit)
#rf.toq

#importance(rf.toq)
#varImpPlot(rf.toq)

##
# We have to bucket all the rest of the variables 
toq_names <- names(toq_clean)

for (val in toq_names) {
  print(val)
  print(sort(table(toq_clean[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}

# variables to bucket:
# application term, application_limit, apply_from, source, medium,
# mobile_source, mobile-medium, date, term, limit

# will remove apply_from, source, medium, mobile_source, mobile_medium from 
# the data set for the random forest
# unclear how to bucket or what the interpretation would be



# bucketing application term
toq_clean <- toq_clean %>%
  mutate(., application_term_buckets = ifelse(application_term < 10, 
                                              "10", 
                                              ifelse(application_term < 20,
                                                     "20",
                                                     ifelse(application_term < 30,
                                                            "30",
                                                            ifelse(application_term < 40,
                                                                   "40",
                                                                   ifelse(application_term < 50,
                                                                          "50",
                                                                          ifelse(application_term < 100,
                                                                                 "100",
                                                                                 ifelse(application_term < 250, 
                                                                                        "250",
                                                                                        ifelse(application_term < 500,
                                                                                               "500", 
                                                                                               "600")))))))))

# bucketing application limit
toq_clean <- toq_clean %>%
  mutate(., app_limit_buckets = ifelse(application_limit < 6,
                                       "6",
                                       ifelse(application_limit < 11,
                                              "11",
                                              ifelse(application_limit < 25,
                                                     "25",
                                                     ifelse(application_limit < 51,
                                                            "51",
                                                            ifelse(application_limit < 100,
                                                                   "100",
                                                                   ifelse(application_limit < 300,
                                                                          "300",
                                                                          ifelse(application_limit < "1000",
                                                                                 "1000",
                                                                                 ifelse(application_limit < 100000,
                                                                                        "100000",
                                                                                        "200000")))))))))

# bucketing dates - they range from 754 to 1546
toq_clean <- toq_clean %>%
  mutate(., date_buckets = ifelse(date < 800,
                                  "7",
                                  ifelse(date < 900,
                                         "8",
                                         ifelse(date < 1000,
                                                "9",
                                                ifelse(date < 1100,
                                                       "10",
                                                       ifelse(date < 1200,
                                                              "11",
                                                              ifelse(date < 1300,
                                                                     "12",
                                                                     ifelse(date < 1400,
                                                                            "13",
                                                                            ifelse(date < 1500,
                                                                                   "14",
                                                                                   "15")))))))))
                                                    




toq_clean <- toq_clean %>%
  mutate(., term_buckets = ifelse(term == 12,
                                  "12",
                                  ifelse(term == 24,
                                         "24",
                                         ifelse(term == 36,
                                                "36",
                                                ifelse(term == 48,
                                                       "48",
                                                       ifelse(term == 60,
                                                              "60",
                                                              ifelse(term < 100,
                                                                     "100",
                                                                     ifelse(term < 241,
                                                                            "241",
                                                                            "300"))))))))


# buckets for limit
toq_clean <- toq_clean %>%
  mutate(., limit_buckets = ifelse(limit < 4, 
                                   "4",
                                   ifelse(limit < 11,
                                          "10",
                                          ifelse(limit < 21,
                                                 "20",
                                                 ifelse(limit < 51,
                                                        "51",
                                                        ifelse(limit < 101,
                                                               "101",
                                                               ifelse(limit < 201,
                                                                      "201",
                                                                      ifelse(limit < 500,
                                                                             "500",
                                                                             ifelse(limit < 100,
                                                                                    "1000",
                                                                                    ifelse(5000,
                                                                                           "5000",
                                                                                           "6000"))))))))))

toq_names <- names(toq_clean)

for (val in toq_names) {
  print(val)
  print(sort(table(toq_clean[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}

# now that we have make buckets for the necessary variables, we 
# will remove apply_from, source, medium, mobile_source, mobile_medium
# and have to remove all the variables that we just bucketed

toq_rf_df <- toq_clean %>%
  select(., -c(apply_from, 
               source, 
               medium, 
               mobile_source, 
               mobile_medium,
               limit,
               term,
               date,
               application_limit,
               application_term
               ))

# now we impute all NAs with -999


toqrf_names <- names(toq_rf_df)

for (val in toqrf_names) {
  print(val)
  print(sort(table(toq_clean[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}

na_names_toqrf <- colnames(toq_rf_df)[!complete.cases(t(toq_rf_df))]
na_names_toqrf

# impute op_type column with -999

#dat[["four"]][is.na(dat[["four"]])] <- 0

toq_rf_df[["op_type"]][is.na(toq_rf_df[["op_type"]])] <- -999

#impute col_type with -999

toq_rf_df[["col_type"]][is.na(toq_rf_df[["col_type"]])] <- -999

# impute user_loan_experience with -999

toq_rf_df[["user_loan_experience"]][is.na(toq_rf_df[["user_loan_experience"]])] <- -999

# impute user_has_car with -999
toq_rf_df[["user_has_car"]][is.na(toq_rf_df[["user_has_car"]])] <- -999

# impute user_social security with -999
toq_rf_df[["user_social_security"]][is.na(toq_rf_df[["user_social_security"]])] <- -999

# impute qid77 with -999
toq_rf_df[["qid77"]][is.na(toq_rf_df[["qid77"]])] <- -999

# impute user_work_period with -999
toq_rf_df[["user_work_period"]][is.na(toq_rf_df[["user_work_period"]])] <- -999

# impute loan_limit_n with -999
toq_rf_df[["loan_limit_n"]][is.na(toq_rf_df[["loan_limit_n"]])] <- -999

# impute op_type_n with -999
toq_rf_df[["op_type_n"]][is.na(toq_rf_df[["op_type_n"]])] <- -999

# impute col_type_n with -999
toq_rf_df[["col_type_n"]][is.na(toq_rf_df[["col_type_n"]])] <- -999

# impute user_loan_experience_n with -999
toq_rf_df[["user_loan_experience_n"]][is.na(toq_rf_df[["user_loan_experience_n"]])] <- -999

# impute user_has_car_n with -999
toq_rf_df[["user_has_car_n"]][is.na(toq_rf_df[["user_has_car_n"]])] <- -999

# impute user_social_security_n with -999
toq_rf_df[["user_social_security_n"]][is.na(toq_rf_df[["user_social_security_n"]])] <- -999

# impute qid77_n with -999
toq_rf_df[["qid77_n"]][is.na(toq_rf_df[["qid77_n"]])] <- -999

# impute cash_receipts_n with -999
toq_rf_df[["cash_receipts_n"]][is.na(toq_rf_df[["cash_receipts_n"]])] <- -999

# impute user_income_by_card_n with -999
toq_rf_df[["user_income_by_card_n"]][is.na(toq_rf_df[["user_income_by_card_n"]])] <- -999

# impute user_work_period_n with -999
toq_rf_df[["user_work_period_n"]][is.na(toq_rf_df[["user_work_period_n"]])] <- -999

# impute user_age_n with -999
toq_rf_df[["user_age_n"]][is.na(toq_rf_df[["user_age_n"]])] <- -999

# impute is_paid with -999
toq_rf_df[["is_paid"]][is.na(toq_rf_df[["is_paid"]])] <- -999

# impute quality with -999
toq_rf_df[["quality"]][is.na(toq_rf_df[["quality"]])] <- -999

# impute mobile_is_paid with -999
toq_rf_df[["mobile_is_paid"]][is.na(toq_rf_df[["mobile_is_paid"]])] <- -999

# impute mobile_quality with -999
toq_rf_df[["mobile_quality"]][is.na(toq_rf_df[["mobile_quality"]])] <- -999

# impute standard_type with -999
toq_rf_df[["standard_type"]][is.na(toq_rf_df[["standard_type"]])] <- -999

# impute guarantee_type with -999
toq_rf_df[["guarantee_type"]][is.na(toq_rf_df[["guarantee_type"]])] <- -999

# impute user_income_by_card_bucket with "unknown"
toq_rf_df[["user_income_by_card_bucket"]][is.na(toq_rf_df[["user_income_by_card_bucket"]])] <- "-999"

#check our imputation 

check_names <- names(toq_rf_df)

for (val in check_names) {
  print(val)
  print(sort(table(toq_rf_df[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}

# convert everything to factor to avoid coercion to NA
d_f <- toq_rf_df %>% 
  mutate_if(is.character, as.factor)

d_f <- d_f %>%
  select(-result, result)

write.csv(d_f, 'new_rf_order_quality_df.csv', row.names = F)

check_names <- names(d_f)


for (val in check_names) {
  print(val)
  print(sort(table(toq_rf_df[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
}


set.seed(0)
train = sample(1:nrow(d_f), 8*nrow(d_f)/10) #Training indices.
toq.test = d_f[-train, ] #Test dataset.
toq.test = d_f$result[-train] #Test resp

toq.train = d_f[train, ] # training set is all indices that match our sample
toq.test = d_f[-train, ] # test set is everything else

set.seed(0)
rf.toq = randomForest(result ~ ., data = toq.train, importance = T,
                      do.trace = 50)
rf.toq

importance(rf.toq)
varImpPlot(rf.toq)




