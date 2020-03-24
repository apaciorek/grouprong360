# Data cleaning for quality orders random forest data set 
# Trained to find feature importance quality data set

setwd("~/Downloads/Rong360capstoneproject/annette")
library(dplyr)
library(stringr)
library(ggplot2)
library(randomForest)

product_df <- read.delim("../product.final.txt")
quality_df <- read.delim("../quality.final.txt")
user_df <- read.delim("../user.final.txt")
order_train <- read.delim("../order_train.txt")

# to get rid of the duplicate rows in the quality dataset, can uncomment below:

# clean_quality <- unique(quality_df)

# OR faster to load it from csv: uncomment below and run:

clean_quality <- read.csv("clean_quality_inr.csv")

# join our new data frame which had duplicates removed with order train

order_and_qual <- inner_join(clean_quality, order_train, by = "user_id")

# we see some columns in this new data frame have a lot of missingness

colSums(is.na(order_and_qual))

# remove all columns with more than 50% missing data

trimmed_ord_qual <- order_and_qual[, 
                                   which(colMeans(!is.na(order_and_qual)) > 0.5)]

# see which columns remain in our data frame
names(trimmed_ord_qual)

#trimmed_ord_qual <- trimmed_ord_qual %>%
 # select(., -c(user_id))

#write.csv(trimmed_ord_qual, 'nobuckets_order_quality_df.csv', row.names = F)

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

######################################################################################

# BELOW IS THE FINAL VERSION OF THE DATA SET USED TO TRAIN THE RANDOM FOREST IN PYTHON

######################################################################################

write.csv(d_f, 'new_rf_order_quality_df.csv', row.names = F)

uniqueproduct_df <- unique(product_df)
