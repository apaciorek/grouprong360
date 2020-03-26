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

qual_and_ord <- inner_join(order_train, clean_quality, by = "user_id")
qual_and_ord <- qual_and_ord %>%
  group_by(., user_id) 
qual_and_ord %>% 
  summarise(., n_distinct(user_id))
means <- qual_and_ord %>%
  group_by(., user_id) %>%
  summarise(., av = mean(result))
new_df <- qual_and_ord %>%
  group_by(., user_id) %>%
  mutate(.,)

trimmed_qualord <- qual_and_ord[, 
                                   which(colMeans(!is.na(qual_and_ord)) > 0.5)]

# see which columns remain in our data frame
names(trimmed_qualord)

new_df <- trimmed_qualord %>%
  group_by(., user_id) %>%
  transmute(., av_date = mean(date), 
         av_appterm = mean(application_term),
         av_applimit = mean(application_limit),
         result = median(result),
         application_type = median(application_type),
         application_term = median(application_term),
         application_limit = mean(application_limit),
         op_type = median(op_type),
         col_type = median(col_type),
         user_loan_experience = median(user_loan_experience),
         user_has_car = median(user_has_car),
         user_social_security = median(user_social_security),
         qid77 = median(qid77),
         spam_score = mean(spam_score),
         quality = median(quality),
         user_work_period = median(user_work_period))

     
new_df <- unique(new_df)    

write.csv(new_df, 'new_df.csv', row.names = F)    
  