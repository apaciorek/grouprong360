# Data cleaning for users orders random forest data set 
# Trained to find feature importance users data set

setwd("~/Downloads/Rong360capstoneproject/annette")
library(dplyr)
library(stringr)
library(ggplot2)
library(randomForest)

product_df <- read.delim("../product.final.txt")
quality_df <- read.delim("../quality.final.txt")
user_df <- read.delim("../user.final.txt")
order_train <- read.delim("../order_train.txt")

# removing duplicate rows from the data before joining 

clean_user <- unique(user_df)
clean_order <- unique(order_train)

# joining the data

user_and_order <- inner_join(clean_user, clean_order, by = "user_id")

# investigating missingness

colSums(is.na(user_and_order))

# there is no missingness, so now we investigate the value counts

uao_names <- names(user_and_order)
for (val in uao_names) {
  print(val)
  print(sort(table(user_and_order[[val]], useNA = "ifany") ), decreasing = TRUE)
  print("")
  print("")
  print("")
} 

# dropping id columns 

uao_df <- user_and_order %>%
  select(., -c(user_id,
               product_id
  ))


# making characters as factors may make the random forest run better in r. we save both versions to csv just in case
uao_2_df <- uao_df %>% 
  mutate_if(is.character, as.factor)


write.csv(uao_df, 'users_and_orders.csv', row.names = F)

write.csv(uao_2_df, 'users_and_orders2.csv', row.names = F)






