
# I. Project Overview



#############################################################
# Preparing the data and Create train and test datasets files
#############################################################
##https://www.kaggle.com/htagholdings/canberra-real-estate-sales-20062019
library(tidyverse)
library(dplyr)
library(caret)
library(tidyverse)
library(caret)
library(readr)
housing_price <- read_csv("property_sales_canberra.csv")

##Convert records to numeric or character
h_price_conv <- as.data.frame(housing_price) %>% mutate(DATESOLD = datesold,
                                                        PRICE = as.numeric(price),
                                                        PRICE_M =as.numeric(price)/1000000,
                                                        SUBURB = as.character(suburb),
                                                        POSTCODE = as.numeric(postcode),
                                                        PARKING = as.numeric(parking),
                                                        BATHROOMS = as.numeric(bathrooms),
                                                        BEDROOMS = as.numeric(bedrooms),
                                                        PROPERTYTYPE = as.character(propertyType),
                                                        SUBURBID = suburbid)

housing_price_sales <- h_price_conv
housing_price_sales[housing_price_sales == "N/A"] <- NA
h_price <- na.omit(housing_price_sales)


set.seed(1)
test_index <- createDataPartition(y = h_price$PRICE_M, times = 1, p = 0.1, list = FALSE)
train_set <- h_price[-test_index,]
temp <- h_price[test_index,]
# Make sure suburb and postcode in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "SUBURB") %>%
  semi_join(train_set, by = "POSTCODE")
# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)


train_set <- rbind(train_set, removed)




# III.	Methods and Analysis



### A3.	***Data Exploration***


#create summary table
h_price_summary <- data.frame(number_of_rows = nrow(h_price),
                          number_of_column = ncol(h_price),
                          number_of_suburb = n_distinct(h_price$SUBURB),
                          number_of_postcode = n_distinct(h_price$POSTCODE),
                          average_price = round(mean(h_price$PRICE),2),
                          number_of_parking = n_distinct(h_price$PARKING),
                          number_of_BATHROOMS = n_distinct(h_price$BATHROOMS),
                          number_of_bedrooms = n_distinct(h_price$BEDROOMS),
                          number_of_propertytype = n_distinct(h_price$PROPERTYTYPE),
                          the_first_sell_date = min(h_price$DATESOLD),
                          the_last_sell_date = max(h_price$DATESOLD))

knitr::kable(h_price_summary[,1:5],caption = "Summary of House Price set (part 1)")

knitr::kable(h_price_summary[,6:9],caption = "Summary of House Price set (part 2)")

knitr::kable(h_price_summary[,10:11],caption = "Summary of House Price set (part 3)")







dim(train_set) 


str(train_set[,12:21]) 
  


# First Ten Rows of Dataset :

train_set1  <-  train_set %>% 
  select(12:18)
head(train_set1, n=10) %>% 
  knitr::kable()




train_set2  <-  train_set %>% 
  select(19:21)
head(train_set2, n=10) %>% 
  knitr::kable()




# Five Number Summary for Price

train_set3 <- train_set %>%
  select(PRICE_M)
summary_price <- summary(train_set3)
knitr::kable(summary_price,caption = "Summary of Price ")


### A4.	***Data Visualization***

#plot by SUBURB
c <- h_price %>% 
  mutate(n=1) %>% 
  group_by(SUBURB) %>% 
  summarize(average=mean(PRICE_M))

ggplot(c, aes(x=SUBURB, average)) +
  geom_point()+
  labs(y = "Average PRICE")+
  ggtitle("Figure 1: Average PRICE vs SUBURB") +
 theme(axis.text.x=element_blank(),
       axis.ticks.x=element_blank())


c <- h_price %>% 
  mutate(n=1) %>% 
  group_by(PARKING) %>% 
  summarize(average=mean(PRICE_M))

ggplot(c, aes(PARKING, average))+
  geom_point()+
  ggtitle("Figure 2: Average PRICE vs PARKING")+
  labs(y = "Average PRICE")#+
 # theme(axis.text.x=element_blank(),
 #       axis.ticks.x=element_blank())



c <- h_price %>% 
  mutate(n=1) %>% 
  group_by(BATHROOMS) %>% 
  summarize(average=mean(PRICE_M))
d <- c %>% 
  mutate(year=as.numeric(BATHROOMS))

ggplot(d, aes(BATHROOMS, average))+
  geom_point() +
  ggtitle("Figure 3: Average PRICE vs BATHROOMS")+
  labs(y = "Average PRICE", x="BATHROOMS") 




c <- h_price %>% 
  mutate(n=1) %>% 
  group_by(BEDROOMS) %>% 
  summarize(average=mean(PRICE_M))
d <- c %>% 
  mutate(year=as.numeric(BEDROOMS))

ggplot(d, aes(BEDROOMS, average))+
  geom_point() +
  ggtitle("Figure 4: Average PRICE vs BEDROOMS")+
  labs(y = "Average PRICE", x="BEDROOMS")




#plot by PROPERTYTYPE 
c <- h_price %>% 
  mutate(n=1) %>% 
  group_by(PROPERTYTYPE ) %>% 
  summarize(average=mean(PRICE_M))

ggplot(c, aes(x=PROPERTYTYPE , average)) +
  geom_point()+
  labs(y = "Average PRICE")+
  ggtitle("Figure 5: Average PRICE vs PROPERTYTYPE ") 





train_set %>%
  ggplot(aes(PRICE_M)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab("PRICE") +
  ylab("Relative Frequency (%)") +
  ggtitle("Figure 6: Global PRICE Histogram")+
  ylim(0, 22)




## B. Data Modeling 


### B1. ***Modeling Using the Global Sales Average*** 


### B2. ***Linear Model Price Average by SUBURB***






### B3.  ***Model Based on SUBURB and PARKING Effect***


# IV Results


mu <- mean(train_set$PRICE_M)  
#define mu (average) of PRICE_M column in train_set 
#dataset PRICE_M (in millions of units)
mu



naive_rmse <- RMSE(train_set$PRICE_M, mu) 




      #As we go along, we will be comparing different approaches. 
      #Let's start by creating a results table with this naive approach

rmse_results <- tibble(method = "Just the Average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


#**Result for Model B2** 
  

fit_lm <- lm(PRICE_M ~ SUBURB, data=train_set)
lm_predict <- predict(fit_lm, test_set)
rmse_lm <- RMSE(test_set$PRICE_M, lm_predict)

rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Linear Model",
                                 RMSE = rmse_lm))  # putting data into a table
rmse_results %>% knitr::kable()



# **Result for Model B3** 
  

SUBURB_avgs <- train_set %>%  #incorporating SUBURB effect into equation,
  #b_i is for SUBURB
  group_by(SUBURB) %>% 
  summarize(b_SUBURB = mean(PRICE_M - mu))

predicted_PRICE <- mu + 
  test_set %>%  #testing predicted PRICE data to test_set dataset
  left_join(SUBURB_avgs, by = 'SUBURB') %>% 
  pull(b_SUBURB)

# model_1_rmse <- RMSE(predicted_PRICE, test_set$PRICE_M)  #this test SUBURB effect

PARKING_avgs <- train_set %>%
  left_join(SUBURB_avgs, by='SUBURB') %>%
  group_by(PARKING) %>%
  summarize(b_PARKING = mean(PRICE_M - mu - b_SUBURB))

predicted_PRICE_PARKING <- test_set %>%   #putting both SUBURB and PARKING into model
  left_join(SUBURB_avgs, by='SUBURB') %>% 
  left_join(PARKING_avgs, by='PARKING') %>% 
  mutate(pred = mu +b_PARKING + b_SUBURB) %>%
  #na.omit() %>% 
  pull(pred)

model_2_rmse <- RMSE(predicted_PRICE_PARKING, test_set$PRICE_M)  
#calculate residual mean square error for the two effects
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="PARKING + SUBURB Effects Model",
                                 RMSE = model_2_rmse))  # putting data into a table





BATHROOMS_avgs <- train_set %>%              # adding BATHROOMS into model
  left_join(SUBURB_avgs, by='SUBURB') %>%
  left_join(PARKING_avgs, by='PARKING') %>%
  group_by(BATHROOMS) %>%
  summarize(b_BATHROOMS = mean(PRICE_M - mu - b_SUBURB - b_PARKING))

predicted_PRICE_PARKING_BATHROOMS <- test_set %>%   
  left_join(SUBURB_avgs, by='SUBURB') %>% 
  left_join(PARKING_avgs, by='PARKING') %>% 
  left_join(BATHROOMS_avgs, by='BATHROOMS') %>%
  mutate(pred = mu +b_PARKING + b_SUBURB + b_BATHROOMS) %>%
  #na.omit() %>% 
  pull(pred)



model_3_rmse <- RMSE(predicted_PRICE_PARKING_BATHROOMS, test_set$PRICE_M)  
#calculate residual mean square error for the two effects
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="PARKING + SUBURB + BATHROOMS Effects Model",
                                 RMSE = model_3_rmse))  # putting data into a table



BEDROOMS_avgs <- train_set %>%              # adding BEDROOMS into model
  left_join(SUBURB_avgs, by='SUBURB') %>%
  left_join(PARKING_avgs, by='PARKING') %>%
  left_join(BATHROOMS_avgs, by='BATHROOMS') %>%
  group_by(BEDROOMS) %>%
  summarize(b_BEDROOMS = mean(PRICE_M - mu - b_SUBURB - b_PARKING - b_BATHROOMS))

predicted_PRICE_PARKING_BATHROOMS_BEDROOMS <- test_set %>%   
  left_join(SUBURB_avgs, by='SUBURB') %>% 
  left_join(PARKING_avgs, by='PARKING') %>% 
  left_join(BATHROOMS_avgs, by='BATHROOMS') %>%
  left_join(BEDROOMS_avgs, by='BEDROOMS') %>%
  mutate(pred_BEDROOMS = mu +b_PARKING + b_SUBURB + b_BATHROOMS +b_BEDROOMS) %>%
  #na.omit() %>% 
  pull(pred_BEDROOMS)



model_4_rmse <- RMSE(predicted_PRICE_PARKING_BATHROOMS_BEDROOMS, test_set$PRICE_M)  
#calculate residual mean square error for the two effects
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="PARKING + SUBURB + BATHROOMS + BEDROOMS Effects Model",
                                 RMSE = model_4_rmse))  # putting data into a table



PROPERTYTYPE_avgs <- train_set %>%               # adding PROPERTYTYPE into model
  left_join(SUBURB_avgs, by='SUBURB') %>%
  left_join(PARKING_avgs, by='PARKING') %>%
  left_join(BATHROOMS_avgs, by='BATHROOMS') %>%
  left_join(BEDROOMS_avgs, by='BEDROOMS') %>%
  group_by(PROPERTYTYPE) %>%
  summarize(b_PROPERTYTYPE = mean(PRICE_M - mu - b_SUBURB - b_PARKING - b_BATHROOMS 
                                  - b_BEDROOMS))

predicted_PRICE_PARKING_BATHROOMS_BEDROOMS_PROPERTYTYPE <- test_set %>%   
  left_join(SUBURB_avgs, by='SUBURB') %>% 
  left_join(PARKING_avgs, by='PARKING') %>% 
  left_join(BATHROOMS_avgs, by='BATHROOMS') %>%
  left_join(BEDROOMS_avgs, by='BEDROOMS') %>%
  left_join(PROPERTYTYPE_avgs, by='PROPERTYTYPE') %>%
  mutate(pred_PROPERTYTYPE = mu +b_PARKING + b_SUBURB + b_BATHROOMS +b_BEDROOMS +
           b_PROPERTYTYPE) %>%
  #na.omit() %>% 
  pull(pred_PROPERTYTYPE)



model_5_rmse <- RMSE(predicted_PRICE_PARKING_BATHROOMS_BEDROOMS_PROPERTYTYPE, 
                     test_set$PRICE_M)  
#calculate residual mean square error for the two effects
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="PARKING + SUBURB + BATHROOMS + BEDROOMS + PROPERTYTYPE Effects Model",
                                 RMSE = model_5_rmse))  # putting data into a table



# V. Conclusion


rmse_results %>% knitr::kable()


