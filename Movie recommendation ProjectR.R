
 
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind = "Rounding"
)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)



# Config
library(plotly)
library(flexdashboard)
if(!require(gridExtra)) install.packages('https://cran.rstudio.com/bin/windows/contrib/3.6/gridExtra_2.3.zip')


## **Compind edx and removed:**


edx <- rbind(edx, removed)

# KH # rm(dl, ratings, movies, test_index, temp, movielens, removed)


#### **1.3.2 Data overview**


##### **Edx data set**


knitr::kable(head(edx),caption = "Top rows of edx file")





#create summary table
edx_summary <- data.frame(number_of_rows = nrow(edx),
                          number_of_column = ncol(edx),
                          number_of_users = n_distinct(edx$userId),
                          number_of_movies = n_distinct(edx$movieId),
                          average_rating = round(mean(edx$rating),2),
                          number_of_genres = n_distinct(edx$genres),
                          the_first_rating_date = 
                            as.Date(as.POSIXct(min(edx$timestamp), 
                                               origin = "1970-01-01")),
                          the_last_rating_date = 
                            as.Date(as.POSIXct(max(edx$timestamp),
                                               origin = "1970-01-01")))

knitr::kable(edx_summary[,1:6],caption = "Summary of edx set (part 1)")



knitr::kable(edx_summary[,7:8],caption = "Summary of edx set (part 2)")


##### **Validation set**


validation_summary <- data.frame(number_of_rows = nrow(validation),
                                 number_of_column = ncol(validation),
                                 number_of_users = n_distinct(validation$userId),
                                 number_of_movies = n_distinct(validation$movieId),
                                 average_rating = mean(validation$rating),
                                 number_of_genres = n_distinct(validation$genres),
                                 the_first_rating_date = 
                                   as.Date(as.POSIXct(min(validation$timestamp), 
                                                      origin = "1970-01-01")),
                                 the_last_rating_date = 
                                   as.Date(as.POSIXct(max(validation$timestamp), 
                                                      origin = "1970-01-01")))

#create summary table


knitr::kable(validation_summary[,1:6],caption = "Summary of validation set (part 1)")



knitr::kable(validation_summary[,7:8],caption = "Summary of validation set (part 2)")


### **1.4 Project methodology**


## **2. Analysis**

### **2.1 Variable analysis**


#### **2.1.1 Rating**  


#create a summary table grouping by rating

rating_sum <- edx %>% group_by(rating) %>%
  summarize(count = n())



gg <- rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "white") +
  theme_classic() + 
  labs(x = "Rating", y = "Count",
       title = "Number of rating",
       caption = "Figure 1 - Rating in edx dataset")
gg


gg <- rating_sum %>% 
  mutate(rating = factor(rating),
         top_rating = ifelse(rating %in% c(3,4,5),"high","low")) %>%
  ggplot(aes(x = reorder(rating, count), count, fill = top_rating)) +
  geom_col(color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("steel blue","grey")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(title = "Ranking most rating value",
       x = "Rating",
       caption = "Figure 2 - Data source edx")
gg




#### **2.1.2 Movie**  

  
  
#create summary table grouping by movieId

movie_sum <- edx %>% group_by(movieId) %>%
  summarize(n_rating_of_movie = n(), 
            mu_movie = mean(rating),
            sd_movie = sd(rating))




#create figure of number of rating

gg <- movie_sum %>% ggplot(aes(n_rating_of_movie)) +
  geom_density(fill = "gold1") +
  labs(title = "Density plot - number of rating",
       x = "number of rating",
       y = "density",
       caption = "Figure 3 - The long tail number of rating") + 
  geom_vline(aes(xintercept = mean(movie_sum$n_rating_of_movie)), color = "red")+
  annotate("text", x = 2000, y = 0.0022,
           label = print(round(mean(movie_sum$n_rating_of_movie),0)),
           color = "red", size = 3) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")




gg




gg <- movie_sum %>% 
  ggplot(aes(n_rating_of_movie, mu_movie)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth()+
  geom_vline(aes(xintercept = mean(movie_sum$n_rating_of_movie)), color = "red")+
  annotate("text", x = 2000, y = 5,
           label = print(round(mean(movie_sum$n_rating_of_movie),0)),
           color = "red", size = 3) +
  theme_classic() +
  labs(title = "Scatter plot - Average rating vs number of rating",
       x = "Number of rating / movie",
       y = "Average rating",
       caption = "Figure 4") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12))



gg




gg <- movie_sum %>% 
  mutate(group = cut(n_rating_of_movie, 
                     breaks = c(-Inf, mean(n_rating_of_movie),Inf), 
                     labels = c("n < 843", "n > 843"))) %>%
  ggplot(aes(sd_movie, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Standard deviation of rating",
       x = "Standard deviation",
       y = "count",
       caption = "Figure 7 - 
       N < 843 number of rating less than average,
       N > 843 number of rating greater than average") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12))

gg




#### 2.1.3 User  

#create summary table grouping by userId

user_sum <- edx %>% group_by(userId) %>%
  summarize(n_user_rated = n(),
            mu_user = mean(rating),
            sd_user = sd(rating))




#create figure of number of rating

gg <- user_sum %>% ggplot(aes(n_user_rated)) +
  geom_density(fill = "steel blue", alpha = 0.8) +
  labs(title = "Density plot - number of user rated",
       x = "number of rating",
       y = "density",
       caption = "Figure 8") + 
  geom_vline(aes(xintercept = mean(user_sum$n_user_rated)), color = "red")+
  annotate("text", x = 400, y = 0.009,
           label = print(round(mean(user_sum$n_user_rated),0)),
           color = "red", size = 3) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

gg


#The top 10 users have highest number of rating as as below:
  
top_10_users <- user_sum %>% arrange(desc(n_user_rated)) %>% head(10) 


knitr::kable(top_10_users,caption = "Top 10 users rating")




#top 10 users have lowest number of rating

lowest_10_users <- user_sum %>% arrange(n_user_rated) %>% head(10) 
knitr::kable(lowest_10_users,caption = "Lowest 10 users rating")





user_sum %>% 
  ggplot(aes(n_user_rated, mu_user)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  theme_classic() +
  ylab("average rating") + 
  xlab("number of rating user given") + ggtitle("Ratings vs. Avg. Rating")



gg <- user_sum %>% 
  mutate(group = cut(n_user_rated, 
                     breaks = c(-Inf, mean(n_user_rated), Inf), 
                     label = c("< 129", ">129"))) %>%
  ggplot(aes(sd_user, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Standard deviation of rating by user",
       x = "Standard deviation", y = "count",
       caption = "Figure 10") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

gg




## **2.1.4 Time effect**

edx_up1 <- edx %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01"))) %>% 
  mutate(rating_year = year(rating_time))


#Adding the release year of each movie.



edx_up2 <- edx_up1 %>%
  mutate(release_year = as.integer(substr(title, str_length(title) - 4,
                                          str_length(title) - 1)))








release_year_sum <- edx_up2 %>% group_by(release_year) %>%
  summarize(n = n(), average_rating = mean(rating))






p1 <- ggplot(release_year_sum, aes(release_year, n)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(title = "number of movies by release year",
       caption = "Figure 11") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

p2 <- release_year_sum %>% ggplot(aes(release_year, average_rating)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  theme_classic() + 
  geom_smooth() +
  labs(title = "average rating by release year",
       caption = "Figure 12") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

grid.arrange(p1, p2, nrow = 1) 

fit_lm <- lm(average_rating ~ I(release_year^3) + I(release_year^2) +
               I(release_year), 
             data = release_year_sum)
summary(fit_lm)




#calculate the first rating time of each movie
movie_sum <- edx_up2 %>% group_by(movieId) %>%
  summarize(n_rating_of_movie = n(),
            mu_movie = mean(rating),
            first_rating_time = min(timestamp))

#calculate the aging time
edx_up3 <- edx_up2 %>% left_join(movie_sum, by = "movieId")
edx_up4 <- edx_up3 %>%
  mutate(aging_time = round((timestamp - first_rating_time)/60/60/24/30,0))

#create a summary table grouping by aging time
aging_time_sum <- edx_up4 %>% group_by(aging_time) %>%
  summarize(n_aging_time = n(),
            average_rating = mean(rating))



#visualize by ggplot
p1 <- ggplot(aging_time_sum, aes(aging_time, n_aging_time)) +
  geom_point(color = "steel blue") +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(title = "number of rating per aging time",
       x = "aging time (month)",
       y = "count",
       caption = "Figure 13") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

p2 <- ggplot(aging_time_sum, aes(aging_time, average_rating)) +
  geom_point(color = "steel blue") +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(title = "average rating per aging time",
       x = "aging time (month)",
       y = "average rating",
       caption = "Figure 14") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
grid.arrange(p1, p2, nrow = 1)          




#### **2.1.5 Genres effect**


#create a vector of genres 
genres <- str_replace(edx_up4$genres,"\\|.*","")
genres <- genres[!duplicated(genres)]
genres







#calculate the number of movies per each genres
n_genres <- sapply(genres, function(ge){
  index <- str_which(edx_up4$genres, ge)
  length(edx_up4$rating[index])
  
})

#calculate the average rating of each genres
genres_rating <- sapply(genres, function(ge){
  index <- str_which(edx_up4$genres, ge) 
  mean(edx_up4$rating[index], na.rm = T)
})

#create a summary data by genres
genres_sum <- data.frame(genres = genres, 
                         n_genres = n_genres,
                         average_rating = genres_rating)

#print out the summary table by genres
sum_by_geners <- genres_sum %>% arrange(desc(n_genres)) %>% head 

knitr::kable(sum_by_geners,caption = "Summary By Genners")






#ranking genres by number of each appear in the edx data set
g1 <-  genres_sum %>% 
  mutate(top5 = ifelse(genres %in% c("Comedy","Drama","Action","Thriller","Adventure"),
                       "top5","non")) %>%
  ggplot(aes(x = reorder(genres, n_genres), n_genres, fill = top5)) +
  geom_col(color = "white") +
  theme_classic() +
  coord_flip() +
  labs(title = "number of movie by genres",
       y = "number of rating",
       x = "genres",
       caption = "Figure 15") +
  scale_fill_manual(values = c("grey","steel blue")) +
  theme(legend.position = "none")
# comparing average rating of each genres in edx data set  
g2 <-  ggplot(genres_sum, 
              aes(x = reorder(genres, average_rating), average_rating)) +
  geom_col(fill = "steel blue", color = "white") +
  theme_classic() +
  coord_flip() +
  labs(title = "average rating by genres",
       y = "average rating", x = "genres",
       caption = "Figure 16")

grid.arrange(g1, g2, nrow = 1)





gg <- edx_up4 %>% group_by(genres) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  ggplot(aes(x = reorder(genres, rating), rating)) +
  geom_col(fill = "steel blue") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x = "multiple genres", 
       y = "average rating",
       title = "average rating by genres in edx data",
       caption = "Figure 17")
gg



#create list average users
avg_user_list <- user_sum %>% 
  filter(n_user_rated >= round(mean(n_user_rated),2)-1,
         n_user_rated <= round(mean(n_user_rated),2)+1) %>% 
  select(userId, mu_user)

#select randomly 4 users
set.seed(1, sample.kind = "Rounding")



avg_user_list <- sample(avg_user_list$userId, 10)
avg_user_list



#create figure of rating change by genres of average users
gg <- edx_up4 %>% filter(userId %in% avg_user_list) %>%
  group_by(genres) %>%
  summarize(rating = mean(rating), count = n()) %>%
  ggplot(aes(reorder(genres, rating), rating))+
  geom_col(fill = "steel blue") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "genres", title = "Rating change by genres",
       caption = "Figure 18") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")

gg




### **2.2 Concept of model**
#### **2.2.1 BellKor’s model - Netflix Prize Winner**


#### **2.2.2 Model: baseline is average rating of each movie**

model_1_movie <- RMSE(edx_up4$mu_movie, edx_up4$rating)

rmse_results <- data_frame(method="Only baseline is movie average",  
                           RMSE = model_1_movie)





knitr::kable(rmse_results,caption = "RSME by Method Result")





#### **2.2.3 Adding specific effect by user**

  
  
 
b_j_sum <- edx_up4 %>% mutate(yhat = rating - mu_movie) %>%
  group_by(userId) %>%
  summarize(n_user_rated = n(),
            b_j = mean(yhat))




p1<- b_j_sum %>% ggplot(aes(b_j)) +
  geom_histogram(fill = "steel blue", color = "white") +
  theme_classic() +
  labs(title = "Distribution of user bias",
       caption = "Figure 19") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12))

p2 <- b_j_sum %>% ggplot(aes(n_user_rated, b_j)) +
  geom_point(color = "steel blue", alpha = 0.5) +
  theme_classic() +
  labs(title = "Scatter plot of user bias and number of user rated",
       caption = "Figure 20") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12))

grid.arrange(p1, p2, nrow = 1)



edx_up5 <- edx_up4 %>%
  left_join(b_j_sum, by = "userId") %>%
  mutate(mu_movie_user = mu_movie + b_j)



model_2_movie_user <- RMSE(edx_up5$mu_movie_user, edx_up5$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Add Specific-effect of user",  
                                     RMSE = model_2_movie_user))



knitr::kable(rmse_results,caption = "RSME by Method effect of user")


#### **2.2.4 Add specific-effect of time**

  
  
  
b_time_sum <- edx_up5 %>% 
  mutate(error = rating - mu_movie_user) %>%
  group_by(aging_time) %>%
  summarize(b_time = mean(error))



p1 <-  edx_up5 %>% ggplot(aes(mu_movie_user))+
  geom_histogram(fill = "steel blue", color = "white") +
  theme_classic() +
  labs(title = "predicted rating, movie - user effect",
       caption = "Figure 20")

p2 <- b_time_sum %>% ggplot(aes(b_time)) +
  geom_histogram(fill = "steel blue", color = "white") +
  theme_classic() +
  labs(title = "time effect parameter",
       caption = "Figure 21")


grid.arrange(p1, p2, nrow = 1)



#calculate predicted rating
edx_up6 <- edx_up5 %>% left_join(b_time_sum, by = "aging_time")
edx_up6$b_time[is.na(edx_up5$b_time)] <- 0

edx_up7 <- edx_up6 %>%
  mutate(mu_movie_user_time = mu_movie_user + b_time)





model_3_movie_user_time <- RMSE(edx_up7$mu_movie_user_time, edx_up7$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie - User - Time Effect Model",  
                                     RMSE = model_3_movie_user_time))


knitr::kable(rmse_results,caption = "RSME by Method movie-user-Time effict")






#### **2.2.5 Add specific-effect by genres**

#calculate the genres effect bias
b_genres_sum <- edx_up7 %>% mutate(error = rating - mu_movie_user_time) %>%
  group_by(genres) %>%
  summarize(b_genres = mean(error))



edx_up8 <- edx_up7 %>% left_join(b_genres_sum, by = "genres")

edx_up9 <- edx_up8 %>% 
  mutate(mu_movie_user_time_genres = mu_movie_user_time + b_genres)




model_4_movie_user_time_genres <- RMSE(edx_up9$mu_movie_user_time_genres, edx_up9$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie - User - Time - Genres Effect Model",  
                                     RMSE = model_4_movie_user_time_genres))


knitr::kable(rmse_results,caption = "RSME by Method movie-user-time-genner effict Model")


## **3. Results**
### **3.1 Model summary**

### **3.2 Modeling performance evaluation**

#calculate the rating time
validation <- validation %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01"))) %>% 
  mutate(rating_year = year(rating_time))

#calculate the aging time
validation <- validation %>% left_join(movie_sum, by = "movieId")
validation <- validation %>%
  mutate(aging_time = round((timestamp - first_rating_time)/60/60/24/30,0))


validation <- validation %>% left_join(b_j_sum, by = "userId") %>%
  left_join(b_time_sum, by = "aging_time") %>%
  left_join(b_genres_sum, by = "genres")




knitr::kable(head(validation[,14:16]),caption = "validation")




validation$b_time[is.na(validation$b_time)] <- mean(validation$b_time, na.rm = T)



validation <- validation %>%
  mutate(predicted_rating = mu_movie + b_j + b_time + b_genres)


RMSE(validation$rating, validation$predicted_rating)

