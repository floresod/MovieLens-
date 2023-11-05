#### Data exploration #### 
#load libraries 
library(tidyverse)
library(ggplot2)

options(digits = 6, scipen = 99)

#load edx working file 
load(file = "rda/edx.rda")

#Create index for test set -  using 10% of the data due to the large dataset
set.seed(1, sample.kind = "Rounding")
test_ind <- createDataPartition(edx$rating, times = 1, p = 0.10, list = FALSE)

#Create test sets 
test_set <- edx[test_ind,]

#create training sets 
train_set <- edx[-test_ind,]

#to make sure users and movies are the same in test and train sets
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#remove edx file to free memory 
rm(edx)


### explore users ### 
#number of users 
train_set$userId %>% unique() %>% length()

#number of rating per user
#users with highest number of ratings
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>%
  arrange(desc(obs)) %>% 
  top_n(15)
#users with lowest number of ratings 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>%
  arrange(obs)
#range of rating per user 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>%
  range(.$obs)

#distribution of rating per user 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>% 
  pull(obs) %>% log10() %>% hist()

#mean of obs 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>% 
  pull(obs) %>%  mean()
#SD 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>% 
  pull(obs) %>%  sd()
#meadian 
train_set %>% group_by(userId) %>% 
  summarise(obs = n()) %>% 
  pull(obs) %>%  median()

#Table with summary of users ratings
user_rt <- train_set %>% group_by(userId) %>%
  summarise(obs = n(), 
            avg = mean(rating), sd = sd(rating), 
            u.genres = length(unique(genres)), 
            genres = list(unique(genres)))

#distribution of avg. per user 
user_rt %>% ggplot(aes(avg)) + 
  geom_histogram(bins = 30, color = "black") + 
  theme_bw()

#avg rating vs # ratings per user
user_rt %>% ggplot(aes(x = obs, y = avg, size = u.genres)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0,6000, 1000)) +
  scale_size(breaks = c(20,200,400,600)) + 
  theme_bw()

#SD rating vs rating per user 
user_rt %>% ggplot(aes(x = obs, y = sd, size = u.genres)) +
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0,6000, 1000)) +
  scale_size(breaks = c(20,200,400,600)) + 
  theme_bw()

#proportion of user with less than 10 ratings 
mean(user_rt$obs < 10)



### Explore movies effects ### 
#Range of ratings per movie 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  range(.$obs)
#movies with highest number of ratings
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  arrange(desc(obs)) %>% 
  top_n(15)
#movies with lowest number of ratings 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  arrange(obs)

#distribution of ratings per movie 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  pull(obs) %>% log10() %>%
  hist(main = "distribution of ratings per movie", 
       xlab = "log10(ratings)")
#mean ratings per movie 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  pull(obs) %>% mean()
#median ratings per movie 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  pull(obs) %>% median()
#sd ratings per movie 
train_set %>% group_by(movieId) %>% 
  summarise(obs = n()) %>%
  pull(obs) %>% sd()

#proportion of movies with only 1 rating
movie_rt <- train_set %>% group_by(movieId) %>% 
  summarise(obs = n(), 
            avg = mean(rating), sd = sd(rating))
mean(movie_rt$obs == 1)

#avg rating vs nuo. obs per movie 
movie_rt %>% ggplot(aes(x = obs, y = avg, size = sd)) + 
  geom_point(alpha = 0.5, shape = 1) + 
  labs(y = "avg. rating", x = "number of obs.") + 
  scale_x_continuous(breaks = seq(0,30000,5000)) + 
  theme_bw()
#From this we see that movies with high number of rating have higher rating than those we low number of rates

#proportion of movies with < 10 ratings 
mean(movie_rt$obs < 10) #10% of the movies has less than 10 ratings 

#free memory 
rm(movie_rt, user_rt)




###### Explore genres effects ########
#dataframe 
genres_rt <- train_set %>% 
  group_by(genres) %>% 
  summarise(obs = n(), 
            avg = mean(rating), sd = sd(rating), 
            u.users = length(unique(userId)),
            u.movies = length(unique(movieId)))

#plot avg rating vs no. obs 
genres_rt %>% ggplot(aes(x = obs, y = avg, size = u.users)) + 
  geom_point(alpha = 0.50, shape = 1) + 
  theme_bw()

#plot sd rating vs no. obs 
genres_rt %>% ggplot(aes(x = obs, y = sd, size = u.users)) + 
  geom_point(alpha = 0.50, shape = 1) + 
  theme_bw()


#### Explore years effects ##### 
# get movie titles and years 
movie_titles <- train_set %>% select(movieId, title) %>% 
  distinct() %>% 
  arrange(movieId)
