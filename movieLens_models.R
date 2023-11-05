### Models ### 

#load libraries 
library(plyr)
library(tidyverse)
library(ggplot2)
library(caret)


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

#Movie titles 
movie_titles <- train_set %>% select(movieId, title) %>% 
  distinct() %>% 
  arrange(movieId) %>%
  mutate(year = str_extract(title, "\\([0-9]{4}\\)"), 
         year = str_remove_all(year, "\\("), 
         year = str_remove_all(year, "\\)"), 
         year = as.numeric(year))

#remove edx file to free memory 
rm(edx)


### Define loss function #### 
#loss function root mean square error 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#### normal regression ### 
#Model 
# rating_m.u = mu_hat + b_m + b_u + b_g + b_y 
#mu_hat: average rating in train set 
mu_hat <- mean(train_set$rating)

#b_m: movie coefficient 
movie_coeff <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_m = mean(rating - mu_hat))

#b_u: user coeff 
user_coeff <- train_set %>% 
  left_join(movie_coeff, by = "movieId") %>% 
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_m))

#b_g: genres coeff 
genres_coeff <- train_set %>% 
  left_join(movie_coeff, by = "movieId") %>%
  left_join(user_coeff, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu_hat - b_m - b_u))

#b_y: year coeff 
year_coeff <- train_set %>%
  left_join(movie_titles) %>%
  left_join(movie_coeff) %>%
  left_join(user_coeff) %>%
  left_join(genres_coeff) %>%
  group_by(year) %>%
  summarise(b_y = mean(rating - mu_hat - b_m - b_u - b_g))

## Predictions 
pred_lsm <- test_set %>%
  left_join(movie_titles) %>%
  left_join(movie_coeff) %>%
  left_join(user_coeff) %>%
  left_join(genres_coeff) %>%
  left_join(year_coeff) %>% 
  mutate(pred = mu_hat + b_m + b_u + b_g + b_y)

pred_lsm2 <- test_set %>%
  left_join(movie_titles) %>%
  left_join(movie_coeff) %>%
  left_join(user_coeff) %>%
  left_join(genres_coeff) %>%
  left_join(year_coeff) %>% 
  mutate(pred = mu_hat + b_m + b_u + b_g + b_y, 
         pred2 = round_any(pred, 0.5))

RMSE(pred_lsm$rating, pred_lsm$pred)

#table with RMSE results 
rmse_res <- tibble(method = "movie+user+genres+year", RMSE = RMSE(pred_lsm$rating, pred_lsm$pred))

#evaluate errors 
pred_lsm <- pred_lsm %>%
  mutate(error = rating - pred)

#histogram of error 
pred_lsm %>% 
  ggplot(aes(error)) +
  geom_histogram(bins = 20, color = "black") + 
  scale_x_continuous() + 
  theme_bw()
#mean of error
mean(pred_lsm$error)
#SD error 
sd(pred_lsm$error)

#check if error are normally distributed 
set.seed(1, sample.kind = "Rounding")
sample(pred_lsm$error, 5000) %>% shapiro.test()

#plot pred. vs rating 
pred_lsm %>% ggplot(aes(x = rating, y = pred)) + 
  geom_point() + 
  theme_bw()


#### Including normal error #### 
#Model 
# rating_m.u = mu_hat + b_m + b_u + b_g + b_y + error
lm_model <- train_set %>%
  left_join(movie_titles) %>%
  left_join(movie_coeff) %>%
  left_join(user_coeff) %>%
  left_join(genres_coeff) %>%
  left_join(year_coeff) %>% 
  mutate(pred = mu_hat + b_m + b_u + b_g + b_y, 
         error = rating - pred)

### Individual errors ###
#histogram of error 
lm_model %>%
  ggplot(aes(error)) + 
  geom_histogram(bins = 30, color = "black") + 
  theme_bw()

#mean of error 
avg_error = mean(lm_model$error)
#sd error 
sd_error <- sd(lm_model$error)

### Error by users #### 
user.errors <- lm_model %>%
  group_by(userId) %>%
  summarise(avg_error = mean(error), 
            sd_error = sd(error))

hist(user.errors$avg_error)
  
#using previous table 
set.seed(1, sample.kind = "Rounding")
pred_lsm <- pred_lsm %>%
  left_join(user.errors) %>%
  rowwise() %>%
  mutate(n_error = rnorm(1, avg_error, sd_error)) %>% 
  mutate(pred_2 = mu_hat + b_m + b_u + b_g + b_y + n_error)

RMSE(pred_lsm$rating, pred_lsm$pred_2)

rmse_res <- rmse_res %>%
  rbind(tibble(method = "m+u+g+y+ind.n.error", RMSE = RMSE(pred_lsm$rating, pred_lsm$pred_2)))


#delete obj to free memory 
rm(user.errors, user_coeff, movie_coeff, genres_coeff, year_coeff, pred_lsm)

#### Penalized least squares #### 
#Model 
# rating_m.u = mu_hat + b_m(l_m) + b_u(l_u) + b_g + b_y + error

lambda <- 7 #should be cross-validated 


#To calculate reg. coefficients
#movies 
movie_reg_coeff <- train_set %>%
  group_by(movieId) %>% 
  summarise(b_m = sum(rating - mu_hat)/(n()+lambda))

#users 
users_reg_coeff <- train_set %>% 
  left_join(movie_reg_coeff) %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_hat - b_m)/(n()+lambda))

#genres 
genres_reg_coeff <- train_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu_hat - b_m - b_u)/(n()+lambda))

#year 
year_reg_coeff <- train_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  left_join(genres_reg_coeff) %>%
  left_join(movie_titles) %>% 
  group_by(year) %>% 
  summarise(b_y = sum(rating - mu_hat - b_m - b_u - b_g)/(n()+lambda))

#prediction 
pred_reg <- test_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  left_join(genres_reg_coeff) %>%
  left_join(movie_titles) %>% 
  left_join(year_reg_coeff) %>%
  mutate(pred_reg = mu_hat + b_m + b_u + b_g + b_y)

#RMSE 
RMSE(pred_reg$rating, pred_reg$pred_reg)
RMSE(pred_reg$rating, pred_reg$pred_2)




######### CROSS VALIDATION ########

### Create a function that estimates RMSE in train data
my_model <- function(data, lambda){
  #To calculate reg. coefficients
  #movies 
  movie_reg_coeff <- data %>%
    group_by(movieId) %>% 
    summarise(b_m = sum(rating - mu_hat)/(n()+lambda))
  
  #users 
  users_reg_coeff <- data %>% 
    left_join(movie_reg_coeff, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_m)/(n()+lambda))
  
  #genres 
  genres_reg_coeff <- data %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu_hat - b_m - b_u)/(n()+lambda))
  
  #year 
  year_reg_coeff <- data %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    group_by(year) %>% 
    summarise(b_y = sum(rating - mu_hat - b_m - b_u - b_g)/(n()+lambda))
  
  #prediction 
  model <- data %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    left_join(year_reg_coeff, by = "year") %>%
    mutate(pred_reg = mu_hat + b_m + b_u + b_g + b_y)
  
  RMSE = sqrt(mean((model$rating - model$pred_reg)^2))
  RMSE
}

#### cross validation function 5k with bootstrapping  ### 
set.seed(1, sample.kind = "Rounding")
cv_func <- function(train_set, lamda){
  rmse = numeric()
  for (i in 1:5) {
    ind_b <- sample(1:length(train_set$userId), replace = TRUE)
    data <- train_set[ind_b,]
    rmse[i] <- my_model(data, lambda)
    
  }
  median(rmse)
  
}

#tune lambda
lambdas <- seq(0,10,0.25)

#calculate rmse for different lambdas  
set.seed(1, sample.kind = "Rounding")
cv_rmse <- sapply(lambdas, cv_func, train_set= train_set)

#plot lambdas vs rmse
qplot(lambdas, cv_rmse) + 
  scale_x_continuous(breaks = seq(0,10,1)) + 
  theme_bw()
#min rmse 
min(cv_rmse)
lambdas[which.min(cv_rmse)]


#Prediction with regularized coefficients 

lambda <- lambdas[which.min(cv_rmse)]
#To calculate reg. coefficients
#movies 
movie_reg_coeff <- train_set %>%
  group_by(movieId) %>% 
  summarise(b_m = sum(rating - mu_hat)/(n()+lambda))

#users 
users_reg_coeff <- train_set %>% 
  left_join(movie_reg_coeff) %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_hat - b_m)/(n()+lambda))

#genres 
genres_reg_coeff <- train_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu_hat - b_m - b_u)/(n()+lambda))

#year 
year_reg_coeff <- train_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  left_join(genres_reg_coeff) %>%
  left_join(movie_titles) %>% 
  group_by(year) %>% 
  summarise(b_y = sum(rating - mu_hat - b_m - b_u - b_g)/(n()+lambda))

#prediction 
pred_reg <- test_set %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  left_join(genres_reg_coeff) %>%
  left_join(movie_titles) %>% 
  left_join(year_reg_coeff) %>%
  mutate(pred_reg = mu_hat + b_m + b_u + b_g + b_y)

#RMSE 
RMSE(pred_reg$rating, pred_reg$pred_reg)

rmse_res <- rmse_res %>% 
  rbind(tibble(method = "reg. coeff", RMSE = RMSE(pred_reg$rating, pred_reg$pred_reg)))




#### Final RMSE test #### 
#load validation set 
load(file = "rda/validation.rda")

#predictions 
pred_final <- validation %>%
  left_join(movie_reg_coeff) %>%
  left_join(users_reg_coeff) %>%
  left_join(genres_reg_coeff) %>%
  left_join(movie_titles) %>% 
  left_join(year_reg_coeff) %>%
  mutate(b_m = ifelse(is.na(b_m), mean(movie_reg_coeff$b_m), b_m), # assign avg b_m for those not included
         b_y = ifelse(is.na(b_y), mean(year_reg_coeff$b_y), b_y), # assign avg b_y for those not included
         pred_reg = mu_hat + b_m + b_u + b_g + b_y)



####### FINAL RMSE #####
RMSE(pred_final$rating, pred_final$pred_reg)