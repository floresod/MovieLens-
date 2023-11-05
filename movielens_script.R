### This is the script for the analyses and models of the final MovieLens project ####

#### Set up files and data sets ####
#Load required libraries 
library(tidyverse)
library(ggplot2)
library(caret)

#setup R options
options(digits = 5, scipen = 999)


#### Load edx (working data set) #####
#load edx working data  
load(file = "rda/edx.rda")

#Create index for test set -  using only 10% of the data (this is enough given the large amount of data)
set.seed(1, sample.kind = "Rounding") #to get same results
test_ind <- createDataPartition(edx$rating, times = 1, p = 0.10, list = FALSE)

#Create train and test sets 
temp <- edx[test_ind,] #temporal test set
train_set <- edx[-test_ind,]

#to make sure users and movies are the same in test and train sets
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from validation set back into edx set
removed <-anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#remove unnecessary objects to free space
rm(test_ind, temp, removed)
#free memory
gc()


#### DATA EXPLORATION ####

#Create a table with movie Id and year of release 
movie_titles <- edx %>% select(movieId, title) %>% 
  distinct() %>% 
  arrange(movieId) %>% 
  mutate(year = str_extract(title, "\\([0-9]{4}\\)"), 
         year = str_remove_all(year, "\\("), 
         year = str_remove_all(year, "\\)"), 
         year = as.numeric(year))

#range of movie years of release
range(movie_titles$year)

# No. of users and movies, and general parameters 
edx %>% 
  left_join(movie_titles) %>%
  summarise(no_users = n_distinct(userId), 
            no_movies = n_distinct(movieId),
            no_genres = n_distinct(genres),
            no_years = n_distinct(year),
            mu = mean(rating), 
            sd = sd(rating))


#distribution of rating in edx data set 
fig.1 <- edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(aes(y = ..count../sum(..count..)),  bins = 10 ,fill = "darkblue", color = "white") +
  labs(x = "rating", y = "relative frequency", title = "Fig. 1 Distribution of the movie ratings") + 
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + 
  scale_y_continuous(breaks = seq(0,0.5, 0.05)) + 
  theme_bw()

#print figure
fig.1
#save figure for PDF report  
save(fig.1, file = "figs/fig.1")

### Number of ratings by users, movies, genres, and years ###

#fig 2a distribution of number of ratings per number of users
fig.2a <- edx %>% 
  group_by(userId) %>% 
  summarise(obs = n()) %>%
  ggplot(aes(obs)) + 
  geom_histogram(fill = "darkblue", color = "white", bins = 50 ) +
  labs(x = "Number of ratings",
       y = "Number of users") + 
  scale_x_log10() + #to improve visualization 
  theme_bw()

#print figure
fig.2a
#save figure for PDF report 
save(fig.2a, file = "figs/fig.2a")


#fig 2b distribution of number of ratings per number of movies
fig.2b <- edx %>%
  group_by(movieId) %>% 
  summarise(obs = n()) %>% 
  ggplot(aes(obs)) + 
  geom_histogram(fill = "darkblue", color = "white", bins = 30) + 
  labs(#title = "Fig 2b Number of ratings per movie ", 
       x = "Number of ratings", 
       y = "Number of movies") + 
  scale_x_log10() + 
  theme_bw()

#print figure 
fig.2b
#save figure for PDF report 
save(fig.2b, file = "figs/fig.2b")


#fig. 2c distribution of number of ratings per number of genres 
fig.2c <- edx %>%
  group_by(genres) %>% 
  summarise(obs = n()) %>% 
  ggplot(aes(obs)) + 
  geom_histogram(fill = "darkblue", color = "white", bins = 30) + 
  labs(#title = "Fig 2b Number of ratings per movie ", 
    x = "Number of ratings", 
    y = "Number of genres") + 
  scale_x_continuous(breaks = c(10,100,10000,1000000), trans = "log10") + 
  theme_bw()

fig.2c
#save figure for PDF report 
save(fig.2c, file = "figs/fig.2c")


#fig. 2d distribution of number of ratings per number of years
fig.2d <- edx %>% 
  left_join(movie_titles) %>%
  group_by(year) %>%
  summarise(obs = n()) %>% 
  ggplot(aes(obs)) + 
  geom_histogram(fill = "darkblue", color = "white", bins = 30) + 
  labs(#title = "Fig 2b Number of ratings per movie ", 
    x = "Number of ratings", 
    y = "Number of years") + 
  scale_x_continuous(breaks = c(10,100,10000,1000000), trans = "log10") + 
  scale_y_continuous(breaks = seq(1, 10, 2)) + 
  theme_bw()

#print figure
fig.2d
#save figure for PDF report 
save(fig.2d, file = "figs/fig.2d")


#### delete objectsand  free memory ####
rm(edx, test_ind, fig.1, fig.2a, fig.2b, fig.2c, fig.2d)
gc()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### Loss function definition ##### 
#loss function root mean square error 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### MODEL 1: just the mean ####
#mean of ratings 
mu_hat <- mean(train_set$rating)
mu_hat
# SD
sd_ts <- sd(train_set$rating)
sd_ts

#Table with RMSE results 
m1_rmse <- tibble(Model = "Model 1: Just the mean", RMSE = RMSE(test_set$rating, mu_hat))

### Residuals evaluation ###
#Calculate residuals 
m1_residuals <- test_set %>% 
  mutate(residual = rating - mu_hat)

#distribution of Model 1 residuals  
fig.4a <- m1_residuals %>% 
  ggplot(aes(residual)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), 
                     fill = "darkgreen", color = "white", bins = 10) +
  labs(title = "Model 1 residuals", 
       x = "residuals", 
       y = "relative frequency") + 
  theme_bw()

#print figure
fig.4a
#save figure for PDF report 
save(fig.4a, file = "figs/fig.4a")

### remove objects and free memory ### 
rm(m1_residuals, fig.4a)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### Model 2 ####
# rating_m.u = mu_hat + b_m + b_u + b_g + b_y 

#estimation of the predictors effects (b)
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

## Predictions on test set 
m2_pred <- test_set %>%
  left_join(movie_titles) %>%
  left_join(movie_coeff) %>%
  left_join(user_coeff) %>%
  left_join(genres_coeff) %>%
  left_join(year_coeff) %>% 
  mutate(pred = mu_hat + b_m + b_u + b_g + b_y)

#RMSE model 2
RMSE(m2_pred$rating, m2_pred$pred)

#table with Model 2 RMSE  
m2_rmse <- tibble(Model = "Model 2: All predictors",
                  RMSE = RMSE(m2_pred$rating, m2_pred$pred)) 

#analysis of residuals 
#table with residuals
m2_residuals <- m2_pred %>%
  mutate(residuals = rating - pred) %>%
  arrange(desc(residuals))

#distribution of Model 2 residuals 
fig.4b <- m2_residuals %>% 
  ggplot(aes(residuals)) + 
  geom_histogram(aes(y = ..count../sum(..count..) ), bins = 20, 
                 fill = "darkgreen", color = "white") + 
  labs(title = "Model 2 residuals", 
       x = "residuals", 
       y = "relative frequency") + 
  theme_bw()

#print figure
fig.4b
#save figure for PDF report 
save(fig.4b, file = "figs/fig.4b")

#central tendencies residuals 
m2_residuals %>%
  summarise(avg_res = mean(residuals), 
            med_res = median(residuals), 
            sd_res = sd(residuals))

#check where the errors occur - table 1 with largest residuals for PDF report
table_1 <- m2_residuals %>% 
  group_by(movieId) %>%
  summarise(avg_rating = mean(rating),
            avg_res = mean(residuals) %>% abs(), 
            obs = n()) %>%
  arrange(desc(avg_res)) %>% 
  left_join(movie_titles) %>% 
  select(title, movieId, avg_rating, avg_res, obs) %>%
  .[1:20,]

#print table
table_1
#save table for PDF report
save(table_1, file = "rda/table_1.rda")

### remove objects and free memory ###
rm(movie_coeff, user_coeff, genres_coeff, year_coeff, fig.4b, m2_residuals, m2_pred, table_1)
gc()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

###### Model 3 #######
## Regularization with penalized estimators ##
#same as model 2 but b as function of lambda

### Create a function to make prediction and calculate RMSE with regularization 
my_model <- function(train_set, test_set, lambda){
  #To calculate reg. coefficients
  #movies 
  movie_reg_coeff <- train_set %>%
    group_by(movieId) %>% 
    summarise(b_m = sum(rating - mu_hat)/(n()+lambda))
  
  #users 
  users_reg_coeff <- train_set %>% 
    left_join(movie_reg_coeff, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_m)/(n()+lambda))
  
  #genres 
  genres_reg_coeff <- train_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu_hat - b_m - b_u)/(n()+lambda))
  
  #year 
  year_reg_coeff <- train_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    group_by(year) %>% 
    summarise(b_y = sum(rating - mu_hat - b_m - b_u - b_g)/(n()+lambda))
  
  #prediction 
  model <- test_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    left_join(year_reg_coeff, by = "year") %>%
    mutate(pred_reg = mu_hat + b_m + b_u + b_g + b_y)
  
  rmse <- RMSE(model$rating, model$pred_reg)
  
 return(rmse)
}

### Tune lambda ###  
#define sequence of lambdas to tune
lambdas <- seq(0, 10, 0.25)

#calculate rmse for different lambdas - takes some time 
cv_rmse <- sapply(lambdas, my_model, train_set = train_set, test_set = test_set)

#plot lambdas vs rmse
fig.3 <- tibble(lambdas = lambdas, rmse = cv_rmse) %>% 
  ggplot(aes(x = lambdas, y = rmse)) + 
  geom_point(color = "darkblue") + 
  labs(title = "Fig. 3 Lambdas vs RMSE", 
       y = "RMSE") +
  scale_x_continuous(breaks = seq(0,10,1)) + 
  theme_bw()

#print figure 
fig.3

#save figure for PDF report 
save(fig.3, file = "figs/fig.3")

#lambda that minimizes RMSE  
min(cv_rmse)
best_lambda <- lambdas[which.min(cv_rmse)]
best_lambda

#### Predictions ###

#Modify model 3 function to generate predictions on test set - this function will be used later for final test as well
my_model_pred <- function(test_set, lambda){
  #To calculate reg. coefficients
  #movies 
  movie_reg_coeff <- train_set %>%
    group_by(movieId) %>% 
    summarise(b_m = sum(rating - mu_hat)/(n()+lambda))
  
  #users 
  users_reg_coeff <- train_set %>% 
    left_join(movie_reg_coeff, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu_hat - b_m)/(n()+lambda))
  
  #genres 
  genres_reg_coeff <- train_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu_hat - b_m - b_u)/(n()+lambda))
  
  #year 
  year_reg_coeff <- train_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    group_by(year) %>% 
    summarise(b_y = sum(rating - mu_hat - b_m - b_u - b_g)/(n()+lambda))
  
  #prediction 
  model <- test_set %>%
    left_join(movie_reg_coeff, by = "movieId") %>%
    left_join(users_reg_coeff, by = "userId") %>%
    left_join(genres_reg_coeff, by = "genres") %>%
    left_join(movie_titles, by = c("movieId", "title")) %>% 
    left_join(year_reg_coeff, by = "year") %>%
    mutate(pred_reg = mu_hat + b_m + b_u + b_g + b_y)
  
  
  rmse <- RMSE(model$rating, model$pred_reg)
  
  results <- list(model, rmse)
  names(results) <- c("model", "rmse")
  
  results
}

#Use this model with best lambda - generates prediction and rmse 
m3_pred <- my_model_pred(test_set, best_lambda)

#create a table with Model 3 RMSE 
m3_rmse <- tibble(Model = "Model 3: Regularization", RMSE = RMSE(m3_pred$model$rating, m3_pred$model$pred_reg))

### Model 3 residual analysis ###

#calculate residuals 
m3_residuals <- m3_pred$model %>% 
  mutate(residuals = rating - pred_reg)

#distribution of Model 3 residuals 
fig.4c <- m3_pred$model %>% 
  mutate(residuals = rating - pred_reg) %>%
  ggplot(aes(residuals)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 20, 
                 fill = "darkgreen", color = "white") +
  labs(title = "Model 3 residuals", 
       y = "relative frequency") + 
  theme_bw()

#print figure
fig.4c
#save figure for PDF report 
save(fig.4c, file = "figs/fig.4c")

#central tendencies 
m3_residuals %>%
  summarise(avg_res = mean(residuals), 
            med_res = median(residuals))

### delete objects and free memory ###
rm(m3_residuals, fig.3, fig.4c, m3_pred)
gc()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### Results #### 
#table with Models' RMSE for PDF report 
rmses <- m1_rmse %>%
  rbind(m2_rmse) %>%
  rbind(m3_rmse)

#save table for PDF report 
save(rmses, file = "rda/rmses.rda")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### Final test #### 

# delete more unnecessary objects #
rm(train_set, test_set, sd_ts)

#load edx/working data 
load("rda/edx.rda")

#load validation data set 
load("rda/validation.rda")


### using Model 3: regularized model ###
#fit model
final_rmse <- my_model(edx, test_set = validation, lambda = best_lambda )

############################# FINAL RMSE ##########################################
final_rmse
###################################################################################

#save final RMSE for PDF report 
save(final_rmse, file = "rda/final_rmse.rda")

rm(edx, validation)
