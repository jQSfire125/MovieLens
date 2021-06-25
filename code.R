# 1 Overview and Introduction
# 1.1 Library imports
# Make sure the user has the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Library imports
library(tidyverse)
library(caret)
library(data.table)

# 1.2 Download the raw dataset
# Note: this process could take a couple of minutes
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Download the file and read it
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# 1.3 Build the base data set and split into main (edx) and validation set (final hold-out test set)
# Build the data set
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Cleanup and remove temporary files
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# 2 Analysis 
# 2.1 Initial Exploratory Analysis
# Look at the file
head(edx)
glimpse(edx)

# Summary statistics
summary(edx)

# Number of unique users and movies
edx %>% summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))

# Top 10 movies by ratings
top_10 <- edx %>%
  dplyr::count(movieId) %>%
  top_n(10) %>%
  pull(movieId)
edx %>% filter(movieId %in% top_10) %>%
  group_by(title) %>%
  summarize(n_reviews = n()) %>%
  arrange(desc(n_reviews)) %>%
  knitr::kable()

# Import Library for formatting graphs
library(scales)

# Number of ratings per Movie
# Blockbusters on the right, obscure movies on the left
edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins= 30, fill= 'gray', color= 'black') +
  scale_x_log10() +
  labs(title= "Number of ratings per movie",
       x= "Number of ratings",
       y= "Number of movies")

# Number of ratings per User
# Most users rate between 30 and 100 movies
edx %>% 
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins=30, fill= 'gray', color= 'black') +
  scale_x_log10() +
  scale_y_continuous(label= comma) +
  labs(title= "Number of ratings per user",
       x= "Number of ratings",
       y= "Number of users")

# Ratings Distribution
# We can see that the most common rating is 4
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth= 0.25, fill= 'gray', color= 'black') +
  scale_x_continuous(breaks= seq(0.5,5.0,0.5)) +
  scale_y_continuous(label= comma, breaks= seq(0, 2500000,500000)) +
  labs(title= "Ratings Distribution",
       x= "Rating",
       y= "Number of ratings")

# Mean movie rating
edx %>% summarize(mean_rating = mean(rating))

# 2.2 Data Cleaning and feature engineering
# Remove the year from the title and add it to year column
edx <- edx %>% mutate(year = as.numeric(str_sub(title, -5,-2)))
edx <- edx %>% mutate(title = str_sub(title, 1, -8))

validation <- validation %>% mutate(year = as.numeric(str_sub(title, -5,-2)))
validation <- validation %>% mutate(title = str_sub(title, 1, -8))

# Create Age of movie column. We will use 2020 as the base year.
edx <- edx %>% mutate(movie_age= 2020 - year)
validation <- validation %>% mutate(movie_age= 2020 - year)

# Create a version with genres
# This step takes a long time. For now I will only split the edx dataset
edx_split_genres <- edx %>% separate_rows(genres, sep = "\\|")

# Extract year rated from timestamp column
# Install library for handling dates
library(lubridate)
# The timestamp shows the number of seconds elapsed since January 1st, 1970
edx <- edx %>% 
  mutate(year_rated= year(as.Date(as.POSIXct(timestamp, 
                                             origin= "1970-01-01"))))
validation <- validation %>% 
  mutate(year_rated= year(as.Date(as.POSIXct(timestamp, 
                                             origin= "1970-01-01"))))

# 2.3 Further visual exploration
# Mean rating by movie age
edx %>% group_by(movie_age) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(movie_age, mean_rating)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Mean rating by movie age",
       x= "Age",
       y= "Rating")

# Mean rating by rated_year
edx %>% group_by(year_rated) %>%
  summarize(mean_rating= mean(rating)) %>%
  ggplot(aes(year_rated, mean_rating)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Mean rating by year movie was rated",
       x= "Year rated",
       y= "Rating")

# Boxplots of move ratings per year
edx %>% group_by(movieId) %>%
  summarize(n= n(), year= as.character(first(year))) %>%
  ggplot(aes(year, n)) +
  geom_boxplot() +
  coord_trans(y= "sqrt") +
  theme(axis.text.x= element_text(angle= 90, hjust= 1, size= 5)) +
  labs(title= "Boxplots of movie ratings per year",
       x= "Year",
       y= "Number of ratings")

# Mean rating by genre
edx_split_genres %>% group_by(genres) %>%
  summarize(mean_rating= mean(rating)) %>%
  ggplot(aes(reorder(genres, mean_rating), mean_rating)) +
  geom_point() +
  coord_flip() +
  labs(title= "Mean rating by genre",
       x= "Mean rating",
       y= "Genre")

# Movies that get rated more often have better ratings
# Popular movies get better ratings
edx %>% 
  filter(year>= 1990) %>%
  group_by(movieId) %>%
  summarize(n= n(), 
            age= movie_age[1],
            title= title[1],
            mean_rating= mean(rating)) %>%
  mutate(rate= n/age) %>%
  ggplot(aes(rate, mean_rating)) +
  geom_point() + 
  geom_smooth() +
  scale_x_continuous(label= comma) +
  labs(title= "Frequency of rating and mean rating",
       x= "Frequency of rating",
       y= "Mean rating")

# 2.4 Modeling Approach
# Split the edx dataset into train and test sets
set.seed(157, sample.kind= "Rounding")
test_index <- createDataPartition(y= edx$rating, times= 1,
                                  p= 0.2, list= FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

# Make sure we don't use users and movies on the test set that do not appear on the train set
edx_test <- edx_test %>%
  semi_join(edx_train, by= "movieId") %>%
  semi_join(edx_train, by= "userId")

# Create loss Function. Residual mean square error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Let's build the simplest model possible
# 2.4.1 Model 1. Avg movie rating model
# This model estimates each new rating at the average of all ratings
mu <- mean(edx_train$rating)
mu 

# Calculate the RMSE for this model
m_1_rmse <- RMSE(edx_test$rating, mu)
m_1_rmse

# Create table with RMSE results
rmses_table <- tibble(Model = "Using just the average", RMSE = m_1_rmse)
rmses_table %>% knitr::kable()

# 2.4.2 Model 2. Movie effect model
# Lets add to our estimate the effect of the average rating for each movie
movie_effect <- edx_train %>%
  group_by(movieId) %>%
  summarize(m_e= mean(rating - mu))

# Visualize this effect
movie_effect %>%
  ggplot(aes(m_e)) +
  geom_histogram(bins= 20, fill= 'gray', color= 'black') +
  scale_x_continuous(breaks= seq(-3,1.5,0.5)) +
  scale_y_continuous(label= comma) +
  labs(title= "Movie Effect",
       x= "Movie effect",
       y= "Number of ratings")

# Model with average and movie effect:
predicted_ratings <- edx_test %>%
  left_join(movie_effect, by= "movieId") %>%
  mutate(pred= mu + m_e) %>%
  .$pred

# Calculate the RMSE for this model
m_2_rmse <- RMSE(edx_test$rating, predicted_ratings)
m_2_rmse

# Update summary table
rmses_table <- rmses_table %>% add_row(Model= "Movie effect model", RMSE= m_2_rmse)
rmses_table %>% knitr::kable()

# 2.4.3 Model 3. Movie and user effect model
# Mean rating by user
edx %>%
  group_by(userId) %>%
  summarise(m_r_u = mean(rating)) %>%
  ggplot(aes(m_r_u)) +
  geom_histogram(bins= 30, fill= 'gray', color= 'black') +
  scale_x_continuous(breaks= seq(0.5,5.0,0.5)) +
  scale_y_continuous(label= comma) +
  labs(title= "Mean Rating by user",
       x= "Mean rating",
       y= "Number of users")

# Now lets add the effect of the user bias
user_effect <- edx_train %>%
  left_join(movie_effect, by= "movieId") %>%
  group_by(userId) %>%
  summarize(u_e= mean(rating - mu - m_e))

# Visualize this effect
user_effect %>%
  ggplot(aes(u_e)) +
  geom_histogram(bins= 20, fill= 'gray', color= 'black') +
  scale_x_continuous(breaks= seq(-3.0,2.0,0.5)) +
  scale_y_continuous(label= comma) +
  labs(title= "User Effect",
       x= "User effect",
       y= "Number of ratings")

# Model with average, movie and user effects:
predicted_ratings <- edx_test %>%
  left_join(movie_effect, by= "movieId") %>%
  left_join(user_effect, by= "userId") %>%
  mutate(pred= mu + m_e + u_e) %>%
  .$pred

# Calculate the RMSE for this model
m_3_rmse <- RMSE(edx_test$rating, predicted_ratings)
m_3_rmse

# Update summary table
rmses_table <- rmses_table %>% add_row(Model= "Movie + user effect model", RMSE= m_3_rmse)
rmses_table %>% knitr::kable()

# 2.4.4 Model 4. Year effect
# Mean rating by year
edx %>% group_by(year) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(year, mean_rating)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Mean rating by year",
       x= "Year",
       y= "Rating")

# Now let's add the effect of the year bias
year_effect <- edx_train %>%
  left_join(movie_effect, by= "movieId") %>%
  left_join(user_effect, by= "userId") %>%
  group_by(year) %>%
  summarize(y_e= mean(rating - mu - m_e - u_e))

# Visualize this effect
# Very small effect
year_effect %>%
  ggplot(aes(y_e)) +
  geom_histogram(bins= 20, fill= 'gray', color= 'black') +
  labs(title= "Year Effect",
       x= "Year effect",
       y= "Number of years")

# Model with average, movie, user and year effects:
predicted_ratings <- edx_test %>%
  left_join(movie_effect, by= "movieId") %>%
  left_join(user_effect, by= "userId") %>%
  left_join(year_effect, by= "year") %>%
  mutate(pred= mu + m_e + u_e + y_e) %>%
  .$pred

# Calculate the RMSE for this model
m_4_rmse <- RMSE(edx_test$rating, predicted_ratings)
m_4_rmse

# Update the summary table
rmses_table <- rmses_table %>% add_row(Model= "Movie + user + year effect model", RMSE= m_4_rmse)
rmses_table %>% knitr::kable()

# 2.4.5 Model 5. Regularized movie + user effect model
# Looking at just the movie effect, the biggest errors are for movies with very few ratings
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

# Biggest positive errors
# We can see that they are all obscure movies with a hanful of ratings
edx_train %>% dplyr::count(movieId) %>% 
  left_join(movie_effect) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(m_e)) %>% 
  select(title, m_e, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# Biggest negative errors
edx_train %>% dplyr::count(movieId) %>% 
  left_join(movie_effect) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(m_e) %>% 
  select(title, m_e, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# In order to fix the impact of scarcely reviewed movies, we can use regularization
# lambda is the regularization strength
# We need to find the correct lambda though iteration
lambdas <- seq(0, 10, 0.25)

# We will calculate the RMSE with a sequence of different lambdas
# This process can take a few minutes
# The sapply function iterates over the sequence of lambdas
# At each iteration, it calculates the mean (mu), the movie effect (m_e)
# the user effect (u_e), creates predictions and evaluates the rmse 
rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx_train$rating)
  m_e <- edx_train %>%
    group_by(movieId) %>%
    summarize(m_e= sum(rating - mu)/(n()+l))
  u_e <- edx_train %>%
    left_join(m_e, by= "movieId") %>%
    group_by(userId) %>%
    summarize(u_e= sum(rating - m_e - mu)/(n()+l))
  predicted_ratings <- edx_test %>%
    left_join(m_e, by= "movieId") %>%
    left_join(u_e, by= "userId") %>%
    mutate(pred= mu + m_e + u_e) %>%
    .$pred
  return(RMSE(edx_test$rating, predicted_ratings))
})

# We pick the lambda that minimises the rmse
qplot(lambdas, rmses) +
  labs(title= "Best lambda for regularized movie + user effect model",
       x= "Lambda",
       y= "RMSE")
lambda <- lambdas[which.min(rmses)]
lambda

# This is the RMSE for this model
min(rmses)

# Update the summary table
rmses_table <- rmses_table %>% add_row(Model= "Regularized movie + user effect model", RMSE= min(rmses))
rmses_table %>% knitr::kable()

# 2.4.6 Model 6. Regularized movie + user + year model
# We are going to add the year effect to improve our model
# This process can take a few minutes
# The sapply function iterates over the sequence of lambdas
# At each iteration, it calculates the mean (mu), the movie effect (m_e)
# the user effect (u_e), year effect (y_e), creates predictions and evaluates the rmse 
rmses_2 <- sapply(lambdas, function(l) {
  mu <- mean(edx_train$rating)
  m_e <- edx_train %>%
    group_by(movieId) %>%
    summarize(m_e= sum(rating - mu)/(n()+l))
  u_e <- edx_train %>%
    left_join(m_e, by= "movieId") %>%
    group_by(userId) %>%
    summarize(u_e= sum(rating - m_e - mu)/(n()+l))
  y_e <- edx_train %>%
    left_join(m_e, by= "movieId") %>%
    left_join(u_e, by= "userId") %>%
    group_by(year) %>%
    summarize(y_e= sum(rating - m_e - u_e - mu)/(n()+l))
  predicted_ratings <- edx_test %>%
    left_join(m_e, by= "movieId") %>%
    left_join(u_e, by= "userId") %>%
    left_join(y_e, by= "year") %>%
    mutate(pred= mu + m_e + u_e + y_e) %>%
    .$pred
  return(RMSE(edx_test$rating, predicted_ratings))
})

# We pick the lambda that minimises the rmse
qplot(lambdas, rmses_2) +
  labs(title= "Best lambda for regularized movie + user + year effect model",
       x= "Lambda",
       y= "RMSE")
lambda_2 <- lambdas[which.min(rmses_2)]
lambda_2

# This is the RMSE for this model
min(rmses_2)

# Update the summary table
rmses_table <- rmses_table %>% add_row(Model= "Regularized movie + user + year effect model", RMSE= min(rmses_2))
rmses_table %>% knitr::kable()

# 3 Results
# Final model
# Calculate the RMSE training on the full edx set and testing on the validation set
# We will use the lambda estimated in model 6 for regularization
mu <- mean(edx$rating)

movie_effect_final <- edx %>%
  group_by(movieId) %>%
  summarize(m_e= sum(rating - mu)/(n()+lambda_2))

user_effect_final <- edx %>%
  left_join(movie_effect_final, by= "movieId") %>%
  group_by(userId) %>%
  summarize(u_e= sum(rating - mu - m_e)/(n()+lambda_2))

year_effect_final <- edx %>%
  left_join(movie_effect_final, by= "movieId") %>%
  left_join(user_effect_final, by= "userId") %>%
  group_by(year) %>%
  summarize(y_e= sum(rating - mu - m_e - u_e)/(n()+lambda_2))

predicted_ratings <- validation %>%
  left_join(movie_effect_final, by= "movieId") %>%
  left_join(user_effect_final, by= "userId") %>%
  left_join(year_effect_final, by= "year") %>%
  mutate(pred= mu + m_e + u_e + y_e) %>%
  .$pred

# Calculate the RMSE for this model
# We have improved 18.5% on the baseline model!
m_7_rmse <- RMSE(validation$rating, predicted_ratings)
m_7_rmse

# Update the summary table
rmses_table <- rmses_table %>% add_row(Model= "Final RMSE (validation set, regularized movie + user + year effect model)", RMSE= m_7_rmse)

# Final Summary Table
# This is the summary of all our models
rmses_table %>% knitr::kable()
                  
# 4 Conclusion