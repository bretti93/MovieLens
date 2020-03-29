### In this report only the steps for calculating the final version of the RMSE are included.
### Creation of graphics and similar things do only appear in the report.
### With this the simplicity and understanding for this code shall be enhanced.

###################################################
##        Part provided by the course staff      ##
###################################################

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
set.seed(1, sample.kind="Rounding")
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
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###################################################
##        Part programmed by myself              ##
###################################################

# Factorize userId, movieId and genre for faster computation
edx$userId <- as.factor(edx$userId)
edx$movieId <- as.factor(edx$movieId)
edx$genres <- as.factor(edx$genres)
validation$userId <- as.factor(validation$userId)
validation$movieId <- as.factor(validation$movieId)
validation$genres <- as.factor(validation$genres)

# Convert the timestamp an extracting the year of rating / starting at the 1st of January 1970
edx$timestamp <- year(as.POSIXct(edx$timestamp, origin = "1970-01-01"))
validation$timestamp <- year(as.POSIXct(validation$timestamp, origin = "1970-01-01"))

# Extract the release year of the movie from the title - format: xxx (yyyy) and rename the title without year
# We want to use the year in which the movie has been published for model fitting because we saw a correlation during analysis
edx$year<-substr(edx$title,nchar(as.character(edx$title))-4,nchar(as.character(edx$title))-1)
edx$title<-paste0(substr(edx$title,1,nchar(as.character(edx$title))-6))
validation$year<-substr(validation$title,nchar(as.character(validation$title))-4,nchar(as.character(validation$title))-1)
validation$title<-paste0(substr(validation$title,1,nchar(as.character(validation$title))-6))

# Adding a unique identifier for each rating
edx <- edx %>% mutate(ratingId = row_number())
validation <- validation %>% mutate(ratingId = row_number())

# Free memory - large datasets take it's toll
gc()

# Define the index for train and test set 
train_index <- sample(1:nrow(edx), 0.8 * nrow(edx))
test_index <- setdiff(1:nrow(edx), train_index)

# Create train and test set
train_set <- edx[train_index,]
test_set <- edx[test_index,]

# Make sure we don't have users and movies in the test set which are not in the training set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Function for the calculation of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Seperating the genres
edx_split <- edx %>% separate_rows(genres, sep = "\\|")
train_set_split <- train_set %>% separate_rows(genres, sep = "\\|")
test_set_split <- test_set %>% separate_rows(genres, sep = "\\|")


###  Using the training and the test set for the optimization of the regularization of our 3 variable prediction model ###
# Vector for the tuning parameters
lambdas <- seq(0, 10, 0.25)

# Compute the RMSE for all elements in our lambdas vector
rmses <- sapply(lambdas, function(l){
  
  # Mean value of the rating for the train_set
  mu <- mean(train_set$rating)
  
  # Movie effect including regularization 
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # User effect including regularization 
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  # Release year effect including regularization
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))
  
  b_g <- train_set_split %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+l))
  
  # Predict the rating for the test set
  predicted_ratings <- test_set_split %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) 
  
  predicted_ratings <- predicted_ratings %>% group_by(ratingId) %>% summarize(pred = mean(pred))
  
  RMSE(predicted_ratings$pred, test_set$rating)
})

# Choosing the lambda which minimizes the RMSE for the final model building
lambda <- lambdas[which.min(rmses)]

### Using the whole edx dataset for the building of the new model which we will then test on the validation set ###
# Mean value of the rating for the whole edx set
mu <- mean(edx$rating)

# Movie effect including regularization 
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect including regularization 
b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))

# Release year effect including regularization 
b_y <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda))

b_g <- edx_split %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda))

# Predict the rating for the validation set
predicted_ratings <- validation
predicted_ratings <- predicted_ratings %>% separate_rows(genres, sep = "\\|")

predicted_ratings <- predicted_ratings %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_y +b_g)

predicted_ratings <- predicted_ratings %>% group_by(ratingId) %>% summarize(pred = mean(pred))

RMSE(predicted_ratings$pred, validation$rating)