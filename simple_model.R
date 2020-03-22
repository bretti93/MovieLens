# Calculation of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Starting with the average movie rating as baseline
mu <- mean(train_sub_set$rating)

# Average movie rating as bias 
movie_avgs <- train_sub_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Average user rating as bias
user_avgs <- train_sub_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Average genre rating as bias
genre_avgs <- train_sub_set %>%
  left_join(movie_avgs, by ="movieId") %>%
  left_join(user_avgs, by ="userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
