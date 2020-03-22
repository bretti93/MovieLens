# Calculation of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Vector for the tuning parameters
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_sub_set$rating)
  
  b_i <- train_sub_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_sub_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  b_g <- train_sub_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_i, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

  predicted_ratings <- test_sub_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_i, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_sub_set$rating))
  
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda