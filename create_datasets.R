#####################################################################################################
# Start of the script
#####################################################################################################

# Define the index for train and test set 
train_index <- sample(1:nrow(edx), 0.8 * nrow(edx))
test_index <- setdiff(1:nrow(edx), train_index)

# Create train and test set
train_set <- edx[train_index,]
test_set <- edx[test_index,]

# Create smaller train and test set for faster calculation
sub_index <- sample(1:nrow(edx), 0.1 * nrow(edx))
sub <- edx[sub_index,]

train_sub_index <- sample(1:nrow(sub), 0.8* nrow(sub))
test_sub_index <- setdiff(1:nrow(edx), train_sub_index)

train_sub_set <- sub[train_sub_index,]
test_sub_set <- sub[test_sub_index,]

# Make sure we don't have users and movies in the test set which are not in the training set
test_sub_set <- test_sub_set %>%
  semi_join(train_sub_set, by = "movieId") %>%
  semi_join(train_sub_set, by = "userId")

