# Appendix 3: kNN Implementation
# Code includes the implementation of kNN and its five fold cross-validation function.

# =========================
# 3.1 kNN Default
# =========================

### kNN default training + evaluation

kNN_default <- function(train_X, train_Y, test_X, test_Y, similarity_measure_0, k){
  
  D <- dist_custom(rbind(test_X, train_X), similarity_measure = similarity_measure_0)
  accuracy <- 0
  
  # Take subset of D where it compares test to training points only
  D_test_to_train <- D[1:nrow(test_X), (nrow(test_X) + 1):nrow(D)]
  
  # Evaluation
  for (i in 1:nrow(test_X)){
    
    # row of distances from x_i to training points
    D_i <- D_test_to_train[i, ]
    
    # get k closest distances
    k_idx <- order(D_i)[1:k]
    neighbors_classes <- train_Y[k_idx]
    
    # get the class and compare
    pred_class <- names(which.max(table(neighbors_classes)))
    
    if (pred_class == test_Y[i]){
      accuracy <- accuracy + 1
    }
    
  }
  
  return (accuracy / nrow(test_X))
}

# =========================
# 3.2 kNN 5-fold Cross Validation
# =========================

### kNN 5-fold cross-validation

five_fold_cv_kNN <- function(df, similarity_measure_0, last_col, k){
  
  # Split the dataset into five parts
  indices <- sample(1:nrow(df))
  
  groups <- cut_number(indices, n = 5, labels = FALSE)
  groups_list <- split(indices, groups)
  
  # Initialize metrics
  total_accuracy <- 0
  total_reps <- 0
  
  # 5 Fold Cross Validation
  for (i in 1:5){
    
    # Split Training and Testing
    test_idx <- groups_list[[i]]
    
    test <- df[test_idx,]
    train <- df[-test_idx,]
    
    test_X <- test[, -last_col]
    test_Y <- test[, last_col]
    
    train_X <- train[, -last_col]
    train_Y <- train[, last_col]
    
    # Training and Prediction
    accuracy <- kNN_default(train_X, train_Y, test_X, test_Y, similarity_measure_0, k)
    
    total_accuracy <- total_accuracy + accuracy
    total_reps <- total_reps + nrow(train_X)
  }
  
  
  final_accuracy <- total_accuracy / 5
  final_reps <- total_reps / 5
  
  return (list(final_accuracy, final_reps))
}

### Custom kNN 5-fold cross-validation for multiple datasets
five_fold_cv_custom_kNN <- function(dataset_list, similarity_measure_0, setting_name, k){
  
  acc_values <- c()
  dataset_name <- c()
  setting_labels <- c()
  time_labels <- c()
  reps_labels <- c()
  
  for (dataset in dataset_list){
    
    # dataset_list[[i]] = list(dataset, name, last_col)
    
    # append each cv accuracy to the acc_values vector
    time <- system.time({
      cv <- five_fold_cv_kNN(dataset[[1]], similarity_measure_0, dataset[[3]], k)
    })
    
    acc_values <- c(acc_values, cv[[1]])
    dataset_name <- c(dataset_name, dataset[[2]])
    setting_labels <- c(setting_labels, setting_name)
    time_labels <- c(time_labels, time["elapsed"])
    reps_labels <- c(reps_labels, cv[[2]])
  }
  
  dataframe_ <- data.frame(
    name = dataset_name, 
    accuracy = acc_values,
    time = time_labels,
    setting = setting_labels,
    reps = reps_labels
  )
  
  return (dataframe_)
  
}
