# Appendix 2: kNNModel Implementation
# Code includes the implementation of the kNNModel grouping algorithm, kNNModel evaluation, pruning reps function,
# and five fold cross-validation for kNNModel.

# =========================
# 2.1 kNNModel Training
# =========================

### Group observations and make representatives

kNNModel <- function(X, Y, similarity_measure, r){
  
  # Create distance matrix
  D <- dist_custom(X, similarity_measure)
  
  
  ungrouped <- rep(TRUE, nrow(X)) # Default
  reps_list <- list() # List of representatives
  reps_list_idx <- 1 
  
  # while loop until no more ungrouped
  while (any(ungrouped)){
    
    neighborhood_large_list <- list() # List of all neighborhoods of each observation
    largest_neighborhood_idx <- list(1, 1) # list(length, index)
    
    # Iterate through each ungrouped i and create a neighborhood around it
    for (i in which(ungrouped)){
      
      
      # Create neighborhood for observation i
      sorted_idx <- order(D[i, ])
      
      # Neighborhood includes itself
      neighborhood <- c(i)
      target <- Y[i]
      max_dist <- 0
      
      
      # Check if class matches before adding
      mismatch <- 0
      for (j in sorted_idx){
        
        if (j != i){
          
          # Check if same class and ungrouped
          if (Y[j] == target && ungrouped[j]){
            neighborhood <- c(neighborhood, j)
            max_dist <- D[i, j]
          }
          else {
            
            # Check if it falls within error tolerance
            mismatch <- mismatch + 1
            
            # Falls within error tolerance
            if (mismatch <= r){
              neighborhood <- c(neighborhood, j)
              max_dist <- D[i, j]
            }
            
            # Too many errors
            else {
              break
            }
          }
        }
        
      }
      
      # Append neighborhood to larger list
      neighborhood_large_list[[i]] <- list(neighborhood, max_dist)
      
      # Check global largest
      current_largest_idx <- largest_neighborhood_idx[[2]]
      
      if (length(neighborhood) > length(neighborhood_large_list[[current_largest_idx]][[1]])){
        largest_neighborhood_idx <- list(length(neighborhood), i)
      }
      # Use minimal radius as tie breaker
      else if (length(neighborhood) == length(neighborhood_large_list[[current_largest_idx]][[1]])){
        
        if (neighborhood_large_list[[i]][[2]] < neighborhood_large_list[[current_largest_idx]][[2]]){
          largest_neighborhood_idx <- list(length(neighborhood), i)
        }
        
      }
      
    }
    
    # Group all observations in the largest neighborhood
    
    rep_idx <- largest_neighborhood_idx[[2]]
    radius <- neighborhood_large_list[[rep_idx]][[2]]
    size <- length(neighborhood_large_list[[rep_idx]][[1]])
    class <- Y[rep_idx]
    
    # Create representative
    new_rep <- list(class, radius, size, rep_idx)
    
    # Add to reps list
    reps_list[[reps_list_idx]] <- new_rep
    reps_list_idx <- reps_list_idx + 1
    
    
    # Reset ungrouped list for next loop
    grouped_idx <- neighborhood_large_list[[rep_idx]][[1]]
    ungrouped[grouped_idx] <- FALSE
    
  }
  
  return (reps_list)
}

# =========================
# 2.2 kNNModel Accuracy Evaluation
# =========================

### Compute prediction accuracy

pred_kNNModel <- function(kNN_reps, similarity_measure, train_X, test_X, test_Y){
  # Calculate distances between reps and each obs using similarity measure
  # Make list of neighborhoods that the ob falls into
  
  # Case 1: if it has 1 neighborhood, assign that class to the ob
  # Case 2: if it has multiple, assign class of the largest neighborhood (accessed using size)
  # Case 3: if it has 0 neighborhoods, assign class of closest distance to a rep's radius (idk)
  # Case 3: The Euclidean distance of dt to a representative di’s nearest boundary equals to the
  # difference of the Euclidean distance of di to dt minus Sim(di).
  
  # Compare to actual prediction, add 1 if correct, 0 if not
  # Repeat, then return correctness / total obs
  
  accuracy <- 0
  
  for (i in 1:nrow(test_X)){
    
    x_i <- test_X[i, ]
    
    # Compute distance matrix between x_i and all reps
    temp <- rbind(x_i) 
    
    # Loop through each rep and rbind them individually
    for (rep in kNN_reps){
      temp <- rbind(temp, as.numeric(train_X[rep[[4]], ]))
    }
    
    # Create distance matrix
    D <- dist_custom(temp, similarity_measure)
    
    distances_list <- D[1, 2:nrow(D)] # Distance between ob and each rep
    
    # Calculate neighborhoods x_i falls in
    neighborhood_x_i <- list()
    
    h <- 1
    j <- 1
    
    for (distance in distances_list){
      
      # distance is less than or equal to radius of rep i
      if (distance <= kNN_reps[[j]][[2]]){
        neighborhood_x_i[[h]] <- kNN_reps[[j]]
        h <- h + 1
      }
      
      j <- j + 1
    }
    
    # Class evaluation
    
    x_i_pred_class <- NULL
    
    # Case 1: One neighborhood
    if (length(neighborhood_x_i) == 1){
      
      x_i_pred_class <- neighborhood_x_i[[1]][[1]]
    }
    
    # Case 2: Multiple neighborhoods
    else if (length(neighborhood_x_i) > 1){
      
      max_neighborhood <- neighborhood_x_i[[1]]
      
      for (neighborhood in neighborhood_x_i){
        
        # Larger size and smaller radius
        if (neighborhood[[3]] > max_neighborhood[[3]]){
          
          max_neighborhood <- neighborhood
          
        }
        else if (neighborhood[[3]] == max_neighborhood[[3]]){
          
          if (neighborhood[[2]] < max_neighborhood[[2]]){
            
            max_neighborhood <- neighborhood
            
          }
        }
        
      }
      
      x_i_pred_class <- max_neighborhood[[1]]
    }
    
    # Case 3: No neighborhoods
    else {
      
      # Find nearest neighborhood (distance between rep and ob - rep's radius)
      diff_list <- numeric(length(kNN_reps))
      
      
      for (k in 1:length(kNN_reps)){
        
        diff <- abs(distances_list[k] - kNN_reps[[k]][[2]])
        diff_list[k] <- diff
        
      }
      
      ordered_diff <- order(diff_list)
      
      min_diff <- ordered_diff[[1]]
      
      x_i_pred_class <- kNN_reps[[min_diff]][[1]]
      
    }
    
    # Correct evaluation
    if (test_Y[i] == x_i_pred_class){
      accuracy <- accuracy + 1
    }
    
  }
  
  # Final Accuracy
  return (accuracy / nrow(test_X))
  
}

# =========================
# 2.3 Pruning Reps
# =========================

# Pruning (N > min_node)
# Removes reps, whose neighborhood consists of less than min_node
prune_reps <- function(reps_list, min_node){
  
  new_reps_list <- list()
  j <- 1
  
  for (rep in reps_list){
    
    # [[3]] is the size
    if (rep[[3]] > min_node){
      new_reps_list[[j]] <- rep
      j <- j + 1
    }
    
  }
  
  return (new_reps_list)
}

# =========================
# 2.4 kNNModel 5-fold Cross Validation
# =========================

### 5-fold cross-validation

five_fold_cv_kNNModel <- function(df, similarity_measure, last_col, min_node = 0, r, prune_rate = FALSE){
  
  # Split the dataset into five parts
  indices <- sample(1:nrow(df))
  
  groups <- cut_number(indices, n = 5, labels = FALSE)
  groups_list <- split(indices, groups)
  
  # Initialize metrics
  total_accuracy <- 0
  avg_reduction_rate <- 0
  avg_reps <- 0
  
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
    reps <- kNNModel(train_X, train_Y, similarity_measure, r)
    
    # Method 1 : Prune reps with neighbors < min_node
    pruned_reps <- prune_reps(reps, min_node)
    
    if (prune_rate == TRUE){    
      reduction_rate <- 1 - (length(pruned_reps)/length(reps))
    }
    
    # Method 2: Use error tolerance and leave min_node alone
    else {
      reduction_rate <- 1 - (length(reps)/nrow(train_X))
    }
    
    
    accuracy <- pred_kNNModel(pruned_reps, similarity_measure, train_X, test_X, test_Y)
    
    total_accuracy <- total_accuracy + accuracy
    avg_reduction_rate <- avg_reduction_rate + reduction_rate
    avg_reps <- avg_reps + length(pruned_reps)
  }
  
  final_accuracy <- total_accuracy / 5
  final_reduction_rate <- avg_reduction_rate / 5
  final_reps <- avg_reps / 5
  
  return (list(final_accuracy, final_reduction_rate, final_reps))
  
}


### Custom 5 fold CV function for efficiency, returns a dataframe of accuracies under one setting
five_fold_cv_custom <- function(dataset_list, similarity_measure, min_node, setting_name, r, prune_rate = FALSE){
  
  acc_values <- c()
  dataset_name <- c()
  setting_labels <- c()
  time_labels <- c()
  reduction_rate_labels <- c()
  reps_labels <- c()
  
  for (dataset in dataset_list){
    
    # dataset_list[[i]] = list(dataset, name, last_col)
    
    # append each cv accuracy to the acc_values vector
    time <- system.time({
      cv <- five_fold_cv_kNNModel(dataset[[1]], similarity_measure, dataset[[3]], min_node, r, prune_rate)
    })
    
    acc_values <- c(acc_values, cv[[1]])
    dataset_name <- c(dataset_name, dataset[[2]])
    setting_labels <- c(setting_labels, setting_name)
    time_labels <- c(time_labels, time["elapsed"])
    reduction_rate_labels <- c(reduction_rate_labels, cv[[2]])
    reps_labels <- c(reps_labels, cv[[3]])
  }
  
  dataframe_ <- data.frame(
    name = dataset_name, 
    accuracy = acc_values,
    time = time_labels,
    setting = setting_labels,
    reduction_rate = reduction_rate_labels,
    reps = reps_labels
  )
  
  return (dataframe_)
  
}

