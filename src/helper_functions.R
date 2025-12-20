# Appendix 4: Helper functions
# Code includes the dataset list creator and table creator helper functions.

# =========================
# 4.1 Dataset List Creator
# =========================

# Helper function to create dataset_list

dataset_list_creator <- function(dataset_list_, name_, last_col_){
  
  dataset_info <- list()
  
  for (i in 1:6){
    # list_ = list(dataset, dataset name, last col number)
    list_ <- list(dataset_list_[[i]], name_[i], last_col_[i])
    dataset_info[[i]] <- list_
  }
  
  return (dataset_info)
}

# =========================
# 4.2 Table Creator
# =========================

# Helper function to create two tables, accuracy and time
# Additionally returns reduction_rate table by default and returns reps table if asked for

table_creator <- function(combined_dataframe_, reduction_rate = TRUE, reps_number = FALSE){
  
  # Accuracy table
  table1 <- combined_dataframe_ |>
    select(name, setting, accuracy) |>
    pivot_wider(names_from = setting, values_from = accuracy)
  
  # Time table
  table2 <- combined_dataframe_ |>
    select(name, setting, time) |>
    pivot_wider(names_from = setting, values_from = time)
  
  table_list <- list(table1, table2)
  extra_table_num_idx <- 3
  
  # Reduction Rate table
  if (reduction_rate == TRUE) {  
    table3 <- combined_dataframe_ |>
      select(name, setting, reduction_rate) |>
      pivot_wider(names_from = setting, values_from = reduction_rate)
    
    table_list[[extra_table_num_idx]] <- table3
    extra_table_num_idx <- 4 # in case number of reps is requested
  }
  
  # Number of Reps table fix so this can be returned with reduction rate if both requested
  if (reps_number == TRUE) {  
    table4 <- combined_dataframe_ |>
      select(name, setting, reps) |>
      pivot_wider(names_from = setting, values_from = reps)
    
    table_list[[extra_table_num_idx]] <- table4
  }
  
  
  return (table_list)
}

