# Appendix 5: Experimentation (Original)
# Code includes the experiment for similarity testing and error tolerance.

# =========================
# 5.0 Library and Code setup
# =========================

# Download source files
source("src/setup.R")
source("src/kNNModel_Implementation.R")
source("src/kNN_Implementation.R")
source("src/helper_functions.R")

# Load libraries

library(knitr)
library(tidyr)
library(dplyr)
library(proxy)
library(ggplot2)

# =========================
# 5.9 Similarity Testing
# =========================

### Similarity

## kNNModel
# Create dataframe objects and combine them, todo: justify chebyshev and figure out mahalonobis + cosine
dataframe_info_euclidean <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                                min_node = 0, "Euclidean", r = 1)
dataframe_info_manhattan <- five_fold_cv_custom(dataset_list_processed, "manhattan", 
                                                min_node = 0, "Manhattan", r = 1)
dataframe_info_cosine <- five_fold_cv_custom(dataset_list_processed, "cosine", 
                                             min_node = 0, "Cosine", r = 1)
dataframe_info_canberra <- five_fold_cv_custom(dataset_list_processed, "canberra", 
                                               min_node = 0, "Canberra", r = 1)
dataframe_info_chebyshev <- five_fold_cv_custom(dataset_list_processed, "maximum", 
                                                min_node = 0, "Chebyshev", r = 1)

combined_dataframe_similarity <- rbind(dataframe_info_euclidean, 
                                       dataframe_info_manhattan, 
                                       dataframe_info_cosine,
                                       dataframe_info_canberra,
                                       dataframe_info_chebyshev)

# Create table
tables_similarity <- table_creator(combined_dataframe_similarity)
table10 <- tables_similarity[[1]]
table11 <- tables_similarity[[2]]
table12 <- tables_similarity[[3]]

kable(table10, caption = "Accuracy of kNNModel with Different Similarity Measures")
kable(table11, caption = "Time of kNNModel with Different Similarity Measures (kNNModel)")
kable(table12, caption = "Reduction Rate of kNNModel with Different Similarity Measures") # this table doesn't have anything

# kNN
dataframe_info_1_kNN_euclidean <- five_fold_cv_custom_kNN(dataset_list_processed, "euclidean", "Euclidean", k = 1)
dataframe_info_1_kNN_manhattan <- five_fold_cv_custom_kNN(dataset_list_processed, "manhattan", "Manhattan", k = 1)
dataframe_info_1_kNN_cosine <- five_fold_cv_custom_kNN(dataset_list_processed, "cosine", "Cosine", k = 1)
dataframe_info_1_kNN_canberra <- five_fold_cv_custom_kNN(dataset_list_processed, "canberra", "Canberra", k = 1)
dataframe_info_1_kNN_chebyshev <- five_fold_cv_custom_kNN(dataset_list_processed, "chebyshev", "Chebyshev", k = 1)

combined_dataframe_similarity <- rbind(dataframe_info_1_kNN_euclidean, 
                                       dataframe_info_1_kNN_manhattan, 
                                       dataframe_info_1_kNN_cosine,
                                       dataframe_info_1_kNN_canberra,
                                       dataframe_info_1_kNN_chebyshev)

# Create table
tables_kNN <- table_creator(combined_dataframe_similarity, reduction_rate = FALSE)
table16 <- tables_kNN[[1]]
table17 <- tables_kNN[[2]]

kable(table16, caption = "Accuracy of kNN with Different Similarity Measures")
kable(table17, caption = "Time of kNN with Different Similarity Measures")

# =========================
# 5.10 Error Tolerance (r = 0, 1, 2, 3)
# =========================

### Error Tolerance

# Create dataframe objects and combine them

# Additional tests
dataframe_info_1_error <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                              min_node = 0, "r = 1", r = 1)
dataframe_info_2_error <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                              min_node = 0, "r = 2", r = 2)
dataframe_info_3_error <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                              min_node = 0, "r = 3", r = 3)

combined_dataframe_error_tolerances <- rbind(dataframe_info_0_error, 
                                             dataframe_info_1_error, 
                                             dataframe_info_2_error,
                                             dataframe_info_3_error)

# Create table
tables_error_tolerances <- table_creator(combined_dataframe_error_tolerances)
table7 <- tables_error_tolerances[[1]]
table8 <- tables_error_tolerances[[2]]
table9 <- tables_error_tolerances[[3]]

kable(table7, caption = "Accuracy of kNNModel with Different Error Tolerances")
kable(table8, caption = "Time of kNNModel with Different Error Tolerances")
kable(table9, caption = "Reduction Rate of kNNModel with Different Error Tolerances") # reduction rate is different here

