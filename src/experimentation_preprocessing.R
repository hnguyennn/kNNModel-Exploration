# Appendix 5: Experimentation (Preprocessing)
# Code includes the experiment for table information, feature engineering, scaled vs. unscaled, and kNNDefault.

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
# 5.1 Table Information
# =========================

# Dataset exploration
dataset_info_table <- data.frame(
  name = c("iris", "glass", "wine", "diabetes", "aust", "heart"),
  observations = c(150, 214, 178, 768, 690, 297),
  features = c(4, 9, 13, 8, 14, 13),
  classes = c(3, 7, 3, 2, 2, 2)
)

kable(dataset_info_table, caption = "Basic Dataset information")

# =========================
# 5.2 Feature Engineering
# =========================

### Feature engineering

set.seed(6)

# Scale (glass and wine are worse, need to note this)
# Standardize the variables by subtracting mean and divided by standard deviation

# Iris
scale_iris <- as.data.frame(scale(iris_data[, -5], center=TRUE, scale=TRUE))
scale_iris$target <- iris_data[, 5]

# Glass
scale_glass <- as.data.frame(scale(glass_data[, -10], center=TRUE, scale=TRUE))
scale_glass$target <- glass_data[, 10]

# Wine
scale_wine <- as.data.frame(scale(wine_data[, -1], center=TRUE, scale=TRUE))
scale_wine$target <- wine_data[, 1]

# Diabetes
scale_diabetes <- as.data.frame(scale(diabetes_data[, -9], center=TRUE, scale=TRUE))
scale_diabetes$target <- diabetes_data[, 9]

# Aust
scale_aust <- one_hot_encoding(aust_data, 15)

# Heart
scale_heart <- one_hot_encoding(heart_data, 14)

# Pre-processed Datasets
names <- c("iris", "glass", "wine", "diabetes", "aust", "heart")
last_cols <- c(ncol(scale_iris), ncol(scale_glass), 1, ncol(scale_diabetes), ncol(scale_aust), ncol(scale_heart))

# Creating dataset
dataset_info_processed <- list(scale_iris, scale_glass, wine_data, scale_diabetes, scale_aust, scale_heart)
dataset_list_processed <- dataset_list_creator(dataset_info_processed, names, last_cols)

# =========================
# 5.3 Scaled vs. Unscaled
# =========================

### Scaled vs. Unscaled
dataset_info_scale <- list(scale_iris, scale_glass, scale_wine, scale_diabetes, scale_aust, scale_heart)
dataset_info_unscale <- list(iris_data, glass_data, wine_data, diabetes_data, aust_data, heart_data)
names <- c("iris", "glass", "wine", "diabetes", "aust", "heart")
last_cols <- c(ncol(scale_iris), ncol(scale_glass), 1, ncol(scale_diabetes), ncol(scale_aust), ncol(scale_heart))

# Create dataset_list objects
dataset_list_scale <- dataset_list_creator(dataset_info_scale, names, last_cols)
dataset_list_unscale <- dataset_list_creator(dataset_info_unscale, names, last_cols)

# Create dataframe objects and combine them
dataframe_info_scale <- five_fold_cv_custom(dataset_list_scale, "euclidean", min_node = 0, "scaled", r = 1)
dataframe_info_unscale <- five_fold_cv_custom(dataset_list_unscale, "euclidean", min_node = 0, "unscaled", r = 1)
combined_dataframe <- rbind(dataframe_info_scale, dataframe_info_unscale)

# Create table
tables_scaled_vs_unscaled <- table_creator(combined_dataframe)
table1 <- tables_scaled_vs_unscaled[[1]]
table2 <- tables_scaled_vs_unscaled[[2]]
table3 <- tables_scaled_vs_unscaled[[3]]

kable(table1, caption = "Accuracy of Scaled vs. Unscaled datasets")
kable(table2, caption = "Time of Scaled vs. Unscaled datasets")
kable(table3, caption = "Reduction Rate of Scaled vs. Unscaled datasets") 

# =========================
# 5.4 kNN Default
# =========================

## kNN default

# Create dataframe objects and combine them
dataframe_info_1_kNN_euclidean <- five_fold_cv_custom_kNN(dataset_list_processed, "euclidean", "K = 1", k = 1)
dataframe_info_3_kNN_euclidean <- five_fold_cv_custom_kNN(dataset_list_processed, "euclidean", "K = 3", k = 3)
dataframe_info_5_kNN_euclidean <- five_fold_cv_custom_kNN(dataset_list_processed, "euclidean", "K = 5", k = 5)

combined_dataframe_kNN_euclidean <- rbind(dataframe_info_1_kNN_euclidean,
                                          dataframe_info_3_kNN_euclidean,
                                          dataframe_info_5_kNN_euclidean)

tables_kNN <- table_creator(combined_dataframe_kNN_euclidean, reduction_rate = FALSE, reps_number = TRUE)
table14 <- tables_kNN[[1]]
table15 <- tables_kNN[[2]]
table16 <- tables_kNN[[3]]

kable(table14, caption = "Accuracy of kNN with different k values")
kable(table15, caption = "Time of kNN with different k values")
kable(table16, caption = "Average Number of Nodes of kNN after Pruning")

