# Appendix 5: Experimentation (Replication)
# Code includes the experiment for minimum neighbors, best accuracy, influence of r on Aust and Glass, 
# and error tolerance (r = 0).

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
# 5.5 Minimum Neighbors
# =========================

### Minimum Neighbors

## kNNModel 

# Create dataframe objects and combine them
dataframe_info_1 <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                        min_node = 1, "N > 1", r = 1, prune_rate = TRUE)
dataframe_info_2 <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                        min_node = 2, "N > 2", r = 1, prune_rate = TRUE)
dataframe_info_3 <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                        min_node = 3, "N > 3", r = 1, prune_rate = TRUE)
dataframe_info_4 <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                        min_node = 4, "N > 4", r = 1, prune_rate = TRUE)
dataframe_info_5 <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                        min_node = 5, "N > 5", r = 1, prune_rate = TRUE)

combined_dataframe_min_nodes <- rbind(dataframe_info_1, 
                                      dataframe_info_2, 
                                      dataframe_info_3,
                                      dataframe_info_4,
                                      dataframe_info_5)

# Create tables
tables_min_nodes <- table_creator(combined_dataframe_min_nodes, reps_number = TRUE)
table4 <- tables_min_nodes[[1]]
table5 <- tables_min_nodes[[2]]
table6 <- tables_min_nodes[[3]]
table7 <- tables_min_nodes[[4]]

# Average Reduction Rate
N_1 <- c(12.2, 42.8, 26.2, 179.4, 138.6, 96.6)
N_2 <- c(7.6, 21.4, 16.0, 78.8, 64.6, 32.2)
N_3 <- c(6.0, 13.2, 11.0, 50.6, 39.0, 17.6)
N_4 <- c(5.2, 10.4, 9.6, 35.6, 24.4, 8.6)
N_5 <- c(5.0, 9.0, 7.2, 26.6, 19.4, 4.0)

train_total <- c(nrow(iris_data) * 0.8, nrow(glass_data) * 0.8, nrow(wine_data) * 0.8,
                 nrow(diabetes_data) * 0.8, nrow(aust_data) * 0.8, nrow(heart_data) * 0.8)

# Divide each column (dataset) by its respective train_total
N_matrix <- rbind(N_1, N_2, N_3, N_4, N_5)
N_avg_divided <- sweep(N_matrix, 2, train_total, FUN = "/")

# Average reduction rate for each N_i
avg_reduction_rate_vector <- 1 - rowMeans(N_avg_divided)

avg_reduction_rate_df <- data.frame(
  name = c("N > 1", "N > 2", "N > 3", "N > 4", "N > 5"),
  avg_reduction_rate = avg_reduction_rate_vector
)

# Output table
kable(table4, caption = "Accuracy of kNNModel with Different Minimum Neighbors")
kable(table5, caption = "Time of kNNModel with Different Minimum Neighbors")
kable(table6, caption = "Pruning Rate of kNNModel with Different Minimum Neighbors")
kable(table7, caption = "Average Number of Nodes of kNNModel after Pruning")
kable(avg_reduction_rate_df, caption = "Average Reduction Rate of kNNModel with Different Minimum Neighbors")

# =========================
# 5.6 Best Accuracy
# =========================

### Best Accuracies

# Create dataframe objects and combine them
acc_iris <- five_fold_cv_custom(list(list(scale_iris, "iris", ncol(scale_iris))), "euclidean", 
                                min_node = 1,"N > 1, r = 1", r = 1)
acc_glass <- five_fold_cv_custom(list(list(scale_glass, "glass", ncol(scale_glass))), "euclidean", 
                                 min_node = 1,"N > 1, r = 1", r = 1)
acc_wine <- five_fold_cv_custom(list(list(wine_data, "wine", 1)), "euclidean", 
                                min_node = 1,"N > 1, r = 1", r = 1)
acc_diabetes <- five_fold_cv_custom(list(list(scale_diabetes, "diabetes", ncol(scale_diabetes))), "euclidean", 
                                    min_node = 3,"N > 3, r = 5", r = 5)
acc_aust <- five_fold_cv_custom(list(list(scale_aust, "aust", ncol(scale_aust))), "euclidean", 
                                min_node = 1,"N > 1, r = 2", r = 2)
acc_heart <- five_fold_cv_custom(list(list(scale_heart, "heart", ncol(scale_heart))), "euclidean", 
                                 min_node = 3,"N > 3, r = 3", r = 3)


# Create table
table13 <- data.frame(
  name = c("iris", "glass", "wine", "diabetes", "aust", "heart"),
  accuracy = c(acc_iris$accuracy, acc_glass$accuracy, acc_wine$accuracy, 
               acc_diabetes$accuracy, acc_aust$accuracy, acc_heart$accuracy),
  time = c(acc_iris$time, acc_glass$time, acc_wine$time, 
           acc_diabetes$time, acc_aust$time, acc_heart$time),
  parameters = c(acc_iris$setting, acc_glass$setting, acc_wine$setting, 
                 acc_diabetes$setting, acc_aust$setting, acc_heart$setting)
)

kable(table13, caption = "Best Accuracies reported by Guo et al.")

# =========================
# 5.7 Influence of r on Aust and Glass
# =========================

### Influence of r on Aust and Glass

acc_aust_list <- c()
acc_glass_list <- c()

for (r_0 in 0:10){
  # Aust
  acc_aust <- five_fold_cv_kNNModel(scale_aust, "euclidean", ncol(scale_aust), min_node = 1, r = r_0)
  acc_aust_list <- c(acc_aust_list, acc_aust[[1]])
  
  # Glass
  acc_glass <- five_fold_cv_kNNModel(scale_glass, "euclidean", ncol(scale_glass), min_node = 1, r = r_0)
  acc_glass_list <- c(acc_glass_list, acc_glass[[1]])
  
}

# Create dataframe of accuracy values
all_acc <- data.frame(
  accuracy = c(acc_aust_list, acc_glass_list),
  dataset = c(rep("Aust", 11),
              rep("Glass", 11)),
  error_tolerance = c(0:10)
)

# Plot
ggplot(data = all_acc, mapping = aes(x = error_tolerance, y = accuracy, group = dataset, color = dataset)) +
  geom_point() + geom_line() + labs(x = "Error Tolerance (r)", y = "Accuracy", title = "kNNModel Error Tolerance vs. Accuracy")

# =========================
# 5.8 Error Tolerance (r = 0)
# =========================

### Error Tolerance (r = 0)

# Create dataframe objects and combine them

# Table 5 test
dataframe_info_0_error <- five_fold_cv_custom(dataset_list_processed, "euclidean", 
                                              min_node = 0, "r = 0", r = 0)

# Create table
tables_error_tolerances <- table_creator(dataframe_info_0_error)
table7 <- tables_error_tolerances[[1]]
table8 <- tables_error_tolerances[[2]]
table9 <- tables_error_tolerances[[3]]

kable(table7, caption = "Accuracy of r = 0")
kable(table8, caption = "Time of r = 0")
kable(table9, caption = "Reduction Rate of r = 0") # reduction rate is different here

# kNN Average Classification Accuracy
table21 <- data.frame(
  name = c("iris", "glass", "wine", "diabetes", "aust", "heart"),
  accuracy = c(mean(c(0.9133, 0.9067, 0.9)), mean(c(0.393, 0.374, 0.322)), 
               mean(c(0.6537, 0.5917, 0.603)), mean(c(0.7135, 0.7356, 0.7308)), 
               mean(c(0.7986, 0.8348, 0.8408)), mean(c(0.5085, 0.5692, 0.5726))),
  time = c(mean(c(0.02, 0.009, 0.010)), mean(c(0.014, 0.015, 0.015)), 
           mean(c(0.011, 0.012, 0.011)), mean(c(0.099, 0.097, 0.095)), 
           mean(c(0.081, 0.086, 0.084)), mean(c(0.024, 0.024, 0.024)))
)

kable(table21, caption = "Average Classification Accuracy of kNN")

