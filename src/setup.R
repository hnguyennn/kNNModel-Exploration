# Appendix 1: Set up
# Code includes data download, distance functions, and one-hot encoding

# =========================
# 1.1 Data Download
# =========================

# Library
library(here)

### Download data

### Glass, target is last column (column 10 after dropping first one)
glass_data <- read.csv(here::here("data", "glass+identification", "glass.data"), header = FALSE)

# Drop first column
glass_data <- glass_data[, 2:11]

### Iris, target is last column (column 5)
iris_data <- read.csv(here::here("data", "iris", "iris.data"),
                      header = FALSE,
                      col.names = c("Sepal.Length", "Sepal.Width",
                                    "Petal.Length", "Petal.Width", "Species"))

iris_data <- iris_data[complete.cases(iris_data), ]

### Wine, target is first column (column 1)
wine_data <- read.csv(here::here("data", "wine", "wine.data"), header = FALSE)

### Diabetes, target is last column (column 9)
diabetes_data <- read.csv(here::here("data", "diabetes.csv") , header = TRUE)

### Aust, target is last column (column 15)
aust_data <- read.table(here::here("data", "statlog+australian+credit+approval", "australian.dat"), header = FALSE)

### Heart + Disease
heart_data <- read.csv(here::here("data", "heart+disease", "processed.cleveland.data") , header = FALSE)
heart_data[heart_data == "?"] <- NA
heart_data <- heart_data[complete.cases(heart_data),]
heart_data <- as.data.frame(lapply(heart_data, as.numeric))

# =========================
# 1.2 Distance Functions
# =========================

### Manual Calculations for Cosine

## Distance Calculations

# Cosine
cosine_distance_matrix <- function(x){
  
  distance_matrix <- as.matrix(proxy::dist(x, method = "cosine"))
  
  return (distance_matrix)
  
}

## Custom Distance Matrix 
dist_custom <- function(X, similarity_measure){
  
  if (similarity_measure == "cosine"){
    distance_matrix <- cosine_distance_matrix(X)
  }
  else {
    distance_matrix <- as.matrix(dist(X, method = similarity_measure)) 
  }
  
  return(distance_matrix)
  
}

# =========================
# 1.3 One-Hot Encoding
# =========================

# One hot encoding function

one_hot_encoding <- function(dataset, last_col){
  
  # Separate target variable and predictors
  target <- dataset[, last_col]
  predictors <- dataset[, -last_col]
  
  # Numeric and Categorical predictors
  numeric_cols <- sapply(predictors, is.numeric)
  numeric_names <- names(predictors)[numeric_cols]
  
  # One hot encoding categorical columns only
  ohe_ <- model.matrix(~ . - 1, data = predictors)
  
  # Scale numeric columns only
  ohe_[, numeric_names] <- scale(ohe_[, numeric_names])
  
  # Combine
  processed <- as.data.frame(ohe_)
  processed[, last_col] <- target
  
  # Return processed dataset
  return(processed)
}

