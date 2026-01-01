# Replication and Analysis of KNN-based Algorithm | Statistical Machine Learning, Fall 2025

## Overview
This project was completed as part of the PSTAT 231 course at University of California, Santa Barbara, taught by Professor Guo Yu. The goal of the project was to
replicate an existing research paper's algorithm and experiments, then add additional tests or suggestions. The paper replicated in this project was 
["KNN Model-Based Approach in Classification" by Guo et al.](https://www.researchgate.net/publication/221549123_KNN_Model-Based_Approach_in_Classification).  This paper proposed an algorithm, kNNModel, based on kNN. It grouped similar observations in the training dataset under one
representative, then evaluated new test data points based on these representatives similar to kNN. The goal of the algorithm was to significantly reduce computation times, due 
to the presence of less training points, while also picking an optimal k value.

## Objectives
- Understand the paper and its proposed algorithm
- Replicate the paper's algorithm and its experiments, then compare and analyze results
- Explore additional experiments and testing

## Dataset
Six datasets from the UCI Machine Learning Repository were selected for evaluation.

- [Iris](https://archive.ics.uci.edu/dataset/53/iris)
- [Glass](https://archive.ics.uci.edu/dataset/42/glass+identification)
- [Wine](https://archive.ics.uci.edu/dataset/109/wine)
- [Diabetes](https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database)
- [Aust](https://archive.ics.uci.edu/dataset/143/statlog+australian+credit+approval)
- [Heart Disease](https://archive.ics.uci.edu/dataset/45/heart+disease)
  
All datasets are included in the `data/` folder for reproducibility. Original sources: UCI Machine Learning Repository. 

## Methods
- Implemented kNNModel and standard kNN algorithms in R, translating logic from Python/C++ implementations.
- Conducted feature engineering on all datasets: one-hot encoding for categorical variables and imputation for numerical features.
- Developed utility functions for batch evaluation, performance tables, and cross-validation.
- Performed five-fold cross-validation and hyperparameter tuning (k values, error tolerance r).
- Extended experiments beyond the original paper by testing alternative similarity measures and varying r.

## Results
From our experiment, we found that kNNModel produced similar accuracies to kNN on less data points, which is on par with Guo et. al's results. 
Higher accuracies were observed on smaller values of r, error tolerance, and j, minimum nodes, which were on par with Guo et al.’s conclusions.
We were, however, unable to prove that computation times were faster on average compared to kNN, due to our implementation of both kNNModel and kNN,
which comprised of multiple for-loops, significantly extending the computational times. 

We also explored alternating similarity measures and varying the error tolerance, r. We found that some similarity measures, such as Canberra, performed exceptionally 
well on skewed datasets. Euclidean still remains a stable choice for most datasets, while Chebyshev was generally the worst, but we would like to explore more similarity measures,
such as Mahalanobis, in future works. In our experiment involving r, we evaluated its accuracy and performance against kNN on values of k = 1, 3, and 5. We found different trends and
relationships between r and k, which suggests that there is likely no relationship between the two values.

Overall, most results and trends from the original paper were captured in our replication. Experimentation on similarity measures indicate more possibilities in optimizing the performance
of kNNModel. Error tolerance, in general, had no relationship with k, but still prioritized lower values of r for better accuracy.

## References

Guo, G., Wang, H., Bell, D., Bi, Y., & Greer, K. (2003, November). KNN model-based approach in classi- fication. In OTM Confederated International Conferences” On the Move to Meaningful Internet Systems” (pp. 986-996). 
Berlin, Heidelberg: Springer Berlin Heidelberg.

## Author
Hannah Nguyen

https://github.com/hnguyennn

https://www.linkedin.com/in/hannah-p-nguyen/
