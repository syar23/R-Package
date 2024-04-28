#' Fit a Random Forest model using the randomForest package.
#'
#' This function fits a Random Forest model to your data using the randomForest package.
#' It automatically detects the type of response variable (binary, multiclass, or continuous)
#' and adjusts the model type in the randomForest function call accordingly.
#'
#' @param X Matrix or data frame of predictor variables.
#' Each row represents an observation, and each column represents a predictor.
#' @param y Response variable vector. For classification, it should be a factor.
#' For regression, it should be a numeric vector.
#' @param ntree Number of trees to grow in the forest.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#'
#' @return A fitted Random Forest model object of class "randomForest".
#'
#' @examples
#' # Example usage for classification
#' data(iris)
#' X <- iris[,1:4]
#' y <- iris$Species
#' rf_model <- rf_model(X, y, ntree = 100, mtry = 2)
#'
#' # Example usage for regression
#' data(mtcars)
#' X <- mtcars[,c("wt", "qsec", "hp")]
#' y <- mtcars$mpg
#' rf_model <- rf_model(X, y, ntree = 100, mtry = 2)
#'
#' @import randomForest
#' @export
rf_model <- function(X, y, ntree = 100, mtry = NULL) {
  # Ensure X is a matrix or data frame
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X should be a matrix or a data frame.")
  }
  
  # Determine the type of response variable
  model_type <- if (is.factor(y)) "classification" else "regression"
  
  # Set default mtry if not provided
  if (is.null(mtry)) {
    mtry <- if (model_type == "classification") sqrt(ncol(X)) else ncol(X) / 3
    mtry <- floor(mtry)
  }
  
  # Load required library
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required but not installed.")
  }
  library(randomForest)
  
  # Fit the Random Forest model
  rf_model <- randomForest(X, y, ntree = ntree, mtry = mtry, importance = TRUE)
  
  return(rf_model)
}




# Load the Iris dataset
data(iris)

# Prepare data: predictors and response
X <- iris[, -5]  # exclude the species column to get predictor variables
y <- iris$Species  # response variable

# Fit the Random Forest model
# Using default settings, which are quite effective for this dataset
rf_model <- randomForest(x = X, y = y, ntree = 500, mtry = 2, importance = TRUE)

# Print the model output to see details about the forest
print(rf_model)

# Check out-of-bag (OOB) error rate to evaluate model performance
cat("OOB Estimate of Error Rate: ", rf_model$err.rate[nrow(rf_model$err.rate), "OOB"], "\n")

# Plot variable importance
varImpPlot(rf_model)
