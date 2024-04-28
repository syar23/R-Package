#' Ensemble Learning Model with Random Forest and Elastic Net
#'
#' This function implements an ensemble learning model combining Random Forest, 
#' Elastic Net, and GLM based on the nature of the response variable. It 
#' provides individual accuracies of the models and the ensemble accuracy.
#'
#' @param x Matrix of predictors (independent variables), can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable (binary or continuous).
#' @param data Your dataset containing both 'x' and 'y'.
#'
#' @importFrom randomForest randomForest
#' @importFrom glmnet glmnet
#' @return A list containing the accuracies of the individual models and the ensemble model.
#' @examples
#' data(iris)
#' result <- ensemble_model(iris[,1:4], iris[,5], iris)
#' print(result)

ensemble_model <- function(x, y, data) {
  # Ensure required packages are loaded
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required but not installed.")
  }
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required but not installed.")
  }
  
  # Load necessary libraries
  library(randomForest)
  library(glmnet)
  
  # Prepare data
  x <- as.matrix(data[, names(data) %in% names(x)])
  y <- data[, names(data) %in% names(y)]
  
  # Split data into training and testing sets
  set.seed(123)  # for reproducibility
  train_idx <- sample(1:nrow(x), size = floor(0.7 * nrow(x)))
  x_train <- x[train_idx, ]
  y_train <- y[train_idx]
  x_test <- x[-train_idx, ]
  y_test <- y[-train_idx]
  
  # Check if response variable is continuous or categorical
  is_categorical <- length(unique(y)) < 10 && is.numeric(y)
  
  # Train models
  # Random Forest
  rf_model <- randomForest(x_train, y_train)
  rf_pred <- predict(rf_model, x_test)
  
  # Elastic Net (Linear or Logistic Regression depending on the response variable)
  enet_model <- glmnet(x_train, y_train
                       , family = ifelse(is_categorical, "binomial", "gaussian")
                       )
  
  enet_pred <- predict(enet_model
                       , s = "lambda.min"
                       , newx = x_test
                       , type = ifelse(is_categorical, "response", "link")
                       )
  
  # GLM Model
  glm_model <- glm(y_train ~ ., data = data.frame(x_train, y_train)
                   , family = ifelse( is_categorical, binomial(), gaussian() )
                   )
  glm_pred <- predict(glm_model
                      , newdata = data.frame(x_test)
                      , type = ifelse(is_categorical, "response", "link")
                      )
  
  # Calculate Accuracies
  rf_acc <- mean((rf_pred == y_test) * 1)  # Simple accuracy calculation
  enet_acc <- mean((enet_pred == y_test) * 1)  # Simple accuracy calculation
  glm_acc <- mean((glm_pred == y_test) * 1)  # Simple accuracy calculation
  
  # Final ensemble accuracy using a simple average
  final_pred <- (rf_pred + enet_pred + glm_pred) / 3
  final_accuracy <- mean((final_pred == y_test) * 1)
  
  # Return results
  list(
    Random_Forest_Accuracy = rf_acc,
    Elastic_Net_Accuracy = enet_acc,
    GLM_Accuracy = glm_acc,
    Ensemble_Accuracy = final_accuracy
  )
}
