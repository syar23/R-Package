#' Ensemble Learning Prediction
#'
#' This function performs ensemble learning using Random Forest and Elastic Net (GLM via glmnet)
#' as base learners. It trains on 70% of the data and tests on the remaining 30%. The function returns predictions
#' and accuracies for individual models as well as the ensemble model using either a simple mean or a weighted mean approach.
#'
#' @param x Matrix of predictors (independent variables), can be a mix of
#' continuous, discrete, and binary predictors.
#' @param y Response variable (binary or continuous).
#' @param data Your dataset containing both 'x' and 'y'.
#' @param weights Optional vector of weights for the weighted average of predictions. If NULL,
#' a simple average is used. Length of weights must match the number of base models.
#'
#' @return A list containing the predictions and accuracies of the individual models and the ensemble model.
#' @importFrom randomForest randomForest
#' @importFrom glmnet glmnet
#' @export
#' @examples
#' data(iris)
#' preds <- ensemble_predict(x = iris[,1:4], y = iris$Species, data = iris)
ensemble_predict <- function(x, y, data, weights = NULL) {
  # Load required libraries
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Please install the 'randomForest' package.")
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("Please install the 'glmnet' package.")
  
  # Prepare data
  x <- as.matrix(data[, names(data) %in% names(x)])
  y <- data[, names(data) %in% names(y)]
  
  # Check input types
  if (!is.matrix(x) && !is.data.frame(x)) stop("x must be a matrix or data frame.")
  if (!is.vector(y) || is.matrix(y)) stop("y must be a vector.")
  
  # Prepare response type for GLM
  is_binary <- length(unique(y)) == 2
  response_type <- if (is_binary) "binomial" else "gaussian"
  
  # Split data into training and testing sets
  set.seed(123)  # for reproducibility
  train_idx <- sample(1:nrow(data), size = floor(0.7 * nrow(data)))
  test_idx <- setdiff(1:nrow(data), train_idx)
  x_train <- data[train_idx, -which(names(data) == names(y))]
  y_train <- data[train_idx, which(names(data) == names(y))]
  x_test <- data[test_idx, -which(names(data) == names(y))]
  y_test <- data[test_idx, which(names(data) == names(y))]
  
  # Models setup
  models <- list(
    "RandomForest" = randomForest::randomForest(x_train, y_train, ntree = 100),
    "ElasticNet" = glmnet::cv.glmnet(x_train, y_train, family = response_type)
  )
  
  # Collect predictions and accuracies
  predictions <- list()
  accuracies <- list()
  for (name in names(models)) {
    model <- models[[name]]
    if (inherits(model, "randomForest")) {
      preds <- predict(model, newdata = x_test)
    } else {
      preds <- predict(model, newx = x_test, s = "lambda.min", type = "response")
    }
    predictions[[name]] <- preds
    accuracies[[name]] <- mean((preds - y_test)^2)  # Mean Squared Error for simplicity
  }
  
  # Combine predictions
  combined_predictions <- do.call("cbind", predictions)
  
  # Final ensemble predictions
  if (is.null(weights)) {
    final_predictions <- rowMeans(combined_predictions)
  } else {
    if (length(weights) != length(predictions)) stop("Length of weights must match the number of models.")
    final_predictions <- rowSums(combined_predictions * weights) / sum(weights)
  }
  ensemble_accuracy <- mean((final_predictions - y_test)^2)  # Mean Squared Error
  
  return(list(predictions = final_predictions, accuracies = c(accuracies, Ensemble = ensemble_accuracy)))
}
