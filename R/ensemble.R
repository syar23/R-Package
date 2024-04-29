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
#' @param weights Optional vector of weights for the weighted average of predictions.
#' If NULL, a simple average is used. Length of weights must match the number of base models.
#'
#' @return A list containing the predictions and accuracies of the individual models and the ensemble model.
#' @export
#' @examples
#'
#' # Example usage:
#' # ensemble_predict(x, y, data)
#'
# Ensemble prediction function definition
ensemble_predict <- function(x, y, data, weights = NULL) {
  # Split data into training and testing sets
  set.seed(123)  # for reproducibility
  train_idx <- sample(1:nrow(data), size = floor(0.7 * nrow(data)))
  test_idx <- setdiff(1:nrow(data), train_idx)
  x_train <- as.matrix(data[train_idx, -ncol(data)])
  y_train <- data[train_idx, ncol(data)]
  x_test <- as.matrix(data[test_idx, -ncol(data)])

  # Models setup
  models <- list(
    "RandomForest" = randomForest::randomForest(x_train, y_train, ntree = 100),
    "ElasticNet" = glmnet::cv.glmnet(x_train, y_train, family = "gaussian")
  )

  # Collect predictions
  predictions <- lapply(models, function(model) {
    if (inherits(model, "randomForest")) {
      predict(model, x_test)  # Using the generic predict function
    } else {
      predict(model, x_test, s = "lambda.min", type = "response")
    }
  })

  # Combine predictions
  combined_predictions <- do.call(cbind, predictions)

  # Apply weights
  if (!is.null(weights)) {
    if (length(weights) != length(predictions)) stop("Length of weights must match the number of models.")
    combined_predictions <- combined_predictions * weights
  }

  # Final predictions
  final_predictions <- rowMeans(combined_predictions)

  return(final_predictions)
}
