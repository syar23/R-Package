#' Ensemble Learning Prediction
#'
#' This function performs ensemble learning using Random Forest, GLM (Linear and Logistic Regression), and Elastic Net
#' as base learners. It synthesizes predictions using either a simple mean or a weighted mean approach.
#'
#' @param x Matrix of predictors (independent variables), can be a mix of
#' continuous, discrete, and binary predictors.
#' @param y Response variable (binary or continuous).
#' @param data Your dataset containing both 'x' and 'y'.
#' @param weights Optional vector of weights for the weighted average of predictions. If NULL,
#' a simple average is used. Length of weights must match the number of base models.
#'
#' @return A vector of predictions from the ensemble model.
#' @export
#'
#' @examples
#' data(iris)
#' preds <- ensemble_predict(x = iris[,1:4], y = iris$Species, data = iris)
ensemble_predict <- function(x, y, data, weights = NULL) {
  # Load required libraries
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Please install the 'randomForest' package.")
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("Please install the 'glmnet' package.")
  
  # Check input types
  if (!is.matrix(x) || !is.data.frame(x)) stop("x must be a matrix or data frame.")
  if (!is.vector(y) || is.matrix(y)) stop("y must be a vector.")
  
  # Prepare response type check for GLM
  is_binary <- length(unique(y)) == 2
  response_type <- if (is_binary) "binomial" else "gaussian"
  
  # Models setup
  set.seed(123) # for reproducibility
  models <- list(
    "RandomForest" = randomForest::randomForest(x, y, data = data, ntree = 100),
    "GLM" = stats::glm(y ~ ., data = data, family = response_type),
    "ElasticNet" = glmnet::cv.glmnet(as.matrix(x), y, family = response_type)
  )
  
  # Collect predictions
  predictions <- lapply(models, function(model, data) {
    if (inherits(model, "randomForest")) {
      predict(model, newdata = data)
    } else if (inherits(model, "glm")) {
      predict(model, newdata = data, type = "response")
    } else if (inherits(model, "cv.glmnet")) {
      predict(model, newx = as.matrix(data), s = "lambda.min", type = "response")
    }
  }, data = data)
  
  # Combine predictions
  combined_predictions <- do.call("cbind", predictions)
  
  # Apply weights
  if (is.null(weights)) {
    result <- rowMeans(combined_predictions)
  } else {
    if (length(weights) != length(predictions)) stop("Length of weights must match the number of models.")
    result <- rowSums(combined_predictions * weights) / sum(weights)
  }
  
  return(result)
}
