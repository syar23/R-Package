#' Ridge Regression
#'
#'
#' @param X matrix of the predictor variables
#' @param y matrix of the output variables
#' @return coefficient matrix of the covariates
#' @export
ridge_model <- function(X, y) {

  # Perform cross-validation to select the optimal lambda value
  cv_model <- glmnet::cv.glmnet(as.matrix(X), y, alpha = 0)
  best_lambda <- cv_model$lambda.min

  best_model <- glmnet::glmnet(as.matrix(X), y, alpha = 0, lambda = best_lambda)

  return (coef(best_model))

}
