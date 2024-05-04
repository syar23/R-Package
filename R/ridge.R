#' Ridge Regression
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return A list containing the fitted ridge regression model and predicted values.
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100 * 2), ncol = 2)
#' y <- rnorm(100)
#' result <- ridge_model(X, y)
#'
#' @import glmnet
#' @export
ridge_model <- function(X, y) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  print("Ridge regression is being done - ")

  # Determine the type of response variable
  family_type <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  # Perform cross-validation to select the optimal lambda value
  fit <- glmnet::cv.glmnet(X, y, family = family_type, alpha = 0, nfolds = 10)
  opt_lambda <- fit$lambda.min

  # fit the final model
  fit_final <- glmnet::glmnet(X, y, alpha = 0, lambda = opt_lambda)

  return(list(model = fit_final, predicted.value = predict(fit_final, newx = X, type = "response", s = opt_lambda)))
}
