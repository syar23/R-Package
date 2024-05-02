#' Fit a Lasso regression model using glmnet with cross-validation for lambda selection.
#'
#' This function fits a Lasso regression model using the glmnet package with cross-validation to select the optimal lambda value. It automatically detects
#' the type of response variable (binary or continuous) and adjusts the family parameter in the glmnet function call accordingly.
#'
#' @param X Matrix of predictor variables. Each row represents an observation, and each column represents a predictor.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @param fold_percentage Ratio of number of folds to sample size for cross-validation.
#'
#' @return A fitted Lasso regression model object of class "glmnet".
#'
#' @examples
#' # Example usage
#' X <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- sample(0:1, 100, replace = TRUE)
#' lasso_model(X, y, fold_percentage = 0.2)
#'
#' @import glmnet
#' @export
lasso_model <- function(X, y) {
  
  print("Lasso regression is being done - ")
  # Determine the type of response variable
  family_type <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"
  
  # Perform cross-validation to select the optimal lambda value
  fit <- cv.glmnet(X, y, family = family_type, alpha = 1, nfolds = 10)
  opt_lambda <- fit$lambda.min
  
  # fit the final model
  fit_final <- glmnet(X, y, alpha = 1, lambda = opt_lambda)
  
  return (list(model = fit_final, predicted.value = predict(fit_final, newx = X, type = "response", s = opt_lambda)))
}

