#' Ridge Regression
#'
#'
#' @param X matrix of the predictor variables
#' @param y matrix of the output variables
#' @return coefficient matrix of the covariates
#' @export
ridge_model <- function(X, y) {
  
  print("Ridge regression is being done - ")
  # Determine the type of response variable
  family_type <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"
  
  # Perform cross-validation to select the optimal lambda value
  fit <- cv.glmnet(X, y, family = family_type, alpha = 0, nfolds = 10)
  opt_lambda <- fit$lambda.min
  
  # fit the final model
  fit_final <- glmnet(X, y, alpha = 0, lambda = opt_lambda)
  
  return (list(model = fit_final, predicted.value = predict(fit_final, newx = X, type = "response", s = opt_lambda)))
}



