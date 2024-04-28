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
lasso_model <- function(X, y, fold_percentage = 0.2) {
  # Determine the type of response variable
  if (is.factor(y) || all(y %in% c(0, 1))) {
    family_type <- "binomial"
  } else {
    family_type <- "gaussian"
  }
  # Calculate the number of folds based on the fold percentage
  #nfolds <- max(2, round(1 / fold_percentage))
  # Calculate the number of folds ensuring each fold has at least 10 observations
  min_obs_per_fold <- 10
  nfolds <- max(2, min(floor(nrow(X) / min_obs_per_fold), round(1 / 0.2)))
  
  # Perform cross-validation to select the optimal lambda value
  cv_model <- glmnet::cv.glmnet(X, y, family = family_type, nfolds = nfolds)
  
  # Select the lambda value with minimum cross-validated error
  best_lambda <- cv_model$lambda.min
  
  # Fit the final Lasso model with the selected lambda
  model <- glmnet::glmnet(X, y, family = family_type, lambda = best_lambda)
  
  return(model)
}
