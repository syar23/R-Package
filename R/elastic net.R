#' Elastic Net Regression with Cross-Validated Alpha Selection
#'
#' Fits an elastic net regression model with cross-validated alpha selection.
#'
#' @param X Matrix of predictors (independent variables), can be a mix of continuous, discrete, and binary predictors
#' @param y Response variable (binary or continuous)
#' @param alphas Vector of alpha values to try
#' @param lambdas Regularization parameter
#' @param method Method for regression ('glmnet')
#' @param K Number of top predictors to include in the model
#' @param ... Additional arguments to be passed to the regression method
#' @return Fitted model object
#' @examples
#' X <- {}
#' y <- {}
#' model <- elastic_net_regression(X, y, method = 'glmnet', K = 10)
#' @export
elastic_net_regression <- function(X, y, alphas = NULL, lambdas = NULL, method = "glmnet", K = NULL, ...) {
  require(glmnet)
  
  if (is.null(alphas)) {
    alphas <- seq(0, 1, by = 0.05)
  }
  
  min_mse <- Inf
  selected_alpha <- NULL
  
  for (alpha in alphas) {
    # Fit elastic net regression model
    if (!is.null(K)) {
      fit <- elastic_net_regression_helper(X, y, alpha = alpha, method = method, K = K, ...)
    } else {
      fit <- elastic_net_regression_helper(X, y, alpha = alpha, method = method, ...)
    }
    
    # Make predictions
    predictions <- predict(fit, newx = X)
    
    # Calculate MSE
    mse <- mean((predictions - y)^2)
    
    # Update selected alpha if MSE is lower
    if (mse < min_mse) {
      min_mse <- mse
      selected_alpha <- alpha
    }
  }
  
  # Final fit with selected alpha
  if (!is.null(K)) {
    final_fit <- elastic_net_regression_helper(X, y, alpha = selected_alpha, method = method, K = K, ...)
  } else {
    final_fit <- elastic_net_regression_helper(X, y, alpha = selected_alpha, method = method, ...)
  }
  
  return(final_fit)
}

elastic_net_regression_helper <- function(X, y, alpha, method, K, ...) {
  if (is.null(method) | method == "glmnet") {
    # If y is binary, convert it to a factor
    if (is.factor(y)) {
      y <- as.numeric(y) - 1
    }
    # Fit elastic net regression model
    fit <- glmnet(X, y, alpha = alpha, ...)
  } else {
    stop("Invalid method. Supported methods are 'glmnet'")
  }
  
  return(fit)
}
