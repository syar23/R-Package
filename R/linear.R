#' Linear Model Function
#'
#' This function fits a linear model using either linear regression for continuous response variables
#' or logistic regression for binary response variables.
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return A list containing the fitted model and predicted values.
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100), ncol = 2)
#' y <- rnorm(100)
#' model <- linear_model(X, y)
#'
#' @export
linear_model <- function(X, y) {

  print("Linear regression is being done - ")
  if (all(y %in% c(0, 1))) {
    fit <- glm(y ~ ., data = data.frame(y, X), family = "binomial") # logistic regression
  } else {
    fit <- lm(y ~ ., data = data.frame(y, X)) # linear regression
  }

  return(list(model = fit, predicted.value = predict(fit, newx = data.frame(X), type = "response")))
}

