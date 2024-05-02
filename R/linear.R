#' Linear Model Function
#'
#' This function fits a linear model using either linear regression for continuous response variables
#' or logistic regression for binary response variables.
#'
#' @param X Independent variables (predictors).
#' @param y Dependent variable (response).
#' @return Fitted linear model object.
#' @examples
#'
#' # Example usage:
#' # linear_model(X, y)
#'
#' @export
linear_model <- function(X, y) {
  
  print("Linear regression is being done - ")
  if (all(y %in% c(0, 1))) {
    fit <- glm(y ~ ., data = data.frame(y, X), family = "binomial")  # logistic regression
  } else {
    fit <- lm(y ~ ., data = data.frame(y, X))  # linear regression
  }
  
  return(list(model = fit, predicted.value = predict(fit, newx = data.frame(X), type = "response")))
}
