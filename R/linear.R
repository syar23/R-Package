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
  df <- data.frame(y, X)
  if (is.factor(y)) {
    fit <- glm(y ~ ., data = df, family = "binomial")  # logistic regression
  } else {
    fit <- lm(y ~ ., data = df)  # linear regression
  }
  return(fit)
}
