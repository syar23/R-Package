#' Fit a Random Forest model using the randomForest package.
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return A fitted Random Forest model object of class "randomForest".
#'
#' @examples
#' # Example usage for classification
#' X <- matrix(rnorm(100 * 2), ncol = 2)
#' y <- sample(c(0, 1), 100, replace = TRUE)
#' rf_model <- rf_model(X, y)

#' # Example usage for regression
#' X <- matrix(rnorm(100 * 3), ncol = 3)
#' y <- rnorm(100)
#' rf_model <- rf_model(X, y)
#'
#' @import randomForest
#' @export
rf_model <- function(X, y) {

  # Ensure X is a matrix or data frame
  if (!is.matrix(X) && !is.data.frame(X)) {
    print("X should be a matrix or a data frame.")
    return(NULL)
  }

  print("Random Forest method is being applied - ")

  # If the response variable is binary, RF model doesn't recognize it unless it's a factored variable
  # Hence convert y into a factored variable  -- user may use 0, 1 numeric input or a factored variable both are fine.
  if (all(y %in% c(0, 1))) y <- as.factor(y)

  # Determine the type of response variable
  model_type <- if (is.factor(y)) "classification" else "regression"

  # Set default mtry if not provided
  #if (is.null(mtry)) {
    mtry <- if (model_type == "classification") sqrt(ncol(X)) else ncol(X) / 3
    mtry <- floor(mtry)
  #}

  # Fit the Random Forest model
  rf_model <- randomForest::randomForest(X, y, ntree = 100, mtry = mtry, importance = TRUE)

  return(rf_model)
}
