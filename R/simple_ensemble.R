#' Simple Ensemble Learning
#'
#' This function performs ensemble learning using a combination of linear models (such as Elastic Net) and tree-based models (such as Random Forest).
#' It conducts preliminary data screening, trains multiple models, and optionally performs ensemble learning.
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. Can be continuous and normally distributed or binary responses.
#' @param models A character vector specifying the types of models to include in the ensemble. Options include "elastic_net"
#'  and "random_forest". They are the default option too.
#' @param k Top number of predictors to select in the pre-screening step. Default is NULL.
#' @param r.bagging Number of times model will perform sampling with replacement for the samples. Default is 50.
#' @param is.ensemble Logical indicating whether to perform ensemble learning of models passed as parameter.
#'
#' @return A list containing the results of the combined ensemble learning process(That is, with or without bagging,
#'  with or without the pre screening for Top K predictors, with or without combined predictions of models)
#'
#' @export
#' @examples
#' # Example usage:
#' set.seed(123)
#' n <- 1000
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' y <- sample(0:1, n, replace = TRUE)
#'
#' # Call simple_ensemble function with Elastic Net model with R parameter for bagging
#' results <- simple_ensemble(X, y, models = c("elastic_net"), r.bagging = 50, is.ensemble = FALSE, k = NULL)
simple_ensemble <- function(X, y, models = c("elastic_net"), r.bagging = NULL, is.ensemble = FALSE, k = NULL) {

  # variable pre-screening
  X <- variable_pre_screening(X, y, k)


  # If y is a factored variable, convert it into a binary variable
  if (is.factor(y))
    y <- as.numeric(y) - 1

  # Process X for categorical/non-numeric variable
  if (!all(sapply(X, function(x) is.numeric(x)))) {
    df <- data.frame(y, X)
    X <- model.matrix(y ~ ., df)[, -1]
  }

  results <- list()

  for (modeltype in models) {

    #Second check on size of sample vs the number of predictors
    if(modeltype == "linear" && ncol(X)>nrow(X)) {
      print("For linear regression the number of predictors has to be less than sample size. Choose k less than sample size.")
      return(NULL)
    }
    else if(modeltype != "linear" && nrow(X)<200) {
      print(str(X))
      print("Sample size has to be above 200 for the specified models to fit the data.")
      return(NULL)
    }

    if (!is.null(r.bagging)) {
      print("Bagging is being done - ")
      result <- bagged_model(X, y, r.bagging, modeltype)
    } else {
      print(modeltype)
      # call the appropriate model using switch
      result <- switch(modeltype,
        linear = linear_model(X, y),
        ridge = ridge_model(X, y),
        lasso = lasso_model(X, y),
        elastic_net = elastic_net_regression(X, y),
        random_forest = rf_model(X, y)
      )
    }
    results[[modeltype]] <- result
  }

  if (is.ensemble) {
    print("Ensemble is being done - ")
    results[["ensembled"]] <- ensemble_predict(X, y)
  }

  return(results)
}
