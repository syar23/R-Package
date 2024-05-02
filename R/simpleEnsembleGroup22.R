#' Simple Ensemble Learning
#'
#' This function performs ensemble learning using a combination of linear models (such as Elastic Net) and tree-based models (such as Random Forest).
#' It conducts preliminary data screening, trains multiple models, and optionally performs ensemble learning.
#'
#' @param X Matrix of predictors (independent variables).
#' @param y Response variable (dependent variable).
#' @param models A character vector specifying the types of models to include in the ensemble. Options include "elastic.net" and "random.forest".
#' @param k Number of predictors to select in the pre-screening step. Default is half the number of predictors.
#' @param r.bagging Number of times to perform bagging for each model. Default is 50.
#' @param is.ensemble Logical indicating whether to perform ensemble learning. Default is FALSE.
#'
#' @return A list containing the results of the ensemble learning process.
#'
#' The returned list includes the results of training and evaluating individual models specified in 'models', as well as the ensemble model if 'is.ensemble' is TRUE.
#'
#' @export
#' @examples
#' # Example usage:
#' results <- simple_ensemble_group_22(X, y, models = c("elastic net", "random.forest"), r.bagging = NULL, is.ensemble = TRUE, k=NULL, var.prescreen = TRUE)
simple_ensemble_group_22 <- function(X, y, models = c("elastic_net", "random_forest"), r.bagging = NULL, is.ensemble = FALSE, k=NULL, is.prescreen = TRUE) {

  #Pre-screening to check if provided data is valid
  if(is.null(X) || is.null(y)){
    return("Either X and/or y is NULL")
  }
  # y either binary or continuous -- if cont. it is assumed to be normally dist.

  if (!is.numeric(y) || (is.factor(y) && length(unique(y))>2) )
    return("The response variable must be numeric: either binary or continuous!")

  # Identify if all column's data types are acceptable: X can be continuous, discrete, binary
  if (!all(sapply(X, function(x) is.numeric(x) || (is.factor(x) && length(unique(x)) == 2)))) {
    return("The predictor variable must be numeric: binary, discrete, or continuous.")
  }
  
  #check if data supports the model
  if(!is.prescreen && (models == "linear") && nrow(X) < ncol(X))
    return("Linear model with all the variables is not supported when sample size is less then the number of predictors")

  #variable pre-screening
  if(is.prescreen){
    source("variable_screening.R")
    X <- variable_pre_screening(X, y, k)
  }
  
  # If y is a factored variable, convert it into a binary variable
  if (is.factor(y)) {
    y <- as.numeric(y) - 1
  }
  # Process X for categorical variable
  if (!all(sapply(X, function(x) is.numeric(x)))) {
    df <- data.frame(y, X)
    X <- model.matrix(y~.,df)[,-1]
  }
  
  results <- list()

  for (modeltype in models) {
    if (!is.null(r.bagging)) {
      print("Bagging is being done - ")
      source("bagging.R")
      result <- bagged_model(X, y, r.bagging, models)
    } else {
      # load the appropriate source file
      print("List of models given - ")
      print(modeltype)
      file.name <- paste0(modeltype,".R")
      source(file.name)
      print(file.name)
      # call the appropriate model using switch
      result <- switch(modeltype,
                       linear = linear_model(X, y),
                       ridge = ridge_model(as.matrix(X), y),
                       lasso = lasso_model(as.matrix(X), y),
                       elastic_net = elastic_net_regression(as.matrix(X), y),
                       random_forest = rf_model(X, y))
    }
    results[[modeltype]] <- result
  }

  if (is.ensemble) {
    print("Ensemble is being done - ")
    source("ensemble.R")
    results[["ensembled"]] <- ensemble_predict(X, y)
  }

  return(results)
}

