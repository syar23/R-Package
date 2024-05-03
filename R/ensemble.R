#' Ensemble Learning Prediction
#'
#' This function performs ensemble learning using Random Forest and Elastic Net (GLM via glmnet)
#' as base learners. It trains on 70% of the data and tests on the remaining 30%. The function returns predictions
#' and accuracies for individual models as well as the ensemble model using either a simple mean or a weighted mean approach.
#'
#' @param x Matrix of predictors (independent variables), can be a mix of
#' continuous, discrete, and binary predictors.
#' @param y Response variable (binary or continuous).
#' @param data Your dataset containing both 'x' and 'y'.
#' @param weights Optional vector of weights for the weighted average of predictions.
#' If NULL, a simple average is used. Length of weights must match the number of base models.
#'
#' @return A list containing the predictions and accuracies of the individual models and the ensemble model.
#' @export
#' @examples
#'
#' # Example usage:
#' # ensemble_predict(x, y, data)
#'
# Ensemble prediction function definition
ensemble_predict <- function(X, y) {
  
  source("linear.R")
  l.model <- linear_model(X,y)
  source("elastic_net.R")
  e.net <- elastic_net_regression(X,y)
  source("random_forest.R")
  r.forest <- rf_model(X,y)
  
  data.mat <- cbind(x1 = l.model$predicted.value, x2 = e.net$predicted.value, x3 = r.forest$predicted.value)
  if (all(y %in% c(0, 1))){
    data.mat <- round(data.mat)
    y.hat <- round(apply(data.mat, 1, function(x) max(as.numeric(names(which.max(table(x)))))))
  } else {
    #find optimal weight by estimating the MSE
    # mse.values <- c(mean((l.model$predicted.value - y)^2), mean((e.net$predicted.value - y)^2), mean((r.forest$predicted.value - y)^2))
    # reciprocal.mses <- 1/mse.values
    # weight <- reciprocal.mses/sum(reciprocal.mses)
    # y.hat <- weight*t(data.mat)
    y.hat <- apply(data.mat, 1, mean)
  }
  return(list(ensembled_prediction = y.hat, linear = l.model, elastic.net = e.net, random.forest = r.forest)) #weight = weight,
}
