#' Ensemble Learning Prediction
#'
#' This function performs ensemble learning using Random Forest and Elastic Net
#' as base learners. It trains on 70% of the data and tests on the remaining 30%. The function returns predictions
#' and accuracies for individual models as well as the ensemble model using a simple mean approach.
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return A list containing the predictions and accuracies of the individual models and the ensemble model.
#'         - ensembled_prediction: The ensemble prediction.
#'         - elastic.net: List containing information about the Elastic Net model, including predicted values.
#'         - random.forest: List containing information about the Random Forest model, including predicted values.
#' Example usage:
#' X <- matrix(rnorm(1000 * 2), ncol = 2)
#' y <- rnorm(1000)
#' result <- ensemble_predict(X, y)
#'
#' @export
ensemble_predict <- function(X, y) {
  e.net <- elastic_net_regression(X, y)
  r.forest <- rf_model(X, y)

  data.mat <- cbind(x2 = e.net$predicted.value, x3 = r.forest$predicted.value)
  if (all(y %in% c(0, 1))){
    data.mat <- round(data.mat)
    y.hat <- round(apply(data.mat, 1, function(x) max(as.numeric(names(which.max(table(x)))))))
  } else {
    y.hat <- apply(data.mat, 1, mean)
  }
  return(list(ensembled_prediction = y.hat, elastic.net = e.net, random.forest = r.forest))
}
