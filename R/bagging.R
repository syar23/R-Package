#' Load Model Function
#'
#' This function loads the appropriate model based on the specified type.
#'
#' @param X Independent variables (predictors).
#' @param y Dependent variable (response).
#' @param modelType Type of machine learning model (linear, ridge, lasso, elastic net, random forest).
#' @return Fitted model.
load_model <- function(X, y, modelType) {
  switch(modelType,
    linear = linear_model(X, y),
    ridge = ridge_model(X, y),
    lasso = lasso_model(X, y),
    elastic_net = elastic_net_regression(X, y)
  )
}

#' Bagged Model Function
#'
#' This function performs bagging with different machine learning models.
#'
#' @param X Independent variables (predictors).
#' @param y Dependent variable (response).
#' @param r.bagging Number of bagging iterations.
#' @param modelType Type of machine learning model (linear, ridge, lasso, elastic net, random forest).
#' @return If the response variable is factor, returns a list with fitted values and naive variables;
#' otherwise, returns fitted values.
#' @export
bagged_model <- function(X, y, r.bagging, modelType) {
  if (modelType == "random_forest") {
    print("Random Forest is a bagging process! Further bagging is not allowed.")
    return(NULL)
    }

  # Frame a dataset
  df.original <- data.frame(y, X)
  y.hats <- matrix(1, nrow = nrow(df.original), ncol = r.bagging)
  naive.mat <- matrix(0, nrow = r.bagging, ncol = ncol(X))
  for (i in 1:r.bagging) {

    # Sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE), ]

    # Load and fit the model
    print(modelType)
    fit <- load_model(df_temp[, -1], df_temp[, 1], modelType)

    # Prediction
    y.hats[, i] <- fit$predicted.value

    if (modelType == "ridge") next

    if (modelType == "linear") {
      fitCoefs <- summary(fit$model)$coefficients
      terms <- which(fitCoefs[, ncol(fitCoefs)] < 0.05)
    } else if (modelType == "lasso") {
      fitCoefs <- coef(fit$model, alpha = 1)
      fitCoefs_numeric <- as.numeric(as.matrix(fitCoefs))
      terms <- which(fitCoefs_numeric != 0)

    } else {
      print("elastic net bagging - ")
      fitCoefs <- coef(fit$model)
      fitCoefs_numeric <- as.numeric(as.matrix(fitCoefs))
      terms <- which(fitCoefs_numeric != 0)

    }

    terms <- terms[terms != 1]
    terms <- terms - 1
    naive.mat[i, terms] <- 1
  }

  # Process results
  if (all(y %in% c(0, 1))) {
    y.hat <- apply(y.hats, 1, function(x) max(as.numeric(names(which.max(table(x))))))
  } else {
    y.hat <- apply(y.hats, 1, mean)
  }

  if (modelType == "ridge") {
    return(y.hat)
  } else {
    return(list(fitted.values = y.hat, naive = apply(naive.mat, 2, sum)))
  }
}
