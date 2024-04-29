#' Load Model Function
#'
#' This function loads the appropriate model based on the specified type.
#'
#' @param X Independent variables (predictors).
#' @param y Dependent variable (response).
#' @param mtype Type of machine learning model (linear, ridge, lasso, elastic net, random forest).
#' @return Fitted model.
load_model <- function(X, y, mtype) {
  file.name <- paste0(mtype, ".R")
  source(file.name)
  switch(mtype,
         linear = main.linear(X, y),
         ridge = ridge_model(as.matrix(X), y),
         lasso = lasso_model(as.matrix(X), y),
         "elastic net" = elastic_net_regression(X, y),
         "random forest" = rf_model(X, y)
  )
}

#' Process Model Results Function
#'
#' This function processes the results of the fitted model based on the specified type.
#'
#' @param fitted Fitted values.
#' @param mtype Type of machine learning model (linear, ridge, lasso, elastic net, random forest).
#' @param terms Terms.
#' @param y.hats Predicted values.
#' @return Processed results.
process_results <- function(fitted, mtype, terms, y.hats) {
  if (mtype %in% c("lasso", "elastic net", "random forest")) {
    naive.mat <- matrix(1, ncol = length(fitted) + 1, nrow = nrow(y.hats))
    for (j in terms)
      naive.mat[j, ] <- 1
    list(fitted.values = fitted, naive = apply(naive.mat, 2, sum))
  } else {
    fitted
  }
}

#' Bagged Model Function
#'
#' This function performs bagging with different machine learning models.
#'
#' @param X Independent variables (predictors).
#' @param y Dependent variable (response).
#' @param r.bagging Number of bagging iterations.
#' @param mtype Type of machine learning model (linear, ridge, lasso, elastic net, random forest).
#' @return If the response variable is factor, returns a list with fitted values and naive variables; 
#' otherwise, returns fitted values.
#' @export
bagged_model <- function(X, y, r.bagging, mtype) {
  # Frame a dataset
  df.original <- data.frame(y, X)
  y.hats <- matrix(1, nrow = nrow(df.original), ncol = r.bagging)
  for (i in 1:r.bagging) {
    # Sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE),]
    # Load and fit the model
    fit <- load_model(df_temp[, -1], df_temp[, 1], mtype)
    # Predict
    y.hats[, i] <- predict(fit, df_temp[, -1])
  }
  
  # Process results
  if (is.factor(y)) {
    process_results(apply(y.hats, 1, function(x) max(as.numeric(names(which.max(table(x)))))), mtype, NULL, y.hats)
  } else {
    process_results(apply(y.hats, 1, mean), mtype, NULL, y.hats)
  }
}
