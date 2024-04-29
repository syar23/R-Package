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
         linear = linear_model(X, y),
         ridge = ridge_model(as.matrix(X), y),
         lasso = lasso_model(as.matrix(X), y),
         "elastic_net" = elastic_net_regression(X, y),
         "random_forest" = rf_model(X, y)
  )
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
  naive.mat <- matrix(1, nrow = r.bagging, ncol = ncol(df.original))
  for (i in 1:r.bagging) {
    # Sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE),]
    # Load and fit the model
    fit <- load_model(df_temp[, -1], df_temp[, 1], mtype)
    if (mtype == "ridge")
      fitCoefs <- coef(fit, alpha = 0)
    else if (mtype == "lasso")
      fitCoefs <- coef(fit, alpha = 1)
    else
      fitCoefs <- coef(fit)
    terms <- which(fitCoefs != 0)
    terms <- terms[terms != 1]
    terms <- terms - 1
    # Predict
    if(mtype == "linear")
      X_temp <- df_temp[, -1]
    else
      X_temp <- as.matrix(df_temp[, -1])
    y.hats[, i] <- predict(fit, X_temp)
    
    if (mtype %in% c("lasso", "elastic_net", "random_forest")) {
      for (j in terms)
        naive.mat[i,j] <- 1
    }
  }
  
  # Process results
  if (is.factor(y)) {
    y.hat = apply(y.hats, 1, function(x) max(as.numeric(names(which.max(table(x))))))
  } else {
    y.hat = apply(y.hats, 1, mean)
  }
  
  if (mtype %in% c("lasso", "elastic_net", "random_forest")){
    return(list(fitted.values = y.hat, naive = apply(naive.mat, 2, sum)))
  } else {
    return(fitted.values = y.hat)
  }
}
