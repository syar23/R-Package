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
         "elastic_net" = elastic_net_regression(as.matrix(X), y)
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
  if (mtype == "random_forest"){
    stop("Random Forest is a bagging process! Further bagging is not allowed.")
  }
  
  # Frame a dataset
  df.original <- data.frame(y, X)
  y.hats <- matrix(1, nrow = nrow(df.original), ncol = r.bagging)
  naive.mat <- matrix(0, nrow = r.bagging, ncol = ncol(df.original))
  for (i in 1:r.bagging) {
    # Sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE),]
    # Load and fit the model
    fit <- load_model(df_temp[, -1], df_temp[, 1], mtype)
    # Prediction
    y.hats[, i] <- fit$predicted.value
    
    if(mtype == "ridge") next
    
    if(mtype == "linear") {
      fitCoefs <- summary(model)$coefficients
      terms <- which(fitCoefs[, "Pr(>|z|)"] < 0.05)
    } else if (mtype == "lasso"){
      fitCoefs <- coef(fit$model, alpha = 1)
      terms <- which(fitCoefs != 0)
    } else {
      fitCoefs <- coef(fit$model)
      terms <- which(fitCoefs != 0)
    }
    
    terms <- terms[terms != 1]
    terms <- terms - 1
    naive.mat[i,terms] <- 1
  }
  
  # Process results
  if (all(y %in% c(0, 1))) {
    y.hat = apply(y.hats, 1, function(x) max(as.numeric(names(which.max(table(x))))))
  } else {
    y.hat = apply(y.hats, 1, mean)
  }
  
  if (mtype == "ridge"){
    return(y.hat)
  } else {
    return(list(fitted.values = y.hat, naive = apply(naive.mat, 2, sum)))
  }
}
