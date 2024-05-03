#' Elastic Net Regression with Cross-Validated Alpha Selection
#'
#' Fits an elastic net regression model with cross-validated alpha selection.
#'
#' @param X Matrix of predictors (independent variables), can be a mix of continuous, discrete, and binary predictors
#' @param y Response variable (binary or continuous)
#' @param alphas Vector of alpha values to try
#' @param method Method for regression ('glmnet')
#' @param ... Additional arguments to be passed to the regression method
#' @return Fitted model object
#' @examples
#' X <- {}
#' y <- {}
#' model <- elastic_net_regression(X, y, alphas = seq(0, 1, by = 0.05), method = 'glmnet')
#' @export
elastic_net_regression <- function(X, y) {
  if(!is.matrix(X)){
    X <- as.matrix(X)
  }
  
  # Determine the type of response variable
  family_type <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  alph_lamb <- data.frame(matrix(ncol=3, nrow=0))
  alphavec = seq(0,1,0.1)
  for (i in 1:length(alphavec)){
    # Fit the elastic net using cross-validation
    fit <- cv.glmnet(X, y, family = family_type, alpha=alphavec[i], nfolds = 10)
    
    min_mse <- min(fit$cvm)
    lam <- fit$lambda[which(fit$cvm == min_mse)]
    alph_lamb <- rbind(alph_lamb, c(alphavec[i], lam, min_mse))
  }
  colnames(alph_lamb) <- c("alpha", "lambda", "cvm")
  min_cvm <- min(alph_lamb$cvm)
  opt_alpha <- alph_lamb[which(alph_lamb$cvm == min_cvm),1]
  opt_lambda <- alph_lamb[which(alph_lamb$cvm == min_cvm),2]
  fit_final <- glmnet(X,y, family = family_type, alpha = opt_alpha, lambda = opt_lambda)
  return(list(model = fit_final, optimal.alpha = opt_alpha, predicted.value = predict(fit_final, newx = X, type = "response", s = opt_lambda)))
}
