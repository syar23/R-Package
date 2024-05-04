#' Elastic Net Regression with Cross-Validated Alpha Selection
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return A list containing the fitted model object, optimal alpha, and predicted values.
#' @examples
#' # Generate some sample data
#' set.seed(123)
#' n <- 1000
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' beta <- rnorm(p)
#' y <- X %*% beta + rnorm(n)
#'
#' # Fit elastic net regression model
#' result <- elastic_net_regression(X, y)
#' @import glmnet
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
    fit <- glmnet::cv.glmnet(X, y, family = family_type, alpha=alphavec[i], nfolds = 10)

    min_mse <- min(fit$cvm)
    lam <- fit$lambda[which(fit$cvm == min_mse)]
    alph_lamb <- rbind(alph_lamb, c(alphavec[i], lam, min_mse))
  }
  colnames(alph_lamb) <- c("alpha", "lambda", "cvm")
  min_cvm <- min(alph_lamb$cvm)
  opt_alpha <- alph_lamb[which(alph_lamb$cvm == min_cvm),1]
  opt_lambda <- alph_lamb[which(alph_lamb$cvm == min_cvm),2]
  fit_final <- glmnet::glmnet(X,y, family = family_type, alpha = opt_alpha, lambda = opt_lambda)
  return(list(model = fit_final, optimal.alpha = opt_alpha, predicted.value = predict(fit_final, newx = X, type = "response", s = opt_lambda)))

}
