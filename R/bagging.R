bagged.model <- function(X, y, r.bagging, mtype){
  #frame a dataset
  df.original = data.frame(y,X)
  y.hats <- matrix(1, nrow = nrow(df.original), ncol = r.bagging)
  naive.mat <- matrix(1, ncol = ncol(df.original) + 1, nrow = r.bagging)
  for (i in 1:r.bagging) {
    # sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE),]
    
    # load the appropriate source file
    file.name <- paste0(mtype,".R")
    source(file.name)
    # call the appropriate model
    if(mtype=="linear"){
      fit <- main.linear(X, y)
      X <- data.frame(X)
    } else if(mtype=="ridge"){
      fit <- ridge_model(X, y)
      X <- as.matrix(X)
    } else if(mtype=="lasso"){
      fit <- lasso_model(X, y)
      X <- as.matrix(X)
      fitCoefs <- coef(fit)
      terms <- which(fitCoefs != 0)
    } else if(mtype=="elastic net"){
      fit <- elastic_net_regression(X, y)
      fitCoefs <- coef(fit)
      terms <- which(fitCoefs != 0)
    } else if(mtype=="random forest"){
      fit <- rf_model(X, y)
      fitCoefs <- coef(fit)
      terms <- which(fitCoefs != 0)
    }

    y.hats[,i] <- predict(fit,X)
    if(mtype=="lasso" || mtype=="elastic net" || mtype=="random forest"){
      for (j in terms)
        naive.mat[j,i] <- 1
    }
  }
  
  majority <- function(x){
    max(as.numeric(names(which.max(table(x)))))
  }
  
  if(mtype=="lasso" || mtype=="elastic net" || mtype=="random forest")
    apply(naive.mat, 2, sum)
  
  if(is.factor(y)){
    # majority vote for binary response variable & naive var for three models
    if(mtype=="lasso" || mtype=="elastic net" || mtype=="random forest")
      return(list(fitted.values = apply(y.hats, 1, majority), naive = apply(naive.mat, 2, sum)))
    else
      return(fitted.values = apply(y.hats, 1, majority)) # majority vote for binary response variable
  } else{
    # simple mean & naive var for three models
    if(mtype=="lasso" || mtype=="elastic net" || mtype=="random forest")
      return(list(fitted.values = apply(y.hats, 1, mean), naive = apply(naive.mat, 2, sum)))
    else
      return(fitted.values = apply(y.hats, 1, mean)) # simple mean
  }
}


#implementation
# df <- data.matrix(cats)
# colnames(df) <- c("y", sprintf("x%d", 1:(ncol(df)-1)))
# X <- df[,2:3]
# y <- df[,1]
# y[y==2] <- 0
# 
# X <- cement[,1:4]
# y <- cement[,5]
# y_pred <- bagged.model(X,y,10,"elastic net")
# y_pred
#"C:/Users/hmoon/Desktop/AMS/AMS 597/Group Project/",