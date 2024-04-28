bagged.model <- function(X, y, r.bagging, mtype){
  #frame a dataset
  df.original = data.frame(y,X)
  y.hats <- matrix(1, nrow = nrow(df.original), ncol = r.bagging)
  for (i in 1:r.bagging) {
    # sampling from the original data set
    df_temp <- df.original[sample(1:nrow(df.original), replace = TRUE),]
    
    # load the appropriate source file
    file.name <- paste0(mtype,".R")
    source(file.name)
    # call the appropriate model
    if(mtype=="linear")
      fit <- main.linear(X, y)
    else if(mtype=="ridge")
      fit <- ridge_model(X, y)
    else if(mtype=="lasso")
      fit <- lasso_model(X, y)
    else if(mtype=="elastic.net")
      fit <- elastic_net_regression(X, y)
    else if(mtype=="random.forest")
      fit <- rf_model(X, y)
    
    if(mtype=="linear")
      X <- data.frame(X)
    if(mtype=="ridge" || mtype=="lasso")
      X <- as.matrix(X)
      
    y.hats[,i] <- predict.glm(fit,X)
  }
  
  majority <- function(x){
    max(as.numeric(names(which.max(table(x)))))
  }
  
  if(is.factor(y))
    return(apply(y.hats, 1, majority)) # majority vote for binary response variable
  else
    return(apply(y.hats, 1, mean)) # simple mean
}


#implementation
df <- data.matrix(cats)
colnames(df) <- c("y", sprintf("x%d", 1:(ncol(df)-1)))
X <- df[,2:3]
y <- df[,1]
y[y==2] <- 0
y_pred <- bagged.model(X,y,10,"elastic.net")
y_pred
