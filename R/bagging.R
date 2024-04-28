bagging.model <- function(X, y, model.type, r.bagging, y_binary){
  #frame a dataset
  o_df = data.frame(y,X)
  model_coeff <- matrix(1,nrow = nR, ncol = ncol(o_df))
  for (i in 1:nR) {
    #sampling from the original dataset
    df <- o_df[sample(1:nrow(o_df), replace = TRUE),]
    #calling the appropriate model
    if(model.type = "linear" && y_binary){
      model_coeff[i,] <- my.glm(df)
    } else if(model.type = "ridge"){
      model_coeff[i,] <- my.ridge(df)
    } else if(model.type = "lasso"){
      model_coeff[i,] <- my.lasso(df)
    } else if(model.type ="elastic.net"){
      model_coeff[i,] <- my.elasticNet(df)
    } else{
      model_coeff[i,] <- my.lm(df)
    }
  }
  
  majority <- function(x){
    max(as.numeric(names(which.max(table(x))))) # needs update
  }
  
  if(y_binary){
    #majority vote for binary response variable
    model <- apply(model_coeff, 2, majority) 
  } else {
    #simple mean
    model <- apply(model_coeff, 2, mean)
  }
  names(model)<- c("(Intercept)", sprintf("x%d", 1:(ncol(o_df)-1)))
  
  return(model)
}