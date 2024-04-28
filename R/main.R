# function to get the model outputs
my.models <- function(X, y, model.type, r.bagging, is.ensemble){
  # Create an empty list
  results <- list()
  
  for (mtype in model.type) {
    if(r.bagging > 1){
      source("bagging.R")
      result <- bagged.model(X, y, y_binary, r.bagging, model.type)
    } else {
      # load the appropriate source file
      file.name <- paste0(mtype,".R")
      source(file.name)
      # call the appropriate model
      if(mtype="linear")
        result <- main.linear(X, y, y_binary)
      
      if(mtype="ridge")
        result <- main.ridge(X, y, y_binary)
      
      if(mtype="lasso")
        result <- main.lasso(X, y, y_binary)
      
      if(mtype="elastic.net")
        result <- main.elastic.net(X, y, y_binary)
      
      if(mtype="random.forest")
        result <- main.random.forest(X, y, y_binary)
    }
    
    results[[mtype]] <- result
  }
  
  if(is.ensemble){
    source("ensemble.learning.R")
    results[["ensembled"]] <- main.ensemble.learning(X, y, y_binary)
  }
  
  return(results)
}

# function to check if the provided predictors are good or not.
is.good.predictor <- function(x){
  if(is.numeric(x)){
    return(TRUE)
  } else{
    # Check if all factor column has only two levels
    if(is.factor(x) && length(unique(x))==2)
      return(TRUE)
    else
      return(FALSE)
  }
}


#model.type = c("linear", "ridge", "lasso", "elastic.net", "random.forest") 
simpleEnsembleGroup22 <- function(X, y, model.type = c("elastic.net", "random.forest"), k = ncol(X)/2, r.bagging = 50, is.ensemble = FALSE){
  #Pre-screening to check if provided data is valid
  if(is.null(X) || is.null(y)){
    return("Either X and/or y is NULL")
  }
  # y either binary or continuous -- if cont. it is assumed to be normally dist. 
  y_binary <- FALSE
  if(is.factor(y) && length(unique(y))==2)
    y_binary <- TRUE
  else if (!is.numeric(y))
    return("The response varibale must be numeric: either binary or continuous!")
  
  # Identify if all column's data types are acceptable: X can be continuous, discrete, binary
  is.good.data <- sapply(X, is.good.predictor)
  if(sum(is.good.data) < length(is.good.data))
    return("The predictor variable must be numeric: binary, discrete, or continuous.")
  
  #variable pre-screening
  source("variable screening.R")
  X <- variable_pre_screening(X, y, k)
  return(my.models(X, y, model.type, r.bagging, is.ensemble))
}


