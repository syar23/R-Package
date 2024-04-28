# DOES DISCRETE VARIABLE AFFECT THE MODEL DEVELOPMENT??

# @params
# is.ensemble = False
# is.bagging = 50
# k = p/2
# -- y can't be categorical, only binary or continuous y is allowed -- Variable screening
#   
#   
#   check r.bagging value once.
#   check if k value works
#   


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
#Q1: Is ensemble only ML & ENet or else?
#Q2. model type - multiple or single at once?
#Q3: is it sufficient to show only the coefficient of the explanatory variables or we need more?
#Q4. Are we impute missing value or ignore them?
#Q5: 


simpleEnsembleGroup22 <- function(X, y, model.type = c("elastic net", "random forest") ,k = ncol(X)/2, r.bagging = 50, is.ensemble = FALSE){ #method.ensemble = c("")
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
  X <- variable_pre_screening(X, y, k)
  
}


