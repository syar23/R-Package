#linear Model
main.linear <- function(X, y){
  df = data.frame(y,X)
  if(is.factor(y))
    fit <- glm(y ~ ., data = df, family = "binomial") # logistic regression
  else
    fit <- lm(y~., data=df) # linear regression
  return(fit)
}
#Do we polynomial regression?


# bagged.linear <- function(X, y){
#   fit <- lm(y~., data=df)
#   return(fit$coefficients)
# }
# 
# bagged.linear <- function(df){
#   fit <- glm(y ~ ., data = df, family = "binomial")
#   return(fit$coefficients)
# }