#linear Model
main.linear <- function(X, y){
  df = data.frame(y,X)
  if(is.factor(y))
    fit <- glm(y ~ ., data = df, family = "binomial") # logistic regression
  else
    fit <- lm(y~., data=df) # linear regression
  return(fit)
}
