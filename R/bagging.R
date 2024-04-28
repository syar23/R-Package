#nresample = ?
#sampling R times:
#fitted R model
#how to combine these -- simple avg, weighted avg., majority vote (logistic - 0/1 binary) -- describe in document.


library(MASS)
#Data for linear regression
X <- cement[,1:4]
y <- cement[,5]
#Data for logistic regression
df <- data.matrix(cats)
colnames(df) <- c("y", sprintf("x%d", 1:(ncol(df)-1)))
X <- df[,2:3]
y <- df[,1]
y[y==2] <- 0


#linear Model
my.lm <- function(df){
  fit <- lm(y~., data=df)
  return(fit$coefficients)
}

#polynomial regression --- do we need it? If so, will add later

#logistic regression
my.glm <- function(df){
  fit <- glm(y ~ ., data = df, family = "binomial")
  return(fit$coefficients)
}


#ridge regression

#lasso regression

#elastic net

#which model?
is_lm <- 1
is_log <- 0
is_ridge <- 0
is_lasso <- 0
is_elastic <- 0

nR <- 10
o_df = data.frame(y,X)
model_coeff <- matrix(1,nrow = nR, ncol = ncol(o_df))
for (i in 1:nR) {
  df <- o_df[sample(1:nrow(o_df), replace = TRUE),]
  if(is_log){
    model_coeff[i,] <- my.glm(df)
  } else if(is_ridge){
    model_coeff[i,] <- my.ridge(df)
  } else if(is_lasso){
    model_coeff[i,] <- my.lasso(df)
  } else if(is_elastic){
    model_coeff[i,] <- my.elasticNet(df)
  } else{
    model_coeff[i,] <- my.lm(df)
  }
}

majority <- function(x){
  max(as.numeric(names(which.max(table(x)))))
}

if(is_log){
  model <- apply(model_coeff, 2, majority) #need majority vote since logistic regression -- To be corrected
} else {
  model <- apply(model_coeff, 2, mean) #simple mean
}

names(model)<- c("(Intercept)", sprintf("x%d", 1:(ncol(o_df)-1))) #we can save the coefficient titles

model

#implement naive parameter to report importance in lasso and elastic net process. -- what about other machine learning?

# Questions ---
#   Need my.bag.lm, my.bag.logit, my.bag.ridge, my.bag.lasso, my.bag.elastic.net, my.bag.svr to return coeff
#   
#   
#   
#   



