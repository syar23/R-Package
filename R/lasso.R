library(glmnet)

# Data Preparation Function
# prepares the data for Lasso regression by scaling and centering the predictors and identifying the response type.
prepare_data <- function(X, y) {
  # check if response variable y is binary or continuous
  if (!all(y %in% c(0, 1)) && length(unique(y)) > 2) {
    response_type <- "continuous"
  } else {
    response_type <- "binary"
  }

  X_scaled <- scale(X)  # scale and center predictors X

  # return a list containing the scaled predictors, response, and response type
  list(X = X_scaled, y = y, response_type = response_type)
}

# Lasso Model Fitting Function
# fit a Lasso regression model using glmnet, automatically adjusting for binary or continuous outcomes.
fit_lasso_model <- function(X, y, response_type) {
  # Fit Lasso model using glmnet, switch family based on response type
  if (response_type == "binary") {
    model <- glmnet(X, y, family = "binomial")
  } else {
    model <- glmnet(X, y, family = "gaussian")
  }

  # Return the fitted model
  return(model)
}

# Model Validation and Selection
# Performs cross-validation to select the optimal lambda and evaluate model performance.
validate_lasso_model <- function(X, y, response_type) {
  # Perform cross-validation using glmnet
  if (response_type == "binary") {
    cv_model <- cv.glmnet(X, y, family = "binomial")
  } else {
    cv_model <- cv.glmnet(X, y, family = "gaussian")
  }

  # Plot the cross-validation curve to visualize performance
  plot(cv_model)

  # Return the cross-validated model, lambda minimizing the cross-validation error,
  # and lambda that is within one standard error of the minimum.
  return(list(model = cv_model, lambda_min = cv_model$lambda.min, lambda_1se = cv_model$lambda.1se))
}

# Prediction Function
# Uses a fitted model to make predictions on new data.
make_predictions <- function(model, new_data, lambda = "lambda.min") {
  # Predict using the specified lambda (lambda.min or lambda.1se)
  predictions <- predict(model, new = new_data, s = lambda, type = "response")

  # Return the predictions
  return(predictions)
}

# Example Usage
# -------------------------------
# Assuming X and y are already defined in your workspace
# data_list <- prepare_data(X, y)
# fitted_model <- fit_lasso_model(data_list$X, data_list$y, data_list$response_type)
# validated_model <- validate_lasso_model(data_list$X, data_list$y, data_list$response_type)
# predictions <- make_predictions(validated_model$model, new_data = data_list$X)
