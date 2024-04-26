
variable_pre_screening <- function(X, y, threshold = 0.05) {

  if (length(y) < 20 * ncol(X)) {
    # Initialize vector to store selected variables
    selected_columns <- integer(0)
    for(x in 1:ncol(X)) {
      pValue <-pScore(x, y)
      if (p_value <= threshold) {
        selected_columns <- c(selected_columns, i)
      }
    }
    return(selected_columns)

  }
    return(X)
}

pScore <- function(x, y) {
  if ((is.factor(y) | is.character(y)) && (is.factor(x) | is.character(X))) {
    # If both x and y are categorical
    contingency_table <- table(x, y)
    out <- chisq.test(contingency_table)$p.value
  } else if ((is.factor(y) | is.character(y)) && is.numeric(x)) {
    # If x is numeric and y is categorical
    numx <- length(unique(x))

    if (numx > 2) {
      # With many values in x, compute a t-test
      out <- t.test(x ~ y)$p.value
    } else {
      # For binary predictors, test the odds ratio == 1 via Fisher's Exact Test
      out <- fisher.test(factor(x), y)$p.value
    }
  } else if (is.numeric(y) && (is.factor(x) | is.character(x))) {
    # If x is categorical and y is numeric
    if (length(unique(y)) > 2) {
      # Perform a correlation test
      out <- cor.test(x, y)$p.value
    } else {
      # For binary response, perform linear regression
      out <- summary(lm(x ~ y))$coefficients[2, 4]  # Extract p-value from regression summary
    }
  } else if (is.numeric(y) && is.numeric(x)) {
    # If both x and y are numeric
    out <- cor.test(x, y)$p.value
  } else {
    stop("Unsupported data types for 'x' and 'y'.")
  }

  return(out)
}



