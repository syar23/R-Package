pScore <- function(x, y) {
  if ((is.factor(y) | is.character(y)) && (is.factor(x) | is.character(x))) {
    # If both x and y are categorical
    print("x and y are categorical")
    contingency_table <- table(x, y)
    out <- chisq.test(contingency_table)$p.value
  } else if ((is.factor(y) | is.character(y)) && is.numeric(x)) {
    # If x is numeric and y is categorical
    print("x is numeric and y is categorical")
    
    numx <- length(unique(x))
    
    if (numx > 2) {
      # With more than two levels in x, compute ANOVA
      print("ANOVA")
      anova_result <- aov(x ~ y)
      summary_anova <- summary(anova_result)
      out <- summary_anova[[1]]$`Pr(>F)`[1]
      
    } else {
      # For binary predictors, test the odds ratio == 1 via Fisher's Exact Test
      print("fisher")
      out <- fisher.test(factor(x), y)$p.value
    }
  } else if (is.numeric(y) && (is.factor(x) | is.character(x))) {
    # If x is categorical and y is numeric
    print("x is categorical and y is numeric")
    
    if (length(unique(y)) > 2) {
      # Perform a correlation test
      #out <- cor.test(x, y)$p.value
      kruskal_test_result <- kruskal.test(y ~ x)
      out <- kruskal_test_result$p.value
    } else {
      # For binary response, perform linear regression
      out <- summary(lm(x ~ y))$coefficients[2, 4]  # Extract p-value from regression summary
    }
  } else if (is.numeric(y) && is.numeric(x)) {
    # If both x and y are numeric
    print("x and y are numeric")
    
    out <- cor.test(x, y)$p.value
  } else {
    stop("Unsupported data types for 'x' and 'y'.")
  }
  
  return(out)
}

#' Variable Screening for top K predictors
#'
#' This function performs univariate filtering on each column of variable
#' matrix X and performs various tests according to the combinations of x 
#' and y variables.
#' In this scenario, we are only considering binary variables under the categorical
#' category.
#' If y is numeric, and x is numeric - Pearson's Test or Spearman's Test
#' If y is numeric, and x is binary - ANOVA or Kendall's
#' If y is binary and x is numeric - ANOVA or Kendall's
#' If y is binary and x is binary - Chi Squared or Mutual Information
#' 
#'
#' @param X matrix of the predictor variables
#' @param y matrix of the output variables
#' @param k Value of number of predictors to be selected
#'        If not specified this value will be nCol(X)/2 
#' @return X matrix of the selected variables
#' @export
variable_screening <- function(X, y, K = NULL) {
  
  #Sample Size, n -=length(y)
  #Number of predictors, p = number of Columns in X
  if (length(y) < 20 * ncol(X)) {
    print("Ratio test failed")
    selected_columns <- list()
    for(i in 1:ncol(X)) {
      pValue <- pScore(X[, i], y)
      print(pValue)
      if(pValue <= 0.05) {
        selected_columns[[i]] <- list(column_index = i, p_value = pValue)
      }
    }
    
    #selected_columns <- sort(selected_columns, decreasing = FALSE, by = function(i) i$p_value)
    #selected_columns <- selected_columns[sapply(selected_columns, function(x) !is.null(x))]
    selected_columns <- Filter(Negate(is.null), selected_columns)
    
    selected_columns <- selected_columns[order(sapply(selected_columns, function(i) i$p_value))]    
    
    
    if (!is.null(K)) {
      selected_columns <- selected_columns[1:K]
    }
    #return(unlist(lapply(selected_columns, function(i) i$column_index)))
    selected_indices <- unlist(lapply(selected_columns, function(i) i$column_index))
    
    print(selected_indices)
    
    selected_dataframe <- X[, selected_indices, drop = FALSE]  # Extract selected columns as dataframe
    return(selected_dataframe)
  }
  
  return(X)
}
