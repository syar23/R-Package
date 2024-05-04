#' Calculate p-value for Univariate Association Test
#'
#' This function calculates the p-value for a univariate association test between two variables x and y.
#' The appropriate statistical test is selected based on the data types of x and y.
#' If both x and y are categorical, chi-squared test is performed.
#' If x is numeric and y is categorical, ANOVA or Fisher's Exact Test is conducted.
#' If x is categorical and y is numeric, Kruskal-Wallis test or Simple Linear Regression is used.
#' If both x and y are numeric, Pearson's correlation test is applied.
#'
#' @param x Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. For binary outcomes, it should be a binary vector (0 or 1). For continuous outcomes, it should be a numeric vector.
#' @return The p-value indicating the significance of association between x and y.
pScore <- function(x, y) {
  if ((is.factor(y) | is.character(y)) && (is.factor(x) | is.character(x))) {

    # If both x and y are categorical
    contingency_table <- table(x, y)
    out <- chisq.test(contingency_table)$p.value

  } else if ((is.factor(y) | is.character(y)) && is.numeric(x)) {

    # If x is numeric and y is categorical
    numx <- length(unique(x))
    if (numx > 2) {
      # With more than two levels in x, compute ANOVA
      anova_result <- aov(x ~ y)
      summary_anova <- summary(anova_result)
      out <- summary_anova[[1]]$`Pr(>F)`[1]
    } else {
      # For binary predictors, test the odds ratio == 1 via Fisher's Exact Test
      out <- fisher.test(factor(x), y)$p.value
    }
  } else if (is.numeric(y) && (is.factor(x) | is.character(x))) {
    # If x is categorical and y is numeric

    if (length(unique(y)) > 2) {
      # Perform a correlation test
      # out <- cor.test(x, y)$p.value
      kruskal_test_result <- kruskal.test(y ~ x)
      out <- kruskal_test_result$p.value
    } else {
      # For binary response, perform linear regression
      out <- summary(lm(x ~ y))$coefficients[2, 4] # Extract p-value from regression summary
    }
  } else if (is.numeric(y) && is.numeric(x)) {
    # If both x and y are numeric
    out <- cor.test(x, y)$p.value
  } else {
    print("Unsupported data types for 'x' and 'y'.")
    out<-0
  }

  return(out)
}

#' Variable Screening for Top Predictors
#'
#' This function performs Univariate filtering (Univariate filtering evaluates each feature's individual association with
#' the target variable through statistical tests, selecting the most significant features based on measures such as correlation,
#' ANOVA, or chi-squared tests.) on each column of the predictor matrix X based on the provided output variable y.
#' Various statistical tests are applied depending on the data types of X and y to identify significant predictors.
#' If y is numeric and X is numeric, Pearson's correlation test or Spearman's rank correlation test is applied.
#' If y is numeric and X is binary, ANOVA or Kendall's rank correlation test is performed.
#' If y is binary and X is numeric, ANOVA or Fisher's Exact Test is used.
#' If both y and X are binary, Chi-squared test is applied.
#'
#' @param X Matrix of predictors. Each row represents an observation, and each column represents a predictor.
#'        can be a mix of continuous, discrete, and binary predictors.
#' @param y Response variable. Can be continuous and normally distributed or binary responses.
#' @param K Number of top predictors to select. If not specified, default is ncol(X)/2.
#' @return A matrix containing the selected predictor variables based on statistical significance.
#' @export
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100 * 25), ncol = 25)
#' y <- rbinom(100, 1, 0.5)
#' selected_vars <- variable_pre_screening(X, y, 10)
#'
#' @seealso
#' `pScore` for calculating the statistical significance of predictors.
#'
variable_pre_screening <- function(X, y, K = NULL) {
  # Sample Size, n -=length(y)
  # Number of predictors, p = number of Columns in X
  if (length(y) < 20 * ncol(X) || !is.null(K)) {
    print("Some top predictors are being selected - ")
    selected_columns <- list()
    for (i in 1:ncol(X)) {
      pValue <- pScore(X[, i], y)
      print(pValue)
      if (pValue <= 0.05) {
        selected_columns[[i]] <- list(column_index = i, p_value = pValue)
      }
    }

    selected_columns <- Filter(Negate(is.null), selected_columns)
    selected_columns <- selected_columns[order(sapply(selected_columns, function(i) i$p_value))]

    if (!is.null(K)) {
      selected_columns <- selected_columns[1:K]
    }

    selected_indices <- unlist(lapply(selected_columns, function(i) i$column_index))
    print(selected_indices)
    # Extract selected columns as dataframe
    selected_dataframe <- X[, selected_indices, drop = FALSE]
    return(selected_dataframe)
  }

  #If not above, entire X matrix is returned as is
  print("Returning Original X ")
  return(X)
}
