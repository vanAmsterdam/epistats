#' Calculate dispersion of a model fit
#'
#' Calculate dispersion of glm model fit
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a glm fit
#' @return a lenght 1 numeric with the dispersion factor

dispersion <- function(fit) {
  p_resids = residuals(fit, type = "pearson")
  df_resid = fit$df.resid
  return(sum(p_resids^2)/df_resid)
}


#' Get proportion and confidence interval from a logical variable
#'
#' Get proportion and confidence interval from a logical variable in one line
#'
#' @author Wouter van Amsterdam
#' @export
#' @param x a logical variable (or 0-1 numeric / integer)
#' @param prop_true logical flag, get proportion of 'trues' or 'falses'
#' @param method passed to \code{\link{binom::binom.confint}} for method
#' of interval calculation. Default is 'wilson'
#' @param conf.level confidence level for interval
#' @param ... other options for \code{\link{binom::binom.confint}}
#' @return a \code{data.frame} with 1 row containing
#' number of trues, length of vector, proportion (mean), lower and upper bound for intervals.

binom.confint_logical <- function(x, prop_true = T,
                                  method = "wilson", conf.level = 0.95, ...) {
  if (!(is.logical(x) | setequal(unique(x), c(0, 1)))) {
    stop("provide logical vector or vector with 0-1 values")
  }

  if (!is.logical(x)) x = as.logical(x)

  binom::binom.confint(
    x = ifelse(prop_true, sum(x), sum(!x)),
    n = length(x),
    conf.level = conf.level,
    method = method,
    ...
  )
}


#' Sorted data.frame of correlations
#'
#' Get a sorted `data.frame` of correlation coefficients, insted of a correlation matrix
#'
#' @param x a matrix (-like structure) that goes into \code{\link{cor}}
#' @param absolute a logical, use absolute values of correlation for ordering?
#' @param decreasing a logical, order with highest correlation first or last
#' @param ... other arguments for \code{\link{cor}}, like `method = "spearman"`
#'
#' @author Wouter van Amsterdam
#' @export
#'

cor.sorted <- function(x, absolute = TRUE, decreasing = T, ...) {
  # get correlation matrix
  cor_matrix <- cor(x, ...)

  # reduce redundancy by removing diagonal and lower triangle
  # (which is the same as the upper diagonal)
  cor_matrix[lower.tri(cor_matrix, diag = T)] <- NA

  # melt to data.frame with columns Var1 and Var2, and correlation,
  # so the pairwise correlations
  suppressWarnings(
    cor_melted <- data.table::melt(cor_matrix, value.name = "correlation")
  )

  # remove redundant rows
  cor_melted <- cor_melted[!is.na(cor_melted$correlation),]

  # add absolute correlation
  cor_melted$abs_correlation <- abs(cor_melted$correlation)

  # order results
  if (absolute) {
    cor_order <- order(cor_melted$abs_correlation, decreasing = decreasing)
  } else {
    cor_order <- order(cor_melted$correlation, decreasing = decreasing)
  }

  cor_sorted <- cor_melted[cor_order,]
  rownames(cor_sorted) <- 1:nrow(cor_sorted)

  return(cor_sorted)
}

##### Development area
# lm.steps <- function(fit, scope, direction = c("both", "backward","forward")) {
#
# }
#
#
# stand.coef <- function(fit) {
#
# }
