
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
