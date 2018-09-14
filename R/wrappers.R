#' Get concordance index from Cox model fit
#'
#' Get concordance index from Cox model fit
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a \code{coxph} fit
#' @return result of \code{\link{Hmisc::rcorr.cens}}

survfit_ci <- function(fit, outx = FALSE) {
  Hmisc::rcorr.cens(x = -fit$linear.predictors, S = fit$y, outx = outx)
}

#' Get ROC curve from a logistic glm fit in 1 line
#'
#' Get ROC curve from a logistic glm fit in 1 line
#'
#' @author Wouter van Amsterdam
#' @export
#' @param fit the result of a glm fit with \code{family = binomial("logit")}
#' @return an ROC plot


#' Extract coefficients and confidence intervals from a fit
#'
#' Extract coefficients and confidence intervals from a fit, based on the
#' Wald approximation.
#'
#' @author ? and Wouter van Amsterdam
#' @references function modified from excercises in the Advanced
#' methods in causal research course from the Julius Center Utrecht, the Netherlands
#' @param fit the result of \code{glm} or \code{lm}
#' @param alpha required confidence level (coverage) for interval
#' @param return_terms return terms as in a separate column?
#' @return a matrix with first column the estimate, 2nd column CI-low, 3d column CI-high

extract_RR <- function(fit, alpha = 0.05, return_terms = F){

  coefs = exp(fit$coef);
  se    = diag(vcov(fit))
  ci_lo = coefs - qnorm(1 - alpha / 2) * sqrt(se)
  ci_hi = coefs + qnorm(1 - alpha / 2) * sqrt(se)
  if (!return_terms) {
    return(data.frame(estimate = coefs,
                      ci_low = ci_lo,
                      ci_high = ci_hi))
    }
  data.frame(term = names(coefs),
             estimate = coefs,
             ci_low = ci_lo,
             ci_high = ci_hi)
  }


#' Create a table with margins
#'
#' Wrapper for \code{addmargins(xtabs(...))}
#'
#' @author Wouter van Amsterdam
#' @param ... arguments to \code{xtabs}
#' @return a table with margins
#' @seealso \code{\link{xtabs}}, \code{\link{addmargins}}
#' @export
#'
mtable <- function(..., margin = 1) {
  tab <- table(...)
  tab <- addmargins(tab, margin = margin)
  return(tab)
}



#' split a vector at specified indices
#'
#' split a vector at specified indices
#'
#' @author Wouter van Amsterdam
#' @param x a vector to split
#' @param pos an integer vector containing the indices on which \code{x} will be
#' split. These are the starting indices of the resulting chunks
#' @param removeSplitElements should the indices at which to perform the split
#' be removed from the resulting chunks? Default = FALSE
#' @return a list with the chunks of x
#' @seealso \code{split}
#' @examples
#' x <- 1:10
#' splitAt(x, c(2, 7))
#' splitAt(x, c(2, 7), removeSplitElements = T)
#' @export
splitAt <- function(x, pos, removeSplitElements = FALSE) {
  if (removeSplitElements) {
    x <- x[-pos]
    pos <- pos - (seq_along(pos)-1)
  }
  spl <- split(x, cumsum(seq_along(x) %in% pos))
  return(unname(spl))
}

#' Create a table with usaNA = 'always' as default option
#'
#' @param ... any arguments passed to the base table function
#' @seealso \code{\link{table}}
#' @return a table
#' @export
tabl <- function(...) table(..., useNA = 'always')


#' Get the sum of NA's in one call
#'
#' Tired of typing \code{sum(is.na(...))} or \code{colSums(is.na(...))}?
#'
#' @author Wouter van Amsterdam
#' @usage nna(x)
#' @param x whatever you put into \code{is.na} (vector, data.frame, ...)
#' @param prop logical, get counts or proportion of missing values
#' @return Either an integer if you provide a vector, or a named integer vector of length \code{ncol(df)} if you provide a data.frame
#' @seealso \code{\link{is.na}}
#' @examples
#' x <- c(1:5, NA)
#' nna(x)
#' df <- data.frame('foo' = c(1:4, NA), 'bar' = c(1:3, NA, NA))
#' nna(df)
#' @export
nna <- function(x, prop = F) {
  if (is.data.frame(x)) {
    if (prop) return(colMeans(is.na(x)))
    return(colSums(is.na(x)))
  }
  if (prop) return(mean(is.na(x)))
  sum(is.na(x))
}
