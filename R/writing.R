#' Print coefficients of a fit
#'
#' Grab exponentiaded coefficients of a glm fit and print them,
#' with or without confidence interval
#'
#' @author Wouter van Amsterdam
#' @param fit the model fit, a result of \code{\link{glm}}
#' @param term an integer, supplying the index of the required coefficient
#' @param print_ci include the confidence interval
#' @param alpha the confidence level (1 - coverage) for the confidence interval
#' @param ... further formatting options for \code{\link{print.ci}}

print_coef <- function(fit, term, print_ci = T, alpha = 0.05,
                       ...) {
  coefs <- extract_RR(fit = fit, alpha = alpha)[term,]
  print.ci(coefs, ...)
}

#' Myround
#'
#' Round a number, preserving extra 0's
#'
#' Round a number, preserving extra 0's.
#'
#' @author kbroman
#' @references \link{https://github.com/kbroman/broman/blob/master/R/myround.R}
#' @param x Number to round.
#' @param digits Number of digits past the decimal point to keep.
#' @details
#' Uses \code{\link[base]{sprintf}} to round a number, keeping extra 0's.
#'
#' @export
#' @return
#' A vector of character strings.
#'
#' @examples
#' myround(51.01, 3)
#' myround(0.199, 2)
#'
#' @seealso
#' \code{\link[base]{round}}, \code{\link[base]{sprintf}}
#'
#' @keywords
#' utilities
myround <-
  function(x, digits=1)
  {
    if(digits < 1)
      stop("This is intended for the case digits >= 1.")

    if(length(digits) > 1) {
      digits <- digits[1]
      warning("Using only digits[1]")
    }

    if (is.character(x)) return(x)

    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero

    tmp
  }


#' Lexical printing of estimates with confidence intervals for a matrix of values
#'
#' Print an estimate with confidence interval for a matrix of values
#'
#' @param x a matrix (or object coercible to one) with 3 columns in order: estimate, ci.low, ci.high
#' @param add_percentage multiply with 100 and add % after values?
#' @param digits an integer vector of length 1, how many digits to round to?
#' @export
#' @author Wouter van Amsterdam
print.ci.matrix <- function(x, add_percentage = F, digits = 1) {
  apply(x, 1, function(y) print.ci(estimate = y[1],
                                   ci_low = y[2],
                                   ci_high = y[3],
                                   add_percentage = add_percentage,
                                   digits = digits))
}

#' Lexical printing of estimates with confidence intervals
#'
#' Print an estimate with confidence interval
#'
#' @param x a numeric vector of length 3, with estimate, lower bound for
#' confidence interval, upper bound for confidence interval, in that order.
#' Supply x or \code{estimate, ci_low, ci_high}
#' @param estimate a numeric estimate, supply only when \code{x == NULL}
#' @param ci_low lower bound of estimate, supply only when \code{x == NULL}
#' @param ci_high upper bound of estimate, supply only when \code{x == NULL}
#' @param add_percentage multiply with 100 and add % after values?
#' @param digits an integer vector of length 1, how many digits to round to?
#' @export
#' @author Wouter van Amsterdam
print.ci <- function(x = NULL,
                     estimate = NULL, ci_low = NULL, ci_high = NULL,
                     add_percentage = F, digits = 1) {
  if (!is.null(x) & !is.null(c(estimate, ci_low, ci_high))) {
    stop("Please provide x, or estimate, ci_low and ci_high, not both")
  }
  if (!is.null(x)) {
    estimate = x[1]
    ci_low   = x[2]
    ci_high  = x[3]
  }
  output <- ifelse(add_percentage,
                   paste0(myround(100*estimate, digits),
                          "% (", myround(100*ci_low, digits), "% - ", myround(100*ci_high, digits), "%)"),
                   paste0(myround(estimate, digits),
                          " (", myround(ci_low, digits), " - ", myround(ci_high, digits), ")"))
  return(output)
}

#' Easy printing for t.test
#'
#' Print mean and confidence interval for easy printing / reporting
#'
#' @param x object of class \link{htest}
#' @param lexical print as '<mean> (<lo> - <hi>)'? \code{FALSE} (default)
#' returns a named vector.
#' Only works when \code{out.elements = c('estimate', 'conf.int')}
#' @param out.elements A character vector with the elements of the \code{htest} - object you want printed
#' @param out.names A character vector with the desired names
#' @param digits An integer vector of length 1,
#' set to NA to disable rounding
#' @param add_percentage multiply with 100 and add % after values?
#' @return A character vector of length 1 (when \code{lexical == T}) or of \code{length(out.elements)}
#' @author Wouter van Amsterdam
#' @export
#' @seealso \code{\link{t.test}, \link{htest}}
print.test <-  function(x, lexical = F, add_percentage = T,
                        out.elements = c('estimate', 'conf.int'),
                        out.names = c('mean', 'lower_bound', 'upper_bound'),
                        digits = 1) {
  output <- x[c(out.elements)]
  output <- unlist(output)
  if (add_percentage) output <- 100*output
  if (!is.na(digits)) {output <- myround(output, digits)}

  names(output) <- out.names
  if (lexical)
    output <- ifelse(add_percentage,
                     paste0(output[1],
                            "% (", output[2], "% - ", output[3], "%)"),
                     paste0(output[1],
                            " (", output[2], " - ", output[3], ")")
    )
  return(output)
}

#' Paste text in the normal way: a, b, c and d
#'
#' For printing results
#'
#' @author Wouter van Amsterdam
#' @param x a character vector of any length (or an object coercable to character)
#' @return a character for printing
#' @export
#' @seealso \code{\link{paste}}
lexicalPaste <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  if (length(x) == 1)      x_print <- x
  else if (length(x) == 2) x_print <- paste(x, collapse = " and ")
  else if (length(x) > 2)  x_print <- paste(paste0(head(x, -1), collapse = ", "),
                                            tail(x, 1), sep = " and ")
  else stop("is x a character of length>0?")
  return(x_print)
}

#' Print nice q-values
#'
#' @author Wouter van Amsterdam
#' @param q_value the q value to print
#' @export
#' @seealso \code{\link{prettyP}}
prettyQ <- function(q_value) prettyP(q_value, param = 'q')

#' Print nice p-values
#'
#' Function to print a p-value based on it's value
#'
#' @author Wouter van Amsterdam
#' @param x a vector of p-values
#' @param param Can be set to a different letter (for example to 'q'
#' for FDR corrected p-values)
#' @param lexical a logical, if \code{length(x) > 1} should the p-values be printed in a nice lexical format?
#' @export
prettyP <- function(x, param = 'p', lexical = T) {
  stopifnot(is.numeric(x))
  pretty_ps <- sapply(x, function(p_value) {
    if (p_value >= 0.05 & p_value <= 1) print_p = paste0(param, " = ", round(p_value, 2))
    else if (p_value < 0.001) print_p = paste0(param, " < 0.001")
    else if (p_value < 0.01) print_p = paste0(param, " < 0.01")
    # else if (p_value < 0.05) print_p = paste0(param, " = ", round(p_value, 3))
    else if (p_value < 0.05) print_p = paste0(param, " < 0.05")
    else stop("are you supplying a p-value?")
    return(print_p)
  })
  if (lexical) return(lexicalPaste(pretty_ps))
  return(pretty_ps)
}

#' Replace underscores empty strings (for pretty printing)
#'
#' This is a wrapper for \code{gsub}
#'
#' @author Wouter van Amsterdam
#' @param x a character string (or an object coercible to one)
#' @return a character string
#' @export
#' @seealso \code{\link{gsub}}
noSpace <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  gsub(pattern = "_", replacement = "", x = x)
}
