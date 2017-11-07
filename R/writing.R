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
  apply(x, 1, function(y) print.ci(y[1], y[2], y[3],
                                   add_percentage = add_percentage,
                                   digits = digits))
}

#' Lexical printing of estimates with confidence intervals
#'
#' Print an estimate with confidence interval
#'
#' @param estimate a numeric estimate
#' @param ci.low lower bound of estimate
#' @param ci.high upper bound of estimate
#' @param add_percentage multiply with 100 and add % after values?
#' @param digits an integer vector of length 1, how many digits to round to?
#' @export
#' @author Wouter van Amsterdam
print.ci <- function(estimate, ci.low, ci.high, add_percentage = F, digits = 1) {
  output <- ifelse(add_percentage,
                   paste0(myround(100*estimate, digits),
                          "% (", myround(100*ci.low, digits), "% - ", myround(100*ci.high, digits), "%)"),
                   paste0(myround(estimate, digits),
                          " (", myround(ci.low, digits), " - ", myround(ci.high, digits), ")"))
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
