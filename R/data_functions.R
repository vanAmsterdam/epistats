

#' Convert SPSS dates to Dates
#'
#' Convert SPSS dates to Dates
#'
#' @author The Internet
#' @param x a date vector from an spss file
#' @return a vector of class \code{Date}
#' @export
#' @seealso \code{\link{foreign}}
date.spss <- function(x) {as.Date(x/(60*60*24), origin = '1582-10-14')}

#' Trim extreme values
#'
#' Trim of extreme values for a given percentile
#'
#' @author Petr Klasterecky
#' @author Wouter van Amsterdam
#' @references \link{https://stat.ethz.ch/pipermail/r-help/2007-March/127619.html}
#' @param x a numeric vector, or data frame or matrix
#' @param prop proportion of data to trim off (two-sided)
#' @export
trim <- function(x, prop = 0.05) {
  if (is.null(dim(x))) {
    return(trim_vec(x, prop = prop))
  } else if (length(dim(x) == 2)) {
    return(apply(x, 2, trim_vec, prop = prop))
  }
}


#' is.true? (NA's are false)
#'
#' Treat NA's in logical vectors as \code{FALSE}
#'
#' @author JWilliman
#' @references http://stackoverflow.com/a/26877156
#' @param x a logical vector
#' @return a logical vector without missing values
#' @examples
#' a = c(T, F, F, NA)
#' is.true(a)
#' @export
is.true <- function(x) {!is.na(x) & x}


#' Convert logical to factor, with default labels
#'
#' Converts a logical to factor, with default labels. Set na_label to NULL
#' to not change NA's to 'missing'
#'
#' @author Wouter van Amsterdam
#' @param x a logical vector
#' @param labels optional factor labels (default to 'yes' and 'no')
#' @param na_label label for NA's, set to NULL to keep \code{NA}
#' @return a factor vector
#' @export
#' @seealso \code{\link{as.factor}}, \code{\link{na2missing}}
logical2factor <- function(x, labels = c("yes", "no"), na_label = "missing") {
  x <- factor(x, levels = c(T, F), labels = labels)
  if (!is.null(na_label)) x <- na2missing(x, label = na_label)
  return(x)
}

#' Replace 'NA' in factor variable with a string
#'
#' Replace \code{NA}'s in a factor variable with a specified string for pretty
#' printing (in one line of code)
#'
#' @author Wouter van Amsterdam
#' @param x a factor variable (or character, or logical)
#' @param label optional, a character vector of lenght 1, specifying the name to give to the
#' \code{NA}
#' @param ifany logical, should 'missing' be added as a factor level only if there are missing values?
#' @seealso \code{\link{addNA}, \link{levels}}
#' @export
na2missing <- function(x, label = "missing", ifany = T) {
  stopifnot(class(x) %in% c("factor", "character", "ordered", "logical"))
  if (is.logical(x) | is.character(x)) x <- as.factor(x)
  y <- addNA(x, ifany = ifany)
  if (nlevels(y)>nlevels(x)) levels(y) <- c(levels(x), label)
  return(y)
}


#' Recode a variable in quantiles
#'
#' Wrapper for \code{cut} and \code{quantile}
#'
#' @author Wouter van Amsterdam
#' @param x a vector to recode to quantiles
#' @param n.tiles the number of quantiles (so 4 = quartiles, 5 = quintiles, etc)
#' @param probs optional: provide explicit probabilities
#' @seealso \code{\link{cut}} \code{\link{quantile}}
#' @export
quant <- function(x, n.tiles = 4, probs = "") {
  if (probs == "") probs = seq(0, 1, length.out = n.tiles + 1)
  breaks = quantile(x, probs = probs, na.rm = T)
  cut(x, breaks = breaks, include.lowest = T, ordered_result = T)
}


#' Rename column names and factor levels to 'code friendly' names (wrapper for make.names)
#'
#' This function applies the \code{make.names} function from the \code{base} package to the column names
#' of a dataframe, with the option to apply it to all factor levels as well
#' NB to do: optional data.table argument
#'
#' @author Wouter van Amsterdam
#' @param data a data.frame
#' @param cols a logical, specifying whether to apply \code{make.names} to the column names
#' @param factors a logical, specifying whether to apply \code{make.names} to the labels of factor variables in \code{data}
#' @return a data.table with friendly names
#' @export
#' @import data.table
#' @seealso \code{make.names}
friendlyName <- function(data, cols = T, factorLabels = T) {
  if (is.data.frame(data)) {
    if (!is.data.table(data)) stop("this function only works for data.tables")
    if (cols) colnames(data) <- make.names(colnames(data))
    if (factorLabels) {
      old_colnames <- colnames(data)
      fac_cols <- old_colnames[sapply(data, is.factor)]
      fac_data <- data[, .SD, .SDcols = fac_cols]
      other_data <- data[, -fac_cols, with = F]
      for (i in seq_along(fac_cols)) {
        levels(fac_data[[i]]) <- make.names(levels(fac_data[[i]]))
      }
      data_new <- cbind(other_data, fac_data)
      data_new <- data_new[, .SD, .SDcols = old_colnames] # reorders columns back to original
      data <- data_new
    }
  }
  return(data)
}


