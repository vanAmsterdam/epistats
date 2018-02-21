

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
