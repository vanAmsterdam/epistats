## Check for updates in source files and curation files
#'
#' When updating datasets and curation files, check if there is anything
#' new in the curation or source files. If so, update, otherwise,
#' leave as is
#'
#' @author Wouter van Amsterdam, authors of stats package
#' @export
#' @param source The file with the curation. The last referenced file should be the ouput of curated data
#' @param uses_here logical, does the curation file use the function \code{\link{here}}
#' from the package with the same name to refer to files? Currently the only option
#' @return Nothing, the curated file is updated when something had changed
#' @import stringr
#' @seealso \code{\link{here}}

curate_update <- function(source, uses_here = TRUE) {
  if (!uses_here) {stop("case of uses_here = FALSE is not implemented")}
  source_lines <- readLines(source)
  file_lines <- str_subset(source_lines, "here\\(")
  # remove commented file lines
  file_lines <- file_lines[!str_detect(file_lines, "^#")]
  if (length(file_lines) == 1) {warning("no referenced files found, does the source use 'here('")}

  current_md4 <- ifelse(
    file.exists(paste0(source, ".md4")),
    readLines(paste0(source, ".md4"))[[1]],
    ""
  )

  source_sha <- openssl::sha224(source_lines)

  file_paths <- file_lines %>%
    map(~str_extract(.x, "(?<=here\\().*")) %>%
    map(between_brackets) %>%
    map(~str_remove_all(.x, "\"")) %>%
    map(~str_split(.x, ",")[[1]]) %>%
    map(str_trim) %>%
    map(~do.call(here, as.vector(.x, mode = "list")))

  cat("hashing files...\n")
  file_shas <- map(file_paths, function(file_path) {
    cat(file_path); cat("\n")
    con <- file(system.file(file_path))
    hash <- openssl::sha224(con)
    close(con)
    return(hash)
  })

  all_shas <- c(source_sha, file_shas)

  sum_md4 <- openssl::md4(paste0(all_shas, collapse = ""))

  if (sum_md4 == current_md4) {
    print("curation file and source files unchanged")
  } else {
    print("changes detected, re-running curation")
    if (str_detect(source, ".Rmd")) {
      print("knitting Rmd file")
      knitr::knit(source)
    } else {
      print("running source file")
      source(source)
    }
    con = file(paste0(source, ".md4"))
    writeLines(sum_md4, con)
    close(con)
  }
}

#' Extract text between brackets from a character
#'
#' Extract text between brackets from a character
#'
#' @export
#' @author Wouter van Amsterdam
#' @param x A character vector. Should start after opening parenthesis (e.g.
#' \code{file = foo.txt)})
#' @import stringr
#'
between_brackets <- function(x) {
  x_len <- str_length(x)
  open_count = 1
  close_count = 0
  i = 0
  while (open_count > close_count & i <= x_len) {
    i = i + 1
    open_count  <- open_count  + str_detect(str_sub(x, i, i), "\\(")
    close_count <- close_count + str_detect(str_sub(x, i, i), "\\)")
  }
  if (open_count != close_count) {
    print(x)
    stop("Line ended before closing bracket. Multiline calls for file paths not implemented")
  }
  return(str_sub(x, end = i-1))
}



#' Fill missing values in vectors by last non-empty entry
#'
#' Fill missing values in vectors by last non-empty entry
#'
#' @author Wouter van Amsterdam
#' @param x a vector (with possible missing values)
#' @param first_to_last a logical, indicating whether to go from first to
#' last or last to first
#' @return a vector with no missing values
#' @export
#'

fill_recursive <- function(x, first_to_last = TRUE) {
  ### to-do: write a vectorize version of this function

  if (!first_to_last) x = rev(x)

  x_prev = x[1]
  if (is.na(x_prev)) warning("first value of x is missing, don't know how to handle those")

  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x_prev
    } else {
      x_prev = x[i]
    }
  }

  if (!first_to_last) x = rev(x)

  x
}


#' Recode 2 factor variables into 1
#'
#' Recode 2 factor variables into 1
#'
#' @author Wouter van Amsterdam
#' @param data a named list or data frame with factors of length 2
#' @param informative_labels return factor levels based on the original labels
#' or just group numbers?
#' @param missing_as_level treat missings as a category or return \code{NA}
#' @return a factor with \code{nlevels(x1)*nlevels(x2)} levels.
#' @export
#'

recode_2_factors <- function(data, informative_labels = TRUE,
                             missing_as_level = FALSE) {
  if (!is.list(data) | is.null(names(data))) stop("please provide a dataframe or named list")
  if (length(data) > 2) warning("using only first two columns")

  if (!is.data.frame(data)) data = as.data.frame(data)

  x_names <- names(data)

  x <- data[[1]]
  y <- data[[2]]

  out <- factor(glue::glue('{x_names[1]}{x}{x_names[2]}{y}'))

  if (!missing_as_level) out[!complete.cases(data)] <- NA

  if (informative_labels) return(out)
  as.numeric(out)
}

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
logical2factor <- function(x, labels = c("no", "yes"), na_label = "missing") {
  x <- factor(x, levels = c(F, T), labels = labels)
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
#' @param label a prefix for groups. Set to "none" to return the ranges
#' @return an ordered factor
#' @seealso \code{\link{cut}} \code{\link{quantile}}
#' @export
quant <- function(x, n.tiles = 4, probs = NULL, label = "q") {
  if (is.null(probs)) probs = seq(0, 1, length.out = n.tiles + 1)
  breaks = quantile(x, probs = probs, na.rm = T)
  out = cut(x, breaks = breaks, include.lowest = T, ordered_result = T)
  if (label == "none") return(out)
  return(factor(out, levels = levels(out), labels = paste0(label, 1:n.tiles)))
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


