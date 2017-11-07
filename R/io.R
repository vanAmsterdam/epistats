#' Create path to file based on the parent directory
#'
#' This function circumvents \code{setwd("..")} and setting it back again.
#' Usefull for working with slidify for instance
#'
#' @author Wouter van Amsterdam
#' @param file a character string giving the path based on the parent directory
#' @param levelsUp an integer specifying how many 'generations' you want to go up.
#' Default = 1
#' @export
#' @references Based on a post by Keith Hughitt on stackoverflow
fromParentDir <- function(file = "", levelsUp = 1) {
  parts = unlist(strsplit(getwd(), .Platform$file.sep))
  parent = do.call(file.path, as.list(parts[1:(length(parts) - levelsUp)]))
  path = file.path(parent, file)
}


#' Read in all datasets within a directory
#'
#' This function reads in all data stored in flat text files in a specified folder (optional: with subfolders)
#' It assigns them to reasonable names in the specified environment (default = global)
#' Furthermore it creates a vector containing the names of the datasets
#'
#' @author Wouter van Amsterdam
#' @param dir directory with data files
#' @param recursive logical, include subfolders or not
#' @param extensions character vector specifying the possible extensions your datafile have
#' @param envir environment to assing the datasets to
#' @param ... arguments to pass on to fread, like \code{headers = T, stringsAsfactors = T, colclasses = c(...)}
#' @export
#' @importFrom utils tail
#' @importFrom data.table fread
#' @seealso \code{\link[data.table]{data.table}}, \code{\link[data.table]{fread}}
readinDir <- function(dir, recursive = T, extensions = c(".txt",".csv"), envir = .GlobalEnv, ...) {
  fileNames <- list.files(dir, recursive = recursive)
  fileNames <- grep(paste0(extensions, collapse = "|"), fileNames, value = T)
  if (anyDuplicated(fileNames)) stop("file names in the directory should be unique")
  fileNames <- make.names(fileNames)
  datasetNames <- character()
  lapply(fileNames, function(fileName) {
    datasetName <- gsub(".txt", '', fileName)
    if (recursive) {
      datasetName <- tail(strsplit(datasetName, split = "/")[[1]], 1) # splits subdirectories, last item is file name
    }
    datasetNames <<- append(datasetNames, datasetName)
    # eval(parse(text =
    # paste0(datasetName, " <<- fread(\"",dir,fileName,"\")")
    # ))
    data <- fread(paste0(dir, fileName), ...)
    assign(datasetName, data, envir = envir)
  })
  setListName <- paste0(tail(strsplit(dir, split = '/')[[1]], 1), "_datasets")
  assign(setListName, datasetNames, envir = envir)
  return()
}

#' Save objects and rename them
#'
#' Use this function if you want export R objects from one project to another,
#' but want to the objects to carry a different (prespecified) name in the other project
#'
#' @param ... any arguments passed to \code{save}, but arguments may be named
#' @param file a filename
#' @examples
#' x <- 1:10
#' \dontrun{saveAs(y = x, file = "myFile.Rdata")}
#' @export
saveAs <- function(..., file) {
  x <- list(...)
  save(list=names(x), file=file, envir=list2env(x))
}
