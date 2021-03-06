#' Get paths of R scripts
#'
#' Get the paths of all R scripts inside a directory.
#'
#' @param directory A path to the directory containing the R scripts.
#' @param recursive If \code{\link[base:logical]{TRUE}}, subdirectories are
#' recursively searched for R scripts.
#' @inheritParams base::list.files
#'
#' @export
script_paths <- function(directory, recursive = TRUE, full.names = TRUE, ending = ".*[.](r|R|s|S|q)([.](lnk|LNK))*$") {
  dir(
    directory,
    recursive = recursive,
    full.names = full.names,
    pattern = ending
  )
}


