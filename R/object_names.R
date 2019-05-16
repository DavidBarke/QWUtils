#' Get the names of all objects assigned in an R script
#'
#' Get the names of all objects assigned in an R script.
#'
#' @param path Path to the R script.
#' @param chdir If \code{\link[base:logical]{TRUE}}, the \code{R} working
#' directory is temporarily changed to the directory containing the R script.
#' @param all.names Logical. If \code{\link[base:logical]{TRUE}} all object
#' names are returned. If \code{\link[base:logical]{FALSE}}, names which begin
#' with a . are omitted.
#'
#' @export
object_names <- function(
  path, chdir = FALSE, all.names = FALSE
) {
  .envir <- new.env()
  R.utils::sourceTo(file = path, chdir = chdir, envir = .envir, encoding = "UTF-8")
  objects <- ls(.envir, all.names = all.names)
  types <- purrr::map_chr(objects, function(object){
    typeof(.envir[[object]])
  })
  names(types) <- objects
  types
}
