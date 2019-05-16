#' Get paths of R scripts
#'
#' Get the paths of all R scripts inside a folder.
#'
#' @param path A path to the directory containing the R scripts.
#' @param recursive If \code{\link[base:logical]{TRUE}}, subdirectories are
#' recursively searched for R scripts.
#'
#' @export
script_paths <- function(path, recursive = TRUE) {
  dir(
    path,
    recursive = recursive,
    full.names = TRUE,
    pattern = ".*[.](r|R|s|S|q)([.](lnk|LNK))*$"
  )
}

#' Get the names of all objects assigned in a R script
#'
#' Get the names of all objects assigned in a R script.
#'
#' Note that this function can't detect the presence of the following
#'
#' @param path Path to the R script.
#' @param chdir If \code{\link[base:logical]{TRUE}}, the \code{R} working
#' directory is temporarily changed to the directory containing the R script.
#' @param all.names Logical. If \code{\link[base:logical]{TRUE}} all object
#' names are returned. If \code{\link[base:logical]{FALSE}}, names which begin
#' with a . are omitted.
source_names <- function(
  path, chdir = FALSE, all.names = FALSE
) {
  .envir <- new.env()
  R.utils::sourceTo(file = path, chdir = chdir, envir = .envir)
  objects <- ls(e, all.names = all.names)
  types <- purrr::map_chr(objects, function(object){
    typeof(.envir[[object]])
  })
  names(types) <- objects
  types
}

expand_list_by_vector <- function(.list, .vector) {
  for (i in seq_along(.vector)) {
    if (i == 1) {
      current_names <- names(.list)
    } else {
      current_names <- names(.list[[.vector[seq_len(i - 1)]]])
    }
    if (! (.vector[i] %in% current_names) ) {
      if (i == 1) {
        .list[[.vector[1]]] <- list()
      } else {
        .list[[.vector[seq_len(i - 1)]]][[.vector[i]]] <- list()
      }
    }
  }
  .list
}

variable_list <- function(
  path, chdir = FALSE, verbose = FALSE, exclude_internal = FALSE
) {
  script_paths <- script_paths(path)

  variable_list <- list()

  for (path in script_paths) {
    source_names <- source_names(path, chdir, verbose, exclude_internal)
    path_vec <- str_split(path, "/")[[1]]
    variable_list <- expand_list_by_vector(variable_list, path_vec)
    variable_list[[path_vec]] <- source_names
  }

  variable_list
}
