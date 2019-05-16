#' Get name and type of objects from all the scripts of a directory
#'
#' Get name and type of all objects that are assigned on the top level of the
#' R scripts in a directory.
#'
#' @inheritParams script_paths
#' @inheritParams object_names
#'
#' @return
#'
#' A list of the objects. The path from the root of the list to an object
#' corresponds to the path of the R script.
#'
#' @export
object_list <- function(
  directory, recursive, chdir = FALSE, all.names = FALSE
) {
  script_paths <- script_paths(directory, recursive = recursive)

  object_list <- list()

  for (path in script_paths) {
    object_names <- object_names(path, chdir, all.names = all.names)
    path_vec <- stringr::str_split(path, "/")[[1]]
    object_list <- expand_list(object_list, path_vec)
    object_list[[path_vec]] <- object_names
  }

  object_list
}
