#' Get the name of all functions in a loaded package
#'
#' @param package Package name.
#'
#' @export
package_functions <- function(package) {
  pos <- paste0("package:", package)
  objects <- ls(pos = pos)
  is_fun <- purrr::map_lgl(objects, function(object) {
    object <- get(object, pos = pos)
    typeof(object) %in% c("closure", "builtin", "special")
  })
  objects[is_fun]
}
