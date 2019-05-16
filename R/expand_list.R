#' Easy way to add an empty list to a list
#'
#' Add an empty list to a list at a position specified by a character vector,
#' which is interpreted recursively. See \code{Examples}.
#'
#' \code{\link[base:Extract]{`[[`}} allows indexing a list with a vector, but only
#' if the indices are already present in the list. \code{expand_list} implements
#' an easy way to equip a list with the needed indices.
#'
#' @param .list A list.
#' @param position A character vector.
#'
#' @examples
#' l <- list()
#' # Add empty list "a" on top level
#' l <- expand_list(l, "a")
#' # Add list "b" on top level with empty list "c" inside it
#' l <- expand_list(l, c("b", "c"))
#' # Add empty list "d" to the previously defined list "b"
#' l <- expand_list(l, c("b", "d"))
#'
#' @export
expand_list <- function(.list, position) {
  for (i in seq_along(position)) {
    if (i == 1) {
      current_names <- names(.list)
    } else {
      current_names <- names(.list[[position[seq_len(i - 1)]]])
    }
    if (! (position[i] %in% current_names) ) {
      if (i == 1) {
        .list[[position[1]]] <- list()
      } else {
        .list[[position[seq_len(i - 1)]]][[position[i]]] <- list()
      }
    }
  }
  .list
}
