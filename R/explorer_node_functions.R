#' Get ancestor node of a node
#'
#' Get the ancestor node of a node which is \code{generation} generations away
#' from \code{node}.
#'
#' @param node A \code{\link{ExplorerNode}}.
#' @param generation An \code{\link[base:integer]{integer}} indicating the number
#' of generations to go back in the node tree. Negative integers and 0 will result
#' in returning \code{node}.
#'
#' If generation is greater than the number of the node's ancestors, NULL is
#' returned.
#'
#' @export
get_ancestor_node <- function(node, generation) {
  # If generation equals NULL, while loop will be skipped
  while(generation > 0) {
    generation <- generation - 1
    node <- QWUtils::fallback(node$get_parent_node(), NULL)
  }

  node
}

#' Get distance of a node to an ancestor node
#'
#' Get the distance of a node to another node in the tree
#'
#' @param node,ancestor_node An object of class \code{\link{ExplorerNode}}.
#'
#' If \code{node} is the ancestor node, \code{0} is returned. If \code{ancestor_node}
#' is no ancestor of \code{node}, \code{\link[base:NULL]{NULL}} is returned.
#'
#' @export
get_node_distance <- function(node, ancestor_node) {
  distance <- 0

  while(!(node$get_id() == ancestor_node$get_id())) {
    distance <- distance + 1
    node <- node$get_parent_node()
    if (purrr::is_null(node)) {
      return(NULL)
    }
  }

  distance
}
