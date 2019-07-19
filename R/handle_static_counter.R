#' Handle assignment of a static counter for R6 classes
#'
#' This is a convenience function, checking if a static counter is already
#' initialised. If not, it initialises it with \code{initial_value}, otherwise
#' it adds one to it.
#'
#' @param static \code{\link[base:environment]{Environment}}, which holds the
#' counter.
#' @param name Name of the variable representing the counter.
#' @param initial_value Initial value of the variable representing the counter.
#'
#' @export
handle_static_counter <- function(static, name = "counter", initial_value = 1) {
  if (purrr::is_null(static[[name]])) {
    static[[name]] <- initial_value
  } else {
    static[[name]] <- static[[name]] + 1
  }
}
