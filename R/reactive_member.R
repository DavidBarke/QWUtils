#' Reactive member
#'
#' Generate a \code{\link[R6:R6Class]{R6Class}}-member with reactive behaviour.
#'
#' @param value If \code{\link[shiny:reactive]{reactive}} or function, this
#' reactive or function is returned, else a
#' \code{\link[shiny:reactiveVal]{reactiveVal}} with value as its initial value
#' is returned.
#'
#' @export
reactive_member <- function(value) {
  if (purrr::is_function(value)) {
    return(value)
  } else {
    return(shiny::reactiveVal(value))
  }
}
