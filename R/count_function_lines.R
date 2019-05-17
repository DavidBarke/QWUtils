#' Count lines of a function definition
#'
#' @param fun Function name.
#'
#' @export
count_function_lines <- function(fun) {
  length(deparse(fun))
}
