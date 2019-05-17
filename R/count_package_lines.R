#' Count the lines of all functions in a loaded package
#'
#' Count the lines of all functions in a loaded package, which is NOT equal to
#' the number of lines of the scripts defining the functions due to the absence
#' of the roxygen documentation. Furthermore only functions are considered and
#' not for example R6 objects.
#'
#' @param package Package name.
#' @param Output If \code{"sum"}, calculate the sum of all lines; if
#' \code{"separated"}, show number of lines for each individual package
#' function.
#'
#' @export
count_package_lines <- function(package, output = c("sum", "separated")) {
  output <- match.arg(output)

  funs <- package_functions(package)
  lines <- purrr::map_dbl(funs, function(fun) {
    fun <- get(fun, pos = paste0("package:", package))
    count_function_lines(fun)
  })

  if (output == "sum") {
    sum(lines)
  } else {
    names(lines) <- funs
    lines
  }
}
