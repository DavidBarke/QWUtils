#' Count the lines of all scripts inside a directory
#'
#' Count the lines of all scripts inside a directory.
#'
#' @inheritParams script_paths
#' @param output If \code{"sum"}, calculate the sum of all lines; if
#' \code{"separated"}, show number of lines for each individual script.
#'
#' @export
count_lines <- function(
  directory, recursive = TRUE, output = c("sum", "separated")
) {
  output <- match.arg(output)

  scripts <- script_paths(directory, recursive = recursive)

  lines <- purrr::map_dbl(scripts, function(script) {
    length(readLines(script))
  })

  if (output == "sum") {
    return(sum(lines))
  } else {
    names(lines) <- scripts
    return(lines)
  }
}
