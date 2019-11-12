#' Count the lines of all scripts inside a directory
#'
#' Count the lines of all scripts inside a directory.
#'
#' If used with \code{comment = "roxygen"} the results are comparable to
#' \code{\link{count_package_lines}}.
#'
#' @inheritParams script_paths
#' @param output If \code{"sum"}, calculate the sum of all lines; if
#' \code{"separated"}, show number of lines for each individual script.
#' @param remove If \code{"comments"}, all lines
#' starting with "#" don't add to the count. If \code{"roxygen"}, all lines
#' starting with "#'" dont' add to the count.
#'
#' @export
count_directory_lines <- function(
  directory, recursive = TRUE, output = c("sum", "separated"),
  remove = c("none", "comments", "roxygen")
) {
  output <- match.arg(output)
  remove <- match.arg(remove)

  scripts <- script_paths(directory, recursive = recursive)

  lines <- purrr::map_dbl(scripts, function(script) {
    lines <- suppressWarnings(readLines(script))
    if (remove == "comments") {
      sum(!stringr::str_detect(lines, "^[\\s]*#"))
    } else if (remove == "roxygen") {
      sum(!stringr::str_detect(lines, "^[\\s]*#'"))
    } else {
      length(lines)
    }
  })

  if (output == "sum") {
    return(sum(lines))
  } else {
    names(lines) <- scripts
    return(lines)
  }
}
