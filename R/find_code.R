#' Find scripts containing specific code
#'
#' Find all the R scripts in a directory that contain some code as defined in
#' \code{pattern}. This is especially useful when functions are redesigned
#' and have to be adjusted in all scripts where they are used.
#'
#' @inheritParams script_paths
#' @inheritParams stringr::str_detect
#'
#' @export
find_code <- function(directory, pattern, recursive = TRUE) {
  scripts <- script_paths(directory, recursive = recursive)

  list_of_lines <- purrr::map(scripts, function(script) {
    suppressWarnings(readLines(script))
  })

  detected <- purrr::map_lgl(list_of_lines, function(lines) {
    any(stringr::str_detect(lines, pattern = pattern))
  })

  scripts[detected]
}
