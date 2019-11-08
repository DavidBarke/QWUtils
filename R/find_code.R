#' Find scripts containing specific code
#'
#' Find all the R scripts in a directory that contain some code as defined in
#' \code{pattern}. This is especially useful when functions are redesigned
#' and have to be adjusted in all scripts where they are used.
#'
#' @inheritParams script_paths
#' @inheritParams stringr::str_detect
#' @param count If \code{\link[base:logical]{TRUE}}, a vector with the number
#' of occurences per script is returned. If \code{\link[base:logical]{FALSE}},
#' a \code{\link[base:character]{character}} vector with the scripts containing
#' at least one occurence is returned.
#' @param indices \code{\link[base:integer]{Integer}} vector specifying the indices
#' of lines to be searched in each script. This is useful, if you get an Shiny
#' error indicating the line in which the error occured, but not the script. If
#' \code{\link[base]{NULL}} all lines of each script get searched.
#'
#' @export
find_code <- function(
  directory, pattern, recursive = TRUE, count = FALSE, indices = NULL
) {
  scripts <- script_paths(directory, recursive = recursive)

  list_of_lines <- purrr::map(scripts, function(script) {
    suppressWarnings(readLines(script))
  })

  if (!purrr::is_null(indices)) {
    list_of_lines <- purrr::map(list_of_lines, function(lines) {
      stats::na.omit(lines[indices])
    })
  }

  detected <- purrr::map_lgl(list_of_lines, function(lines) {
    any(stringr::str_detect(lines, pattern = pattern))
  })

  if (count) {
    detected_lines <- list_of_lines[detected]
    counts <- purrr::map_int(detected_lines, function(lines) {
      count_occurences <- purrr::map_int(lines, function(line) {
        stringr::str_count(line, pattern = pattern)
      })
      sum(count_occurences)
    })
    names(counts) <- scripts[detected]
    return(counts)
  } else {
    return(scripts[detected])
  }
}
