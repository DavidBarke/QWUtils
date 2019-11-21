#' Replace Code
#'
#' Replace code in .R scripts. Be sure to use version control as wrong patterns
#' might result in unintended replacements. Note that you should specify the
#' pattern by \code{stingr::regex(pattern, dotall = TRUE)} to also match line
#' terminators (all lines of a script are pasted into a single string).
#'
#' @param file A path to an .R script or a directory containing .R scripts.
#' @inheritParams stringr::str_replace
#' @param all If \code{\link[base:logical]{TRUE}}, replacement is done using
#' \code{\link[stringr:str_replace]{str_replace_all}},
#' if \code{\link[base:logical]{FALSE}} using \code{\link[stringr]{str_replace}}.
#'
#' @export
replace_code <- function(
  file, pattern, replacement, ending = ".*[.](r|R|s|S|q)([.](lnk|LNK))*$",
  all = FALSE
) {
  if (R.utils::isDirectory(file)) {
    scripts <- script_paths(file, ending = ending)
  } else {
    scripts <- file
  }

  replace_fun <- if (all) {
    stringr::str_replace_all
  } else {
    stringr::str_replace
  }

  script_texts <- purrr::map_chr(scripts, function(script) {
    paste0(readLines(script), collapse = "\n")
  })

  purrr::walk2(script_texts, scripts, function(text, file) {
    text <- replace_fun(text, pattern, replacement)

    writeLines(text, file)
  })
}
