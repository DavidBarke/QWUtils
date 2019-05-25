#' Create label in multiple languages
#'
#' This function returns text in the language specified by the \code{.language}
#' global variable. If this variable is not defined the first language will be
#' returned.
#'
#' @param ... Named characters.
#'
#' @export
label_lang <- function(...) {
  label_list <- list(...)
  .language <- getOption(".language", 1)
  label <- label_list[[.language]]
  return(label)
}

#' Create choices for shiny inputs in multiple languages
#'
#' Some shiny inputs like \code{\link[shiny]{selectInput}} need a named list
#' for their choices.
#'
#' @param ... Named character vectors.
#' @param value Character vector with unique values that will be returned by
#' input$<inputId>
#'
#' @export
label_lang_list <- function(..., value) {
  label <- label_lang(...)
  stopifnot(length(label) == length(value))
  label_list <- as.list(value)
  names(label_list) <- label
  label_list
}

#' @export
label_lang_convert_fun <- function(value, ...) {
  label <- label_lang(...)
  stopifnot(length(label) == length(value))
  names(label) <- value
  function(x) {
    unname(label[[x]])
  }
}

#' List with common labels.
#'
#' @export
.label <- function() {
  list(
    add_row = label_lang(
      de = "Neue Zeile",
      en = "Add row"
    ),
    add_table = label_lang(
      de = "Neue Tabelle",
      en = "New table"
    ),
    apply = label_lang(
      de = "Anwenden",
      en = "Apply"
    ),
    apply_all = label_lang(
      de = "Auf alle anwenden",
      en = "Apply to all"
    ),
    collapse_rows = label_lang(
      de = "Minimiere Zeilen",
      en = "Collapse rows"
    ),
    expand_rows = label_lang(
      de = "Maximiere Zeilen",
      en = "Expand rows"
    ),
    modal = list(
      confirm = label_lang(
        de = "Bestätigen",
        en = "Confirm"
      ),
      dismiss = label_lang(
        de = "Abbrechen",
        en = "Dismiss"
      )
    ),
    remove_row = label_lang(
      de = "Entferne Zeile",
      en = "Remove row"
    ),
    remove_table = label_lang(
      de = "Entferne Tabelle",
      en = "Remove table"
    )
  )
}
