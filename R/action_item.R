# TODO: Use ... argument of actionButton
#' Action item
#'
#' Create a \code{\link[shinydashboard:sidebarMenu]{menuItem}}-like
#' \code{\link[shiny]{actionButton}} with an enclosing div with class
#' "div-btn-sidebar".
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button or link - usually a text label, but
#' you could also use any other HTML, like an image.
#' @param An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
#' see \code{\link[shiny]{validateCssUnit}}.
#'
#' @export
actionItem <- function(inputId, label, ...) {
  if (missing(...) || is.null(list(...)[[1]])) {
    ui <- tags$li(
      `data-toggle` = "tab",
      `data-value` = "tab",
      tags$a(
        div(
          class = "div-btn-sidebar",
          shiny::actionButton(
            inputId = inputId,
            label = label
          )
        )
      )
    )
  } else {
    ui <- tags$li(
      class = "treeview",
      tags$a(
        div(
          class = "div-btn-sidebar",
          shiny::actionButton(
            inputId = inputId,
            label = label
          )
        ),
        tags$i(
          class = "fa fa-angle-left pull-right"
        )
      ),
      tags$ul(
        class = "treeview-menu",
        style = "display: none",
        `data-expanded` = inputId,
        ...
      )
    )
  }
  return(ui)
}


#' Action Subitem
#' Create a \code{\link[shinydashboard:sidebarMenu]{menuSubItem}}-like
#' \code{\link[shiny]{actionButton}} which can be passed to \code{
#' \link{actionItem}} as \code{...} argument.
#'
#' @inheritParams actionItem
#'
#' @export
actionSubItem <- function(inputId, label) {
  ui <- tags$li(
    tags$a(
      `data-toggle` = "tab",
      `data-value` = "tab",
      tags$div(
        class = "div-btn-sidebar-sub",
        shiny::actionButton(
          inputId = inputId,
          label = tags$div(
            tags$i(
              class = "fa fa-angle-double-right"
            ),
            label
          )
        )
      )
    )
  )
}


#' Create multiple actionItem
#'
#' Create multiple \code{\link{actionItem}}s which can in turn include
#' \code{\link{actionSubItem}}s.
#'
#' @param inputId_list inputIds for \code{\link[shiny]{actionButton}}. See
#' 'Examples'.
#' @param label_list labels for \code{\link[shiny]{actionButton}}. See
#' 'Examples'.
#'
#' @examples
#'
#' @export
multiple_actionItem <- function(inputId_list, label_list) {
  stopifnot(length(inputId_list) == length(label_list))
  .actionSubItem_list <- vector("list", length = length(inputId_list))
  .inputId_list <- vector("list", length = length(inputId_list))
  .label_list <- vector("list", length = length(inputId_list))
  for (i in seq_along(.actionSubItem_list)) {
    if (names(inputId_list[i]) == "" || is.null(names(inputId_list[i]))) {
      .actionSubItem_list[i] <- list(NULL)
      .inputId_list[[i]] <- inputId_list[[i]]
      .label_list[[i]] <- label_list[[i]]
    } else {
      .actionSubItem_list[[i]] = multiple_actionSubItem(
        inputId_list = inputId_list[[i]],
        label_list = label_list[[i]]
      )
      .inputId_list[[i]] <- names(inputId_list[i])
      .label_list[[i]] <- names(label_list[i])
    }
  }
  ui <- purrr::pmap(
    .l = list(
      inputId = .inputId_list,
      label = .label_list,
      .actionSubItem_list
    ),
    .f = actionItem
  )
}

#' Create multiple actionSubItem
#'
#' This function is used internal in \code{\link{
#' multiple_actionItem}}. It could be used manually as the \code{...} argument
#' to \code{\link{actionItem}}.
multiple_actionSubItem <- function(inputId_list, label_list) {
  ui <- purrr::pmap(
    .l = list(
      inputId = inputId_list,
      label = label_list
    ),
    .f = actionSubItem
  )
}
