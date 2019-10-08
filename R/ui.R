#' Create a themed actionButton
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button or link-usually a text label, but you
#' could also use any other HTML, like an image.
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param style Style of the button, to choose between \code{simple},
#' \code{bordered}, \code{minimal}, \code{stretch}, \code{jelly}, \code{gradient},
#' \code{fill}, \code{material-circle}, \code{material-flat}, \code{pill},
#' \code{float}, \code{unite}.
#' @param color Color of the button: \code{default}, \code{primary},
#' \code{warning}, \code{danger}, \code{success}, \code{royal}.
#' @param size Size of the button: \code{xs}, \code{sm}, \code{md}, \code{lg}.
#' @param block Logical, full width button.
#' @param no_outline Logical, don't show outline when navigating with keyboard/
#' interact using mouse or touch.
#' @param tooltip Text to appear as a tooltip on the button.
#' @param dropdown Logical, button displayed in dropdown.
#'
#' @export
actionButtonQW <- function(
  inputId, label, icon = NULL, style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE, tooltip = NULL, ...,
  dropdown = FALSE
) {
  if (dropdown) {
    ui <- div(
      style = "margin: 0px 2px",
      shinyWidgets::actionBttn(
        inputId = inputId,
        label = label,
        icon = icon,
        style = style,
        color = color,
        size = size,
        block = block,
        no_outline = no_outline
      )
    )
  } else {
    ui <- shinyWidgets::actionBttn(
      inputId = inputId,
      label = label,
      icon = icon,
      style = style,
      color = color,
      size = size,
      block = block,
      no_outline = no_outline
    )
  }

  if (!is.null(tooltip)) {
    ui <- tagList(
      ui,
      shinyBS::bsTooltip(
        id = inputId,
        title = tooltip,
        placement = "top"
      )
    )
  }

  ui
}

#' Search Input
#'
#' @export
searchInputQW <- function(
  inputId, label = NULL, value = "", placeholder = NULL, btnSearch = NULL,
  btnReset = NULL, resetValue = "", width = NULL, disable_text = FALSE,
  disable_search = FALSE, disable_reset = FALSE
) {
  value <- shiny::restoreInput(id = inputId, default = value)

  if (!is.null(btnSearch)) {
    btnSearch <- htmltools::tags$button(
      class = "btn btn-default btn-addon action-button",
      id = paste0(inputId, "_search"),
      type = "button",
      btnSearch
    )

    if (disable_search) btnSearch <- htmltools::tagAppendAttributes(
      btnSearch, disabled = "disabled"
    )

  }

  if (!is.null(btnReset)) {
    btnReset <- htmltools::tags$button(
      class = "btn btn-default btn-addon action-button",
      id = paste0(inputId, "_reset"),
      type = "button",
      btnReset
    )

    if (disable_reset) btnReset <- htmltools::tagAppendAttributes(
      btnReset, disabled = "disabled"
    )

  }
  css_btn_addon <- paste0(
    ".btn-addon{", "font-size:14.5px;", "margin:0 0 0 0 !important;",
    "display: inline-block !important;","}"
  )

  text_input <- htmltools::tags$input(
    id = paste0(inputId,  "_text"),
    style = "border-radius: 0.25em 0 0 0.25em !important;",
    type = "text",
    class = "form-control",
    value = value,
    placeholder = placeholder
  )

  if (disable_text) text_input <- htmltools::tagAppendAttributes(
    text_input, disabled = "disabled"
  )

  searchTag <- htmltools::tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    if (!is.null(label)) htmltools::tags$label(label, `for` = inputId),
    htmltools::tags$div(
      id = inputId,
      `data-reset` = !is.null(resetValue),
      `data-reset-value` = resetValue,
      class = "input-group search-text",
      text_input,
      htmltools::tags$div(
        class = "input-group-btn",
        btnReset,
        btnSearch
      )
    ),
    singleton(tags$head(tags$style(css_btn_addon))))

  shinyWidgets:::attachShinyWidgetsDep(searchTag)
}
