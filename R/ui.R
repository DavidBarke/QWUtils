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
#' @param dropdown Logical, button displayed in dropdown.
#' @param tooltip Text to appear as a tooltip on the button.
#'
#' @export
actionButtonQW <- function(
  inputId, label, icon = NULL, style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE, dropdown = FALSE,
  tooltip = NULL
) {
  if (dropdown) {
    ui <- div(
      style = "margin: 0px 2px",
      actionBttn(
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
    ui <- actionBttn(
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
      bsTooltip(
        id = inputId,
        title = tooltip,
        placement = "top"
      )
    )
  }

  ui
}
