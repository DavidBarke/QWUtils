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
#' @param adjust_height Numeric, margin-bottom in px.
#'
#' @export
actionButtonQW <- function(
  inputId, label, icon = NULL, style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE, tooltip = NULL,
  dropdown = FALSE, adjust_height = 0
) {
  if (dropdown) {
    margin_left = "2px"
    margin_right = "2px"
  } else {
    margin_left = "0px"
    margin_right = "0px"
  }

  margin_bottom = paste0(adjust_height, "px")

  style <- paste("margin: 0px", margin_right, margin_bottom, margin_left)

  ui <- div(
    style = style,
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
