#' Collapsible tab box
#'
#' Create a collapsible \code{\link[shinydashboard]{tabBox}}.
#'
#' @param ... \code{\link[shiny]{tabPanel}} elements to include in the tabset
#' @param id If provided, you can use \code{input$id} in your server logic to
#' determine which of the current tabs is active. The value will correspond to
#' the \code{value} argument that is passed to \code{\link[shiny]{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#' of the tab that should be selected by default. If \code{NULL}, the first tab
#' will be selected.
#' @param title Title for the tabBox.
#' @param width The width of the box, using the Bootstrap grid system. This is
#' used for row-based layouts. The overall width of a region is 12, so the
#' default valueBox width of 4 occupies 1/3 of that width. For column-based
#' layouts, use \code{NULL} for the width; the width is set by the column that
#' contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#' the height scales automatically with the content.
#' @param side Which side of the box the tabs should be on (\code{"left"} or
#' \code{"right"}). When \code{side="right"}, the order of tabs will be
#' reversed.
#' @param closeable Only functional in \code{\link{tabList_R6}}.
#'
#' @details
#' The \code{collapsible_tabBox} has class "collapsible-tab-box", the enclosing
#' div of the closing \code{\link[shiny]{actionButton}} has class
#' "div-btn-close".
#'
#' @export
collapsible_tabBox <- function(
  ..., id = NULL, selected = NULL, title = NULL, width = 6, height = NULL,
  side = c("left", "right")
) {
  print("ID")
  print(id)
  unique_id <- shiny:::createUniqueId()
  ui <- shinydashboard::box(
    title = title,
    collapsible = TRUE,
    width = width,
    height = height,
    shinydashboard::tabBox(
      id = id,
      width = 12,
      side = match.arg(side),
      selected = selected,
      ...
    )
  )
  ui$children[[1]]$children[[2]]$children[[1]]$attribs$class <- paste(
    ui$children[[1]]$children[[2]]$children[[1]]$attribs$class,
    "collapsible-tab-box"
  )
  ui
}
