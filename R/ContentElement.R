#' Content Element
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, title, ...,
#'     session = shiny::getDefaultReactiveDomain, footer = NULL, closeable = FALSE,
#'     collapsible = TRUE, width = 12, height = NULL, tabPanel_list = NULL)}}{
#'     \tabular{ll}{
#'       \code{id} \tab The content element's id. \cr
#'       \code{title} \tab Title for the tabBox. \cr
#'       \code{...} \tab \code{\link[shiny]{tabPanel}} elements to include in the
#'         tabset. \cr
#'       \code{session} \tab \cr
#'       \code{footer} \tab Footer below the tabBox. \cr
#'       \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, the content
#'         element is closeable. \cr
#'       \code{collapsible} \tab If \code{\link[base:logical]{TRUE}}, the content
#'         element is collapsible. \cr
#'       \code{width} \tab The width of the box, using the Bootsgrap grid system.
#'         This is used for row-based layouts. The overall width of a region is 12,
#'         so the default valueBox width of 4 occupies 1/3 of that width. For
#'         column-based layouts, use \code{\link[base]{NULL}} for the width; the
#'         width is set by the column that contains the box. \cr
#'       \code{height} \tab The height of a box, in pixels or other CSS unit. By
#'         default the height scales automatically with the content. \cr
#'       \code{tabPanel_list} \tab Instead of using the \code{...} argument for
#'         passing \code{\link[shiny]{tabPanel}} elements, you may pass a list
#'         of \code{tabPanel} elements in this argument. \cr
#'     }
#'   }
#'   \item{\code{append_tab(tab, select = FALSE, closeable = FALSE)}}{
#'     \tabular{ll}{
#'       \code{tab} \tab The \code{\link[shiny]{tabPanel}} element to be added. \cr
#'       \code{select} \tab Should \code{tab} be selected upon being inserted? \cr
#'       \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, the added
#'         tab is closeable via an action button. \cr
#'     }
#'   }
#'   \item{\code{hide()}}{Hide this content element.
#'   }
#'   \item{\code{get_id()}}{Get the content element's id.
#'   }
#'   \item{\code{show()}}{Show the content element.
#'   }
#'   \item{\code{show_by_action_button(id, session = shiny::getDefaultReactiveDomain())}}{
#'     Show the content element, if the \code{\link[shiny]{actionButton}} with
#'     \code{inputId == id} is clicked. \code{session} is the session, in which
#'     the actionButton was defined.
#'   }
#'   \item{\code{remove_tab(target)}}{Dynamically remove a \code{\link[shiny]{tabPanel}}.
#'     \tabular{ll}{
#'       \code{target} \tab The \code{value} of the \code{tabPanel} that you want
#'         to remove. \cr
#'     }
#'   }
#'   \item{\code{ui(hidden = TRUE)}}{Get the ui (similar to
#'     \code{\link[shinydashboard]{tabBox}}) associated with this content element.
#'     You should only call this function once. If \code{hidden}, the ui starts
#'     in a hidden mode (see \code{\link[shinyjs]{hidden}}). Use \code{this$show()}
#'     for showing the ui.
#'   }
#'   \item{\code{update_tab(selected)}}{Change the selected tab on the client.
#'     \tabular{ll}{
#'       \code{selected} \tab The name of the tab to make active.
#'     }
#'   }
#' }
#'
#' @name ContentElement
NULL

#' @export
ContentElement <- R6::R6Class(
  "ContentElement",
  public = list(
    initialize = function(
      id, title, ..., session = shiny::getDefaultReactiveDomain(), footer = NULL, closeable = TRUE,
      collapsible = TRUE, width = 12, height = NULL,
      tabPanel_list = NULL
    ) {
      private$id <- id
      private$session <- session
      private$container_id <- session$ns("container" %_% id)

      private$footer <- footer
      private$closeable <- closeable
      private$collapsible <- collapsible
      private$title <- title
      private$width <- width
      private$height <- height
      if (purrr::is_null(tabPanel_list)) {
        private$tabPanel_list <- list(...)
      } else {
        private$tabPanel_list <- tabPanel_list
      }
      invisible(self)
    },

    append_tab = function(tab, select = FALSE, closeable = FALSE) {
      data_value <- tab$attribs[["data-value"]]

      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
        return(FALSE)
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::appendTab(
          inputId = private$id,
          tab = tab,
          select = select,
          session = private$session
        )
        if (closeable) {
          private$close_btn_counter <- private$close_btn_counter + 1
          private$createCloseButton(data_value)
        }
        return(TRUE)
      }
    },

    hide = function() {
      shinyjs::hide(
        selector = paste0("#", private$container_id)
      )
    },

    get_id = function() {
      private$id
    },

    show = function() {
      shinyjs::show(
        selector = paste0("#", private$container_id)
      )
    },

    show_by_action_button = function(id, session = shiny::getDefaultReactiveDomain()) {
      shiny::observeEvent(session$input[[id]], {
        self$show()
      })
    },

    remove_tab = function(target) {
      index <- which(private$open_tab_values == target)
      private$open_tab_values <- private$open_tab_values[-index]

      shiny::removeTab(
        inputId = private$id,
        target = target,
        session = private$session
      )
      invisible(self)
    },

    ui = function(hidden = TRUE) {
      ui <- shinydashboard::box(
        title = private$title,
        collapsible = private$collapsible,
        width = private$width,
        height = private$height,
        # do.call for ...-argument
        do.call(
          tabBox,
          c(
            list(
              id = private$session$ns(private$id),
              width = 12
            ),
            private$tabPanel_list
          )
        ),
        footer = private$footer
      )

      if (private$closeable) {
        # Add close button for whole tabset
        closeId <- "close" %_% private$id
        box_header_ui <- ui$children[[1]]$children[[1]]$children
        box_header_ui[[3]] <- box_header_ui[[2]]
        box_header_ui[[2]] <- div(
          class = "div-btn-close div-btn-close-tablist",
          actionButton(
            inputId = private$session$ns(closeId),
            label = NULL,
            icon = icon("times"),
            style = "color: #97a0b3"
          )
        )
        ui$children[[1]]$children[[1]]$children <- box_header_ui
        shiny::observeEvent(private$session$input[[closeId]], {
          self$hide()
        })
      }

      ui <- htmltools::tags$li(
        id = private$container_id,
        ui
      )

      if (hidden) {
        return(shinyjs::hidden(ui))
      } else {
        return(ui)
      }
    },

    update_tab = function(selected) {
      shiny::updateTabsetPanel(
        session = private$session,
        inputId = private$id,
        selected = selected
      )
    }
  ),
  private = list(
    closeable = TRUE,
    close_btn_counter = 0,
    collapsible = TRUE,
    container_id = character(),
    footer = NULL,
    height = NULL,
    id = character(),
    open_tab_values = character(),
    session = NULL,
    tab_values = character(),
    tabPanel_list = list(),
    title = NULL,
    width = 12,

    # close button for specific tab
    createCloseButton = function(data_value) {
      closeId <- private$id %_% private$close_btn_counter

      div_button <- htmltools::div(
        class = "div-btn-close",
        shiny::actionButton(
          inputId = private$session$ns(closeId),
          label = NULL,
          icon = icon("window-close")
        )
      )

      selector <- paste0("#", private$session$ns(private$id), " li a[data-value=\"", data_value, "\"]")

      shiny::insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = div_button
      )

      shiny::observeEvent(private$session$input[[closeId]], {
        self$remove_tab(target = data_value)
      }, domain = private$session)
    }
  )
)
