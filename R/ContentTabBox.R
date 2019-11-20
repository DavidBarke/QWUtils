ContentProto <- R6::R6Class(
  "ContentProto",
  public = list(
    initialize = function(id, session) {
      private$id <- id
      private$session <- session
      private$container_id <- private$session$ns("element_container" %_% id)
    },

    get = function(what) {
      private[[what]]
    }
  ),
  private = list(
    id = NULL,
    container_id = NULL,
    session = NULL
  )
)

#' @export
content_tab_box <- function(
  id, title, ..., footer = NULL, closeable = TRUE,
  collapsible = TRUE, width = 12, height = NULL,
  tabPanel_list = NULL, session = shiny::getDefaultReactiveDomain()
) {
  tab_box <- ContentTabBox$new(
    id = id,
    ... = ...,
    footer = footer,
    closeable = closeable,
    collapsible = collapsible,
    title = title,
    width = width,
    height = height,
    tabPanel_list = tabPanel_list,
    session = session
  )
  invisible(tab_box)
}

ContentTabBox <- R6::R6Class(
  "ContentTabBox",
  inherit = ContentProto,
  public = list(
    initialize = function(
      id, session, title, ..., footer = NULL, closeable = TRUE,
      collapsible = TRUE, width = 12, height = NULL,
      tabPanel_list = NULL
    ) {
      super$initialize(id, session)
      private$footer <- footer
      private$closeable <- closeable
      private$collapsible <- collapsible
      private$title <- title
      private$width <- width
      private$height <- height
      if (is.null(tabPanel_list)) {
        private$tabPanel_list <- list(...)
      } else {
        private$tabPanel_list <- tabPanel_list
      }
      invisible(self)
    },

    append_tab = function(tab, select = FALSE, closeable = FALSE) {
      private$tabCounter <- private$tabCounter + 1
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
        if (closeable) private$createActionButton(data_value)
        return(TRUE)
      }
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

    ui = function() {
      ui <- shinydashboard::box(
        title = private$title,
        collapsible = private$collapsible,
        width = private$width,
        height = private$height,
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
        # Add close button
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
        observeEvent(private$session$input[[closeId]], {
          hide(
            # The container_id is prefixed with ns() during creation and needs
            # no additional adjustment
            selector = paste0("#", private$container_id)
          )
        })
      }
      ui <- htmltools::tags$li(
        id = private$container_id,
        class = "ui-sortable-handle",
        ui
      )
    },

    update_tab = function(selected) {
      updateTabsetPanel(
        session = private$session,
        inputId = private$id,
        selected = selected
      )
    }
  ),
  private = list(
    open_tab_values = character(),
    tab_values = character(),
    tabCounter = 0,
    footer = NULL,
    closeable = TRUE,
    collapsible = TRUE,
    tabPanel_list = list(),
    title = NULL,
    width = 12,
    height = NULL,

    createActionButton = function(data_value) {
      closeId <- private$id %_% private$tabCounter
      div_button <- div(
        class = "div-btn-close",
        actionButton(
          inputId = private$session$ns(closeId),
          label = NULL,
          icon = icon("window-close")
        )
      )
      selector <- paste0("#", private$session$ns(private$id), " li a[data-value=\"", data_value, "\"]")
      insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = div_button
      )
      observeEvent(private$session$input[[closeId]], {
        self$remove_tab(target = data_value)
      }, domain = private$session)
    }
  )
)
