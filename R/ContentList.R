#' Content List
#'
#' \code{\link[R6:R6Class]{R6Class}} for storing content in a list. Note that
#' \code{this$container()} and \code{this$set_session(session)} must be called
#' in order for the content list to work properly.
#'
#' @section Usage:
#' \preformatted{content_list <- ContentList$new("content")
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, sortable = FALSE)}}{Initialize the content list.
#'     \tabular{ll}{
#'       \code{id} \tab \code{\link[base:character]{Character}}. The id of the
#'       content list's container. If the session associated with the content
#'       list is a session proxy (session inside of a module), the id gets
#'       namespaced. \cr
#'       \code{sortable} \tab Currently defunct.
#'     }
#'   }
#'   \item{\code{container()}}{Call this function in the UI definition. This
#'   function returns the div-element containing the content list.
#'   }
#'   \item{\code{set_session(session)}}{Call this function in the server function
#'   to connect the content list with a session.
#'     \tabular{ll}{
#'       \code{session} \tab A shiny \code{\link[shiny:session]{session}} object.
#'     }
#'   }
#'   \item{\code{add_element(content_element)}}{Add a content element to the
#'   content list. If this particular element has already been added and it is
#'   currently hidden, it gets shown.
#'     \tabular{ll}{
#'       \code{content_element} \tab A content element object.
#'     }
#'   }
#'   \item{\code{add_element_actionButton(content_element, actionButton_id,
#'   actionButton_session)}}{Add a content element to the content list, that gets
#'   displayed after an \code{\link[shiny:actionButton]{actionButton}} has been
#'   clicked.
#'     \tabular{ll}{
#'       \code{content_element} \tab A content element object. \cr
#'       \code{actionButton_id} \tab The inputId associated with the
#'       \code{\link[shiny:actionButton]{actionButton}}. \cr
#'       \code{actionButton_session} \tab The shiny \code{\link[shiny:session]{session}}
#'       object that the \code{\link[shiny:actionButton]{actionButton}} belongs
#'       to.
#'     }
#'   }
#'   \item{\code{add_element_by_id(content_element_id)}}{Show the content element
#'   with \code{id == content_element_id}.
#'   }
#'   \item{\code{append_tab(content_element_id, tab, select = FALSE)}}{Append a
#'   tab to the content element with \code{id == content_element_id}.
#'     \tabular{ll}{
#'       \code{content_element_id} \tab \code{\link[base:character]{Character}}.
#'       The id of the content element. \cr
#'       \code{tab} \tab The item to be added (must be created with
#'       \code{\link[shiny:tabPanel]{tabPanel}}). \cr
#'       \code{select} \tab If \code{\link[base:logical]{TRUE}} \code{tab} gets
#'       selected upon being inserted.
#'     }
#'   }
#'   \item{\code{get_content_element_by_id(content_element_id)}}{Get the content
#'   element with \code{id == content_element_id}.
#'   }
#'   \item{\code{get_content_element_ids()}}{Get the ids of the content elements
#'   as a \code{\link[base:character]{character}} vector.
#'   }
#'   \item{\code{hide_content_element_by_id(content_element_id)}}{Hide the content
#'   element with \code{id == content_element_id}.
#'   }
#'   \item{\code{update_tab(content_element_id, selected)}}{Update the selected
#'   tab of the content element with \code{id == content_element_id}}.
#' }
#'
#' @name ContentList
NULL

#' @export
ContentList <- R6::R6Class(
  "ContentList",
  public = list(
    initialize = function(id, sortable = FALSE) {
      private$container_id <- "list_container" %_% id
      private$sortable <- sortable
    },

    container = function() {
      ns <- private$ns
      if (!private$container_created) {
        private$container_created <- TRUE
        ui <- htmltools::tags$ul(
          id = ns(private$container_id),
          class = "sortable"
        )
        return(ui)
      } else {
        stop("ContentList$container() must not be called more than once")
      }
    },

    set_session = function(session) {
      private$session = session
    },

    add_element = function(content_element) {
      ns <- private$ns
      if (!content_element$get("id") %in% private$element_ids) {
        private$element_ids <- c(private$element_ids, content_element$get("id"))
        private$elements <- c(private$elements, content_element)
        insertUI(
          selector = paste0("#", ns(private$container_id)),
          where = "beforeEnd",
          ui = content_element$ui(),
          session = private$session
        )
      } else {
        element_container_id = content_element$get("container_id")
        shinyjs::show(
          selector = paste0("#", element_container_id)
        )
      }
    },

    add_element_actionButton = function(
      content_element, actionButton_id, actionButton_session
    ) {
      ns <- private$ns
      element_container_id <- content_element$get("container_id")
      observeEvent(
        actionButton_session$input[[actionButton_id]],
        handler.quoted = TRUE,
        handlerExpr = substitute(
          expr = {
            if (!content_element$get("id") %in% private$element_ids) {
              private$element_ids <- c(private$element_ids, content_element$get("id"))
              private$elements <- c(private$elements, content_element)
              insertUI(
                selector = paste0("#", ns(private$container_id)),
                where = "beforeEnd",
                ui = content_element$ui(),
                session = private$session
              )
            } else {
              show(
                selector = paste0("#", element_container_id)
              )
            }
          },
          env = list(
            element_container_id = element_container_id
          )
        )
      )
      # Make content list sortable, this piece of code is called every time a
      # new content element is added.
      if (private$sortable) {
        # jqui_sortable(
        #   paste0("#", ns(private$container_id))
        # )
      }
    },

    add_element_by_id = function(content_element_id) {
      content_element <- self$get_content_element_by_id(
        content_element_id
      )
      self$add_element(content_element)
    },

    append_tab = function(content_element_id, tab, select = FALSE) {
      content_element <- self$get_content_element_by_id(
        id = content_element_id
      )
      stopifnot("content_tabBox" %in% class(content_element))
      content_element$append_tab(
        tab = tab,
        select = select
      )
    },

    get_content_element_by_id = function(id) {
      stopifnot(id %in% private$element_ids)
      private$elements[[which(private$element_ids == id)]]
    },

    get_content_element_ids = function() {
      private$element_ids
    },

    hide_content_element_by_id = function(content_element_id) {
      if (content_element_id %in% private$element_id) {
        content_element <- self$get_content_element_by_id(content_element_id)
        element_container_id <- content_element$get("container_id")
        shinyjs::hide(
          selector = paste0("#", element_container_id)
        )
      }
    },

    update_tab = function(content_element_id, selected) {
      content_element <- self$get_content_element_by_id(
        id = content_element_id
      )
      stopifnot("content_tabBox" %in% class(content_element))
      content_element$update_tab(
        selected = selected
      )
    }
  ),
  private = list(
    container_created = FALSE,
    container_id = NULL,
    sortable = FALSE,
    session = NULL,
    element_ids = list(),
    elements = list(),

    ns = function(id) {
      if (is.null(private$session)) {
        ns <- NS(NULL)
      } else {
        ns <- private$session$ns
      }
      ns(id)
    }
  )
)
