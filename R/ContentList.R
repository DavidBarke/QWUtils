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
#'   \item{\code{add_content_element(content_element, hidden = TRUE)}}{Add a
#'   content element to the content list.
#'     \tabular{ll}{
#'       \code{content_element} \tab A content element object. \cr
#'       \code{hidden} \tab If \code{\link[base:logical]{TRUE}}, this content
#'         element starts in a hidden state. Use \code{this$show_content_element()}
#'         to show it.
#'     }
#'   }
#'   \item{\code{append_tab(content_element_id, tab, select = FALSE,
#'     closeable = FALSE)}}{Append a tab to the content element with
#'     \code{id == content_element_id}.
#'       \tabular{ll}{
#'         \code{content_element_id} \tab \code{\link[base:character]{Character}}.
#'         The id of the content element. \cr
#'         \code{tab} \tab The item to be added (must be created with
#'         \code{\link[shiny:tabPanel]{tabPanel}}). \cr
#'         \code{select} \tab If \code{\link[base:logical]{TRUE}} \code{tab} gets
#'         selected upon being inserted. \cr
#'         \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, tabPanel is
#'         closeable via an button with cross icon next to the tabPanel's title.
#'       }
#'   }
#'   \item{\code{get_content_element(content_element_id)}}{Get the content
#'   element with \code{id == content_element_id}.
#'   }
#'   \item{\code{get_content_element_ids()}}{Get the ids of the content elements
#'   as a \code{\link[base:character]{character}} vector.
#'   }
#'   \item{\code{hide_content_element(content_element_id)}}{Hide the content
#'   element with \code{id == content_element_id}.
#'   }
#'   \item{\code{remove_tab(content_element_id, target)}}{Dynamically remove a
#'     \code{\link[shiny]{tabPanel}} from an existing content element.
#'    \tabular{ll}{
#'      \code{content_element_id} \tab Id of content element. \cr
#'      \code{target} \tab The \code{value} of the \code{tabPanel} to be removed.
#'    }
#'   }
#'   \item{\code{show_content_element(content_element_id)}}{Show the content element
#'   with \code{id == content_element_id}.
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

    add_content_element = function(content_element, hidden = TRUE) {
      private$add(content_element, hidden)
    },

    append_tab = function(content_element_id, tab, select = FALSE, closeable = FALSE) {
      content_element <- self$get_content_element(content_element_id)

      content_element$show()

      content_element$append_tab(
        tab = tab,
        select = select,
        closeable = closeable
      )
    },

    get_content_element = function(content_element_id) {
      stopifnot(content_element_id %in% private$element_ids)
      private$elements[[which(private$element_ids == content_element_id)]]
    },

    get_content_element_ids = function() {
      private$element_ids
    },

    hide_content_element = function(content_element_id) {
      content_element <- self$get_content_element(content_element_id)
      content_element$hide()
    },

    remove_tab = function(content_element_id, target) {
      content_element <- self$get_content_element(content_element_id)
      content_element$remove_tab(target)
    },

    show_content_element = function(content_element_id) {
      content_element <- self$get_content_element(
        content_element_id
      )
      content_element$show()
    },

    update_tab = function(content_element_id, selected) {
      content_element <- self$get_content_element(content_element_id)

      content_element$show()

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
    open_element_container_ids = character(),

    add = function(content_element, hidden = TRUE) {
      # Check if content_element with same id has been added before
      content_element_id <- content_element$get_id()

      if (!content_element_id %in% private$element_ids) {
        ns <- private$ns

        private$element_ids <- c(private$element_ids, content_element_id)
        private$elements <- c(private$elements, content_element)
        insertUI(
          selector = paste0("#", ns(private$container_id)),
          where = "beforeEnd",
          ui = content_element$ui(hidden),
          session = private$session
        )
      }
    },

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
