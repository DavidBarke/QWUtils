#' ErrorControllerList
#'
#' Reactive R6Class that returns HTML according to a test function. Modules like
#' \code{\link{checked_text_input}} or \code{\link{observed_vector_input}}
#' provide a argument accepting an \code{\link{ErrorController}} or
#' ErrorControllerList that expands the module's default tests.
#'
#' @section Usage:
#' \preformatted{
#' error_controller <- shiny::isolate({
#'   ErrorControllerList$new()$add_error_controller(
#'     ErrorController$new(
#'       fun = function(x) purrr::is_null(x),
#'       ui_true = "x is NULL",
#'       ui_false = "x is not NULL",
#'       error = TRUE
#'     )
#'   )})
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{new()}{Initialize the error controller list.
#'   }
#'   \item{add_error_controller(error_controller)}{Add an \code{\link{ErrorController}}
#'     to the error controller list.
#'   }
#'   \item{get_ui()}{Get the UI of all \code{\link{ErrorController}} as a list
#'     ordered by the priority of the respective \code{\link{ErrorController}}.
#'   }
#'   \item{has_error()}{Returns \code{\link[base:logical]{TRUE}} if any of
#'   the \code{\link{ErrorController}} has an error.
#'   }
#'   \item{set_value(value)}{Sets the value of all \code{\link{ErrorController}}.
#'   }
#' }
#'
#' @name ErrorControllerList
NULL

#' @export
ErrorControllerList <- R6::R6Class(
  classname = "ErrorControllerList",
  public = list(
    initialize = function() {
      private$storage <- shiny::reactiveVal(list())

      private$order <- shiny::reactive({
        priority <- purrr::map_dbl(
          private$storage(),
          function(error_controller) {
            error_controller$get_priority()
          }
        )

        order(priority, decreasing = TRUE)
      })

      self
    },

    add_error_controller = function(error_controller) {
      stopifnot("ErrorController" %in% class(error_controller))

      storage <- private$storage()
      storage[[length(storage) + 1]] <- error_controller
      private$storage(storage)

      self
    },

    get_ui = function(ordered = TRUE) {
      if (ordered) {
        storage <- private$storage()[private$order()]
      } else {
        storage <- private$storage()
      }

      purrr::map(storage, function(error_controller) {
        error_controller$get_ui()
      })
    },

    has_error = function() {
      any(purrr::map_lgl(private$storage(), function(error_controller) {
        error_controller$has_error()
      }))
    },

    set_value = function(value) {
      purrr::walk(private$storage(), function(error_controller) {
        error_controller$set_value(value)
      })
    }
  ),
  private = list(
    storage = NULL,
    order = NULL
  )
)
