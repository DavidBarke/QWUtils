#' ErrorController
#'
#' Reactive R6Class that returns HTML according to a test function. Modules like
#' \code{\link{checked_text_input}} or \code{\link{module_vector_text_input}}
#' provide a argument accepting an ErrorController or \code{\link{ErrorControllerList}}
#' that expands the module's default tests.
#'
#' @section Usage:
#' \preformatted{error_controller <- shiny::isolate({ErrorController$new(
#'   fun = function(x) purrr::is_null(x),
#'   ui_true = "x is NULL",
#'   ui_false = "x is not NULL",
#'   error = TRUE
#' )})
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(fun, ui_true = NULL, ui_false = NULL, priority = 0, error = NULL)}}{
#'     Initialize the error controller.
#'     \tabular{ll}{
#'       \code{fun} \tab \code{\link[base:function]{Function}} with exactly one
#'       argument returning \code{\link[base:logical]{TRUE}} or
#'       \code{\link[base:logical]{FALSE}}. \cr
#'       \code{ui_true} \tab HTML-like \code{\link[htmltools:tag]{tag}} to be
#'       returned by \code{this$get_ui()}, if \code{this$fun(this$value) == TRUE}. \cr
#'       \code{ui_false} \tab HTML-like \code{\link[htmltools:tag]{tag}} to be
#'       returned by \code{this$get_ui()}, if \code{this$fun(this$value) == FALSE}. \cr
#'       \code{priority} \tab \code{\link[base:numeric]{Numeric}} which is used
#'       by \code{\link{ErrorControllerList}} to determine the order in which
#'       to display the UIs of their error controllers. \cr
#'       \code{error} \tab Determine which result of this$fun(this$value) shall
#'       be counted as an error or use \code{\link[base:NULL]{NULL}} if either
#'       result produces no error, which is useful for displaying warnings or
#'       help messages based on user input.
#'     }
#'   }
#'   \item{\code{get_priority()}}{Get the error controller's priority.
#'   }
#'   \item{\code{get_ui()}}{Get the error controller's ui.
#'   }
#'   \item{\code{has_error()}}{Get a \code{\link[base:logical]{logical}} indicating
#'    whether this$fun(this$value) equals this$error. If \code{error = NULL}
#'    in this$new(), this method always returns false.
#'   }
#'   \item{\code{set_priority(priority)}}{Set the priority of the error controller.
#'   }
#'   \item{\code{set_value}}{Set the value which is checked by this$fun().
#'   }
#' }
#'
#' @name ErrorController
NULL

#' @export
ErrorController <- R6::R6Class(
  classname = "ErrorController",
  public = list(
    initialize = function(
      fun, ui_true = NULL, ui_false = NULL, priority = 0, error = NULL
    ) {
      private$error <- error
      private$fun <- fun
      private$priority <- shiny::reactiveVal(priority)
      private$ui_true <- ui_true
      private$ui_false <- ui_false
      private$value <- shiny::reactiveVal()
    },

    get_priority = function() {
      private$priority()
    },

    get_ui = function() {

      if (purrr::is_null(private$value())) {
        ui <- NULL
      } else {
        if (private$fun(private$value())) {
          ui <- QWUtils::handle_fun(private$ui_true)
        } else {
          ui <- QWUtils::handle_fun(private$ui_false)
        }
      }

      ui
    },

    has_error = function() {
      if (purrr::is_null(private$value()) || purrr::is_null(private$error)) {
        error <- FALSE
      } else {
        error <- private$error == private$fun(private$value())
      }

      error
    },

    set_priority = function(priority) {
      private$priority(priority)
    },

    set_value = function(value) {
      private$value(value)
    }
  ),
  private = list(
    error = NULL,
    fun = NULL,
    priority = NULL,
    ui_true = NULL,
    ui_false = NULL,
    value = NULL
  )
)
