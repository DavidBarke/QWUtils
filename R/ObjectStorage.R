#' ObjectStorage
#'
#' R6Class for storing similar R6 objects.
#'
#' @section Usage:
#' \preformatted{storage = ObjectStorage$new(allowed_classes = NULL)
#'
#' storage$add_object(object)
#'
#' storage$get_object(name)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(allowed_classes = NULL)}}{Initialize the storage.
#'     \describe{
#'       \item{\code{allowed_classes}}{Character vector. \code{class(object)} has to
#'       return at least one of these classes for being added to the storage.}
#'     }
#'   }
#'   \item{\code{add_object(object)}}{Add an object to the storage.
#'     \describe{
#'       \item{\code{object}}{R6 object with public method \code{get_name()}.}
#'     }
#'   }
#'   \item{\code{get_names()}}{Get the names of the stored objects as a
#'     character vector.
#'   }
#'   \item{\code{get_object(name)}}{Get an object from the storage with
#'     \code{object$get_name() == name}.
#'     \describe{
#'       \item{\code{name}}{Name of an R6 object.}
#'     }
#'   }
#' }
#'
#' @name ObjectStorage
NULL

#' @export
ObjectStorage <- R6::R6Class(
  classname = "ObjectStorage",
  public = list(
    initialize = function(allowed_classes = NULL) {
      private$storage <- shiny::reactiveVal(list())

      private$length <- shiny::reactive({
        length(private$storage())
      })

      private$storage_names <- shiny::reactive({
        map_chr(private$storage(), function(object) {
          object$get_name()
        })
      })

      private$allowed_classes <- allowed_classes
    },

    add_object = function(object) {
      if (!is.null(private$allowed_classes)) {
        stopifnot(any(private$allowed_classes %in% class(object)))
      }
      storage <- private$storage()
      storage[[private$length() + 1]] <- object
      private$storage(storage)
    },

    get_names = function() {
      private$storage_names()
    },

    get_object = function(name) {
      index <- which(private$storage_names() == name)
      if (length(index) != 1) {
        stop(paste0("There are either no or multiple objects with name ", name))
      }
      private$storage()[[index]]
    }
  ),
  private = list(
    allowed_classes = NULL,
    length = NULL,
    storage = NULL,
    storage_names = NULL
  )
)
