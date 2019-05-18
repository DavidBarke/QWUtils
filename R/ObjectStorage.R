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
#'   \item{\code{get_object(name, lazy = FALSE)}}{Get an object from the storage
#'     with \code{object$get_name() == name}.
#'     \describe{
#'       \item{\code{name}}{Name of an R6 object.}
#'       \item{\code{lazy}}{If \code{\link[base:logical]{TRUE}}, allow a name
#'         which is not present in the names of the storage for a short time.
#'       }
#'     }
#'   }
#'   \item{\code{get_objects(names)}}{Get a list of objects from the storage
#'     with \code{object$get_name() \%in\% names}.
#'     \describe{
#'       \item{\code{names}}{Character vector. Each element has to be a name of
#'         an object in the storage.
#'       }
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

      invisible(self)
    },

    add_object = function(object) {
      if (!exists(
        x = "get_name",
        where = object
      )) {
        stop(
          "ObjectStorage: object has to have a method with name \"get_name\""
        )
      }
      if (!is.null(private$allowed_classes)) {
        stopifnot(any(private$allowed_classes %in% class(object)))
      }
      storage <- private$storage()
      storage[[private$length() + 1]] <- object
      private$storage(storage)
      invisible(self)
    },

    get_length = function() {
      length(private$storage())
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
    },

    get_objects = function(names) {
      if (!(all(names %in% self$get_names()))) {
        stop("ObjectStorage: Not all names are present in the names of the
          storage object.")
      }
      positions <- map_dbl(names, function(name) {
        which(self$get_names() == name)
      })
      objects <- private$storage()[positions]
      names(objects) <- names
      objects
    }
  ),
  private = list(
    allowed_classes = NULL,
    length = NULL,
    storage = NULL,
    storage_names = NULL
  )
)
