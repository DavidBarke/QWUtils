#' ObjectStorage
#'
#' R6Class for storing similar R6 objects. These objects all need to implement
#' the method get_id(), get_name() and is_removed(). The id of an object is
#' unique in a storage whereas multiple objects can share the same name.
#'
#' @section Usage:
#' \preformatted{storage = ObjectStorage$new(allowed_classes = NULL)
#'
#' storage$add_object(object)
#'
#' storage$get_object(id)
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
#'       \item{\code{object}}{R6 object with public method \code{get_id()}.}
#'     }
#'   }
#'   \item{\code{get_ids()}}{Get the ids of the stored objects as a
#'     character vector.
#'   }
#'   \item{\code{get_object(id, lazy = FALSE)}}{Get an object from the storage
#'     with \code{object$get_id() == id}.
#'     \describe{
#'       \item{\code{id}}{id of an R6 object.}
#'     }
#'   }
#'   \item{\code{get_objects(ids)}}{Get a list of objects from the storage
#'     with \code{object$get_id() \%in\% ids}.
#'     \describe{
#'       \item{\code{ids}}{Character vector. Each element has to be a id of
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

      private$storage_ids <- shiny::reactive({
        purrr::map_chr(private$storage(), function(object) {
          object$get_id()
        })
      })

      private$storage_is_removed <- shiny::reactive({
        purrr::map_lgl(private$storage(), function(object) {
          object$is_removed()
        })
      })

      private$storage_names <- shiny::reactive({
        purrr::map_chr(private$storage(), function(object) {
          object$get_name()
        })
      })

      private$allowed_classes <- allowed_classes

      invisible(self)
    },

    add_object = function(object) {
      if (!exists(
        x = "get_id",
        where = object
      )) {
        stop(
          "ObjectStorage: object has to have a method with name \"get_id\""
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

    # names of the ids are the current names
    get_ids = function(removed = FALSE) {
      if (removed) {
        ids <- private$storage_ids()
        names(ids) <- private$storage_names()
      } else {
        ids <- private$storage_ids()[!private$storage_is_removed()]
        names(ids) <- private$storage_names()[!private$storage_is_removed()]
      }

      ids
    },

    get_names = function(removed = FALSE) {
      if (removed) {
        return(private$storage_names())
      } else {
        return(private$storage_names()[!private$storage_is_removed()])
      }
    },

    get_object = function(id) {
      index <- which(private$storage_ids() == id)
      if (length(index) != 1) {
        stop(paste0("There are either no or multiple objects with id ", id))
      }
      private$storage()[[index]]
    },

    get_objects = function(ids) {
      if (!(all(ids %in% self$get_ids()))) {
        stop("ObjectStorage: Not all ids are present in the ids of the
          storage object.")
      }
      positions <- map_dbl(ids, function(id) {
        which(self$get_ids() == id)
      })
      objects <- private$storage()[positions]
      ids(objects) <- ids
      objects
    }
  ),
  private = list(
    allowed_classes = NULL,
    length = NULL,
    storage = NULL,
    storage_ids = NULL,
    storage_is_removed = NULL,
    storage_names = NULL
  )
)
