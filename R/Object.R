#' Object
#'
#' \code{\link[R6]{R6Class}} representing the most primitive object having only
#' an id and a name.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name = "")}}{Initialize a new object with name \code{name}.
#'   }
#'   \item{\code{get_id()}}{Get the object's id.
#'   }
#'   \item{\code{get_name()}}{Get the object's name.
#'   }
#'   \item{\code{set_name(name)}}{Set the object's name. Only applicable if
#'     \code{name} is not a \code{\link[shiny]{reactive}}.
#'   }
#' }
#'
#' @name Object
NULL

#' @export
Object <- R6::R6Class(
  classname = "Object",
  public = list(
    initialize = function(name = "") {
      QWUtils::handle_static_counter(private$static)
      # prefix an underscore to get a valid column name for lm
      private$id <- paste0("id_", as.character(private$static$counter))

      private$name <- QWUtils::reactive_member(name)
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$name()
    },

    set_name = function(name) {
      private$name(name)
    }
  ),
  private = list(
    id = character(),
    name = NULL,
    static = new.env()
  )
)

#' @export
GroupObject <- R6::R6Class(
  classname = "GroupObject",
  inherit = Object
)

#' @export
DatasetObject <- R6::R6Class(
  classname = "DatasetObject",
  inherit = Object,
  public = list(
    initialize = function(name = "", dataset) {
      super$initialize(name = name)

      private$dataset <- reactive_member(dataset)
    },

    get_dataset = function() {
      private$dataset()
    },

    set_dataset = function(dataset) {
      private$dataset(dataset)
    }
  ),
  private = list(
    dataset = NULL
  )
)
