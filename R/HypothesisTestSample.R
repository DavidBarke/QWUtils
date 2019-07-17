#' HypothesisTestSample
#'
#' \code{\link[R6:R6Class]{R6Class}} representing a sample of a
#' \code{\link{HypothesisTest}}
#'
#' @section Usage:
#' \preformatted{sample <- HypothesisTestSample$new()
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name = "", value = numeric())}}{Initialize a new sample
#'   object.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. The sample's
#'       name. \cr
#'       \code{value} \tab A vector (not necessarily numeric) representing the
#'       sample's value.
#'     }
#'   }
#'   \item{get_id()}{Get the sample's id.
#'   }
#'   \item{get_name()}{Get the sample's name.
#'   }
#'   \item{get_value()}{Get the sample's value.
#'   }
#'   \item{set_name(name)}{Set the sample's name with \code{name}.
#'   }
#'   \item{set_value(value)}{Set the sample's value with \code{value}.
#'   }
#' }
#'
#' @name HypothesisTestSample
NULL

#' @export
HypothesisTestSample <- R6::R6Class(
  classname = "HypothesisTestSample",
  public = list(
    initialize = function(name = "", value = numeric()) {
      if (!("counter" %in% names(self$static))) {
        self$static$counter <- 1
      } else {
        self$static$counter <- self$static$counter + 1
      }
      private$id <- as.character(self$static$counter)

      private$name <- QWUtils::reactive_member(name)
      private$value <- QWUtils::reactive_member(value)
    },

    static = new.env(),

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$name()
    },

    get_value = function() {
      private$value()
    },

    set_name = function(name) {
      private$name(name)
    },

    set_value = function(value) {
      private$value(value)
    }
  ),
  private = list(
    id = character(),
    name = NULL,
    value = NULL
  )
)
