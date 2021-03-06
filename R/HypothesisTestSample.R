#' HypothesisTestSample
#'
#' \code{\link[R6:R6Class]{R6Class}} representing a sample of a
#' \code{HypothesisTest}
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
#'   \item{\code{get_id()}}{Get the sample's id.
#'   }
#'   \item{\code{get_name()}}{Get the sample's name.
#'   }
#'   \item{\code{get_value()}}{Get the sample's value.
#'   }
#'   \item{\code{set_name(name)}}{Set the sample's name with \code{name}.
#'   }
#'   \item{\code{set_value(value)}}{Set the sample's value with \code{value}.
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
      private$id <- stringi::stri_rand_strings(1, 8)

      private$name <- QWUtils::reactive_member(name)
      private$value <- QWUtils::reactive_member(value)
    },

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
