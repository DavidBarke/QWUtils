#' ControlChartSample
#'
#' R6Class representing a control chart sample. Multiple samples may form a
#' \code{\link{ControlChartPhase}}.
#'
#' @section Usage:
#' \preformatted{sample = ControlChartSample$new(value, name)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(value, name = character())}}{Initialize the sample.
#'     \describe{
#'       \item{\code{value}}{A \code{\link[base:numeric]{numeric}} vector.}
#'       \item{\code{name}}{\code{\link[base:character]{character}}.}
#'     }
#'   }
#'   \item{\code{get_id()}}{Get the sample's id.
#'   }
#'   \item{\code{get_length()}}{Get the sample's length.
#'   }
#'   \item{\code{get_name()}}{Get the sample's name.
#'   }
#'   \item{\code{get_value()}}{Get the sample's value.
#'   }
#'   \item{\code{set_name(name)}}{Set the sample's name.
#'   }
#'   \item{\code{set_value(value)}}{Set the sample's value.
#'     \tabular{ll}{
#'       \code{value} \tab A \code{\link[base:numeric]{numeric}} vector.
#'     }
#'   }
#' }
#'
#' @name ControlChartSample
NULL

#' @export
ControlChartSample <- R6::R6Class(
  classname = "ControlChartSample",
  public = list(
    initialize = function(value, name = character()) {
      if (!("counter" %in% names(self$static))) {
        self$static$counter <- 1
      } else {
        self$static$counter <- self$static$counter + 1
      }
      private$id <- as.character(self$static$counter)

      private$name <- shiny::reactiveVal(name)

      private$value <- shiny::reactiveVal(value)
    },

    static = new.env(),

    get_id = function() {
      private$id
    },

    get_length = function() {
      length(private$value())
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
    value = NULL,
    name = NULL
  )
)
