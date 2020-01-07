#' ControlChartControlLine
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(quantile = 0)}}{Initialise a new object of class
#'   \code{ControlChartControlLine}.
#'     \tabular{ll}{
#'       \code{quantile} \tab Numeric value representing the quantile corresponding
#'       to this control line.
#'     }
#'   }
#'   \item{\code{get_id()}}{Get the control line's id.
#'   }
#'   \item{\code{get_name()}}{Get the quantile associated with this control
#'     line.
#'   }
#'   \item{\code{get_quantile()}}{Get the quantile associated with this control
#'     line.
#'   }
#'   \item{\code{get_value(params, type = c("xbar_R", "xbar_s", "R", "s", "p"))}}{
#'     Compute the value of the control line in terms of
#'     the observed variables.
#'     \tabular{ll}{
#'       \code{params} \tab List of control chart parameters returned by
#'       \code{\link[ControlChart]{ControlChart$get_params()}}. \cr
#'       \code{type} \tab The control chart's type. \cr
#'     }
#'   }
#'   \item{\code{set_quantile(quantile)}}{Set the quantile associated with this
#'     control line.
#'     \tabular{ll}{
#'       \code{quantile} \tab Numeric value. \cr
#'     }
#'   }
#' }
#'
#' @export
ControlChartControlLine <- R6::R6Class(
  classname = "ControlChartControlLine",
  public = list(
    initialize = function(quantile = 0) {
      private$id <- stringi::stri_rand_strings(1, 8)
      private$quantile <- shiny::reactiveVal(quantile)
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$quantile()
    },

    get_quantile = function() {
      private$quantile()
    },

    set_quantile = function(quantile) {
      private$quantile(quantile)
    }
  ),
  private = list(
    id = character(),
    quantile = NULL
  )
)
