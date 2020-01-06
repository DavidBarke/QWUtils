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

    get_value = function(params, type = c("xbar_R", "xbar_s", "R", "s", "p")) {
      type <- match.arg(type)
      quantile <- private$quantile()

      if (type %in% c("xbar_s", "s", "p")) {
        value <- switch(
          type,
          "xbar_s" = {
            params$x_bar_bar + quantile * params$s_bar /
              (c4(params$n) * sqrt(params$n))
          },
          "s" = {
            c4_n <- c4(params$n)
            params$s_bar + quantile * params$s_bar * sqrt(1 - c4_n^2) / c4_n
          },
          "p" = {
            params$p_bar + quantile * sqrt(
              params$p_bar * (1 - params$p_bar) / params$n
            )
          }
        )
      } else {
        if (!(quantile %in% c(-3, 0, 3))) {
          stop("ControlChartControlLine: quantile has to be in c(-3, 0, 3)")
        }
        value <- switch(
          type,
          "xbar_R" = {
            quantile <- QWUtils::maprange(quantile, -3, 3, -1, 1)
            params$x_bar_bar + quantile * A2(params$n) * params$R_bar
          },
          "R" = {
            factor <- switch(
              as.character(quantile),
              "-3" = D3(params$n),
              "0" = 1,
              "3" = D4(params$n)
            )
            factor * params$R_bar
          }
        )
      }

      value
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
