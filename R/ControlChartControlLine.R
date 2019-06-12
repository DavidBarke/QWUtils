#' ControlChartControlLine
#'
#' @export
ControlChartControlLine <- R6::R6Class(
  classname = "ControlChartControlLine",
  public = list(
    initialize = function(quantile = 0) {
      if (!("counter" %in% names(self$static))) {
        self$static$counter <- 1
      } else {
        self$static$counter <- self$static$counter + 1
      }

      private$id <- as.character(self$static$counter)
      private$name <- private$id
      private$quantile <- shiny::reactiveVal(quantile)
    },

    static = new.env(),

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$id
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

    set_name = function(name) {
      private$name(name)
    },

    set_quantile = function(quantile) {
      private$quantile(quantile)
    }
  ),
  private = list(
    id = character(),
    name = NULL,
    quantile = NULL
  )
)
