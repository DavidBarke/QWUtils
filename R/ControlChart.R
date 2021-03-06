#' ControlChart
#'
#' R6Class representing a control chart consisting of \code{\link{ControlChartPhase}}s
#' and \code{\link{ControlChartControlLine}}s.
#'
#' @section Usage:
#' \preformatted{control_chart = ControlChart$new(name, type)
#'
#' control_chart$add_phase(ControlChartPhase$new(name))
#'
#' control_chart$get_table()
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name, type = c("xbar_R", "xbar_s", "R", "s", "p"))}}{
#'   Initialize the control chart.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. Name of
#'       the control chart. \cr
#'       \code{type} \tab \code{\link[base:character]{Character}}. Type of
#'       the control chart.
#'     }
#'   }
#'   \item{\code{add_control_line(control_line)}}{Add an object of class
#'     \code{\link{ControlChartControlLine}} to the control chart.
#'   }
#'   \item{\code{add_phase(phase)}}{Add an object of class
#'     \code{link{ControlChartPhase}} to the control chart.
#'   }
#'   \item{\code{get_control_line(id = id)}}{Get the control chart's control line
#'   with \code{id == id}.
#'   }
#'   \item{\code{get_control_line_ids()}}{Get all control line ids as a character
#'   vector.
#'   }
#'   \item{\code{get_control_line_values()}}{Get all control line values as a
#'   numeric vector.
#'   }
#'   \item{\code{get_id()}}{Get the control chart's id.
#'   }
#'   \item{\code{get_name()}}{Get the control chart's name.
#'   }
#'   \item{\code{get_params()}}{Get the parameters of the control charts
#'   preliminary phases values used to calculate the control line values.
#'   }
#'   \item{\code{get_phase(id)}}{Get the control chart's phase with
#'   \code{id == id}.
#'   }
#'   \item{\code{get_phase_ids(preliminary = NULL)}}{Get all phases ids as a
#'   character vector.
#'     \tabular{ll}{
#'       \code{preliminary} \tab If \code{\link[base:NULL]{NULL}} all ids,
#'         if \code{\link[base:logical]{TRUE}} only ids of preliminary phases,
#'         if \code{\link[base:logical]{FALSE}} only ids of non-preliminary
#'         phases are returned.
#'     }
#'   }
#'   \item{\code{get_plot()}}{Get the control chart's plot as a
#'   \code{\link[plotly:plot_ly]{plotly}} plot.
#'   }
#'   \item{\code{get_table(sample_count = c("chart", "phase"),
#'   preliminary = NULL)}}{Get a \code{\link[tibble:tibble]{tibble}} with
#'   columns \code{"phase"}, \code{"sample"}, \code{"value"}.
#'     \tabular{ll}{
#'       \code{sample_count} \tab If \code{"chart"} samples are counted from
#'       the perspective of the chart, if \code{"phase"} from the perspective
#'       of each phase. \cr
#'       \code{preliminary} \tab If \code{\link[base:NULL]{NULL}} all rows,
#'         if \code{\link[base:logical]{TRUE}} only rows of preliminary phases,
#'         if \code{\link[base:logical]{FALSE}} only rows of non-preliminary
#'         phases are returned.
#'     }
#'   }
#'   \item{\code{get_type()}}{Get the control chart's type.
#'   }
#'   \item{remove_phase(id)}{Remove the phase with \code{id == id}.
#'   }
#'   \item{\code{set_name(name)}}{Set the control chart's name.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. New name.
#'     }
#'   }
#'   \item{\code{set_phase_names(new_names, old_names = NULL)}}{Set the names
#'   of one or more control chart's phases. If \code{old_names = NULL} the
#'   length of \code{new_names} must be equal to the number of the control
#'   chart's phases, otherwise the length of \code{new_names} and \code{old_names}
#'   has to be equal.
#'     \tabular{ll}{
#'       \code{new_names} \tab \code{\link[base:character]{Character}}
#'       vector. \cr
#'       \code{old_names} \tab \code{\link[base:character]{Character}}
#'       vector or \code{\link[base:NULL]{NULL}}.
#'     }
#'   }
#'   \item{\code{set_type(type = c("xbar_R", "xbar_s", "R", "s", "p"))}}{Set
#'   the control chart's type.
#'     \tabular{ll}{
#'       \code{type} \tab \code{\link[base:character]{Character}}. New type.
#'     }
#'   }
#' }
#'
#' @name ControlChart
NULL

#' @export
ControlChart <- R6::R6Class(
  classname = "ControlChart",
  public = list(
    initialize = function(
      name, type = c("xbar_R", "xbar_s", "s", "R", "p")
    ) {
      # values is a named list. The name of one element is the phase's id, the
      # values of one element are the phase's values
      type <- match.arg(type)

      private$id <- stringi::stri_rand_strings(1, 8)

      private$name <- shiny::reactiveVal(name)

      private$type <- shiny::reactiveVal(type)

      private$control_line_storage <- ObjectStorage$new(
        allowed_class = "ControlChartControlLine"
      )

      # Always fill the control_line_storage with the center line and lower and
      # upper control lines in distance of three quantile
      private$control_line_storage$add_object(
        ControlChartControlLine$new(
          quantile = 0
        )
      )$add_object(
        ControlChartControlLine$new(
          quantile = -3
        )
      )$add_object(
        ControlChartControlLine$new(
          quantile = 3
        )
      )

      private$phase_storage <- ObjectStorage$new(
        allowed_classes = "ControlChartPhase"
      )

      # Option for the future
      # private$positioner <- Positioner$new(
      #   .length = reactive({length(get_phase_ids())})
      # )
    },

    add_control_line = function(control_line) {
      private$control_line_storage$add_object(control_line)

      invisible(self)
    },

    add_phase = function(phase) {
      private$phase_storage$add_object(phase)

      invisible(self)
    },

    get_control_line = function(id) {
      private$control_line_storage$get_object(id)
    },

    get_control_line_ids = function() {
      private$control_line_storage$get_ids()
    },

    get_control_line_values = function() {
      purrr::map_dbl(
        self$get_control_line_ids(),
        function(id) {
          control_line = private$control_line_storage$get_object(id)

          control_line_value(
            data = self$get_table(),
            quantile = control_line$get_quantile(),
            type = private$type()
          )
        }
      )
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$name()
    },

    get_phase = function(id) {
      private$phase_storage$get_object(id)
    },

    get_phase_ids = function(preliminary = NULL) {
      phase_ids <- private$phase_storage$get_ids()

      if (purrr::is_null(preliminary)) {
        phase_ids
      } else {
        is_preliminary = purrr::map_lgl(phase_ids, function(id) {
          self$get_phase(id)$is_preliminary()
        })
        return(phase_ids[is_preliminary == preliminary])
      }
    },

    get_plot = function() {
      table <- self$get_summarised_table()

      p <- plotly::plot_ly(data = table, x = ~sample, y = ~value, type = "scatter", mode = "lines+markers")

      control_lines <- self$get_control_line_values()

      for (control_line in control_lines) {
        p <- add_trace(
          p = p,
          x = range(table$sample),
          y = control_line,
          type = "scatter",
          mode = "lines",
          showlegend = FALSE,
          inherit = FALSE
        )
      }

      p
    },

    # Really needed?
    get_summarised_table = function(preliminary = NULL) {
      table <- self$get_table(preliminary)
      grouped <- table %>% group_by(sample)
      switch(
        self$get_type(),
        "xbar_s" = {
          summarised_table <- grouped %>%
            summarise(value = mean(value))
        },
        "s" = {
          summarised_table <- grouped %>%
            summarise(value = sd(value))
        },
        "p" = {
          summarised_table <- grouped
        },
        "xbar_R" = {
          summarised_table <- grouped %>%
            summarise(value = mean(value))
        },
        "R" = {
          R <- function(x) diff(range(x))
          summarised_table <- grouped %>%
            summarise(value = R(value))
        }
      )
    },

    # Get table with columns: phase, sample, value
    get_table = function(sample_count = c("chart", "phase"), preliminary = NULL) {
      sample_count <- match.arg(sample_count)

      phase_ids <- self$get_phase_ids(preliminary)

      if (length(phase_ids) > 0) {
        df <- map2_dfr(phase_ids, names(phase_ids), function(id, name) {
          df <- self$get_phase(id)$get_value()
          if (nrow(df) > 0) {
            df$phase <- name
          } else {
            df$phase <- character()
          }
          df
        })

        df <- dplyr::select(df, phase, sample, value)

        # Count the sample number in perspective of the chart, not the phase
        if (sample_count == "chart") {
            times <- table(df$phase, df$sample)
            df$sample <- rep(seq_along(times), times)
        }

      } else {
        df <- tibble(
          phase = character(),
          sample = numeric(),
          value = numeric()
        )
      }

      df
    },

    get_type = function() {
      private$type()
    },

    remove_phase = function(id) {
      private$phase_storage$remove_object(id)
    },

    set_name = function(name) {
      private$name(name)
      invisible(self)
    },

    set_phase_names = function(new_names, old_names = NULL) {
      if (is.null(old_names)) {
        phases <- private$phase_storage
      } else {
        phases <- private$phase_storage$get_object(old_names)
      }

      walk2(phases, new_names, function(phase, name) {
        phase$set_name(name)
      })

      invisible(self)
    },

    set_type = function(type = c("xbar_R", "xbar_s", "R", "s", "p")) {
      type <- match.arg(type)

      private$type(type)

      invisible(self)
    }
  ),
  private = list(
    control_line_storage = NULL,
    id = character(),
    name = NULL,
    phase_storage = NULL,
    type = NULL
    # positioner = NULL
  )
)
