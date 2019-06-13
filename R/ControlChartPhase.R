#' ControlChartPhase
#'
#' R6Class representing a control chart phase consisting of multiple
#' \code{\link[ControlChartSample]{ControlChartSamples}}. Multiple phases may
#' form a \code{\link{ControlChart}}.
#'
#' @section Usage:
#' \preformatted{phase = ControlChartPhase$new(name, preliminary = FALSE)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name, preliminary = FALSE)}}{Initialize the phase.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. The name
#'       of the phase. \cr
#'       \code{preliminary} \tab If \code{\link[base:logical]{TRUE}}, the
#'       phase is used to calculate the control line values of the parent
#'       control chart, otherwise not.
#'     }
#'   }
#'   \item{\code{add_sample(sample)}}{Add a \code{\link{ControlChartSample}} to
#'   the phase.
#'     \tabular{ll}{
#'       \code{sample} \tab A \code{\link{ControlChartSample}}.
#'     }
#'   }
#'   \item{\code{get_id()}}{Get the phase's id.
#'   }
#'   \item{\code{get_name()}}{Get the phase's name.
#'   }
#'   \item{\code{get_sample(id)}}{Get the phase's sample with \code{id == id}.
#'   }
#'   \item{\code{get_sample_ids()}}{Get all sample ids as a character vector.
#'   }
#'   \item{\code{get_value()}}{Get a \code{\link[tibble:tibble]{tibble}} with
#'   columns \code{"sample"} and \code{"value"}.
#'   }
#'   \item{\code{is_preliminary()}}{Returns a \code{\link[base:logical]{logical}}
#'   indicating whether the phase is preliminary or not.
#'   }
#'   \item{\code{remove_sample(id)}}{Remove the sample with \code{id == id}.
#'   }
#'   \item{\code{set_name()}}{Set the phase's name.
#'   }
#'   \item{\code{set_preliminary(value)}}{Set whether the phase is preliminary
#'   or not.
#'     \tabular{ll}{
#'       \code{value} \tab A \code{\link[base:logical]{logical}}.
#'     }
#'   }
#' }
#'
#' @name ControlChartPhase
NULL

#' @export
ControlChartPhase <- R6::R6Class(
  classname = "ControlChartPhase",
  public = list(
    initialize = function(name, preliminary = FALSE) {
      if (!("counter" %in% names(self$static))) {
        self$static$counter <- 1
      } else {
        self$static$counter <- self$static$counter + 1
      }
      private$id <- as.character(self$static$counter)

      private$name <- shiny::reactiveVal(name)
      private$preliminary <- shiny::reactiveVal(preliminary)
      private$sample_storage <- QWUtils::ObjectStorage$new(
        allowed_classes = "ControlChartSample"
      )
    },

    static = new.env(),

    add_sample = function(sample) {
      private$sample_storage$add_object(sample)
      sample$set_name(paste0(
        QWUtils::label_lang(
          de = "Stichprobe ",
          en = "Sample"
        ),
        private$sample_storage$get_length()
      ))
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$name()
    },

    get_sample = function(id) {
      private$sample_storage$get_object(id)
    },

    get_sample_ids = function() {
      private$sample_storage$get_ids()
    },

    get_value = function() {
      sample_ids <- private$sample_storage$get_ids()
      table <- purrr::map2_dfr(sample_ids, seq_along(sample_ids), function(id, index) {
        value <- private$sample_storage$get_object(id)$get_value()
        tibble::tibble(
          sample = index,
          value = private$sample_storage$get_object(id)$get_value()
        )
      })

      if (nrow(table) == 0) {
        table <- tibble::tibble(
          sample = numeric(),
          value = numeric()
        )
      }

      table
    },

    is_preliminary = function() {
      private$preliminary()
    },

    remove_sample = function(id) {
      private$sample_storage$remove_object(id)
    },

    set_name = function(name) {
      private$name(name)
    },

    set_preliminary = function(value) {
      private$preliminary(value)
    }
  ),
  private = list(
    id = character(),
    name = NULL,
    preliminary = NULL,
    sample_storage = NULL
  )
)