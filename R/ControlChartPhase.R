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
#'   \item{\code{add_sample(sample)}}{Add a \code{\link{ControlChartSample}} or
#'   ControlChartSampleList to the phase.
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
      private$id <- stringi::stri_rand_strings(1, 8)

      private$name <- shiny::reactiveVal(name)
      private$preliminary <- shiny::reactiveVal(preliminary)
      private$sample_storage <- ObjectStorage$new(
        allowed_classes = c("ControlChartSample", "ControlChartSampleList")
      )
    },

    add_sample = function(sample) {
      private$sample_storage$add_object(sample)
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

      nested_samples <- purrr::map(sample_ids, function(id) {
        private$sample_storage$get_object(id)$get_value()
      })

      # environment .envir has to have an variable with name index which helps
      # generating increasing indices for the samples
      sample_object_to_df <- function(sample_object, .envir) {
        if (purrr::is_list(sample_object)) {
          # Process lists of samples
          purrr::map_dfr(sample_object, sample_object_to_df, .envir)
        } else {
          # At this point we only have vectors representing values of one sample
          df <- tibble::tibble(
            sample = .envir$index,
            value = sample_object
          )
          .envir$index <- .envir$index + 1
          df
        }
      }

      .envir <- new.env()
      .envir$index <- 1
      samples <- sample_object_to_df(nested_samples, .envir)

      if (length(samples) == 0) {
        table <- tibble::tibble(
          sample = numeric(),
          value = numeric()
        )
      } else {
        table <- samples
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
