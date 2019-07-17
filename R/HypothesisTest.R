#' Hypothesis Test
#' 
#' \code{\link[R6:R6Class]{R6Class}} representing a hypothesis test consisting
#' of \code{\link{HypothesisTestSample}}s and \code{\link{NullHypothesis}}.
#' 
#' @section Usage:
#' \preformatted{hypothesis_test = HypothesisTest$new()
#' 
#' hypothesis_test$add_sample(HypothesisTestSample$new())
#' 
#' hypothesis_test$add_null_hypothesis(NullHypothesis$new())
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize the hypothesis test.
#'   }
#'   \item{\code{add_null_hypothesis(null_hypothesis)}}{Add a 
#'   \code{\link{NullHypothesis}} to the hypothesis test.
#'     \tabular{ll}{
#'       \code{hypothesis_test} \tab A \code{\link{NullHypothesis}} object.
#'     }
#'   }
#'   \item{\code{add_sample(sample)}}{Add a \code{\link{HypothesisTestSample}} 
#'   to the hypothesis test.
#'     \tabular{ll}{
#'       \code{sample} \tab A \code{\link{HypothesisTestSample}} object.
#'     }
#'   }
#'   \item{\code{get_null_hypothesis(id)}}{Get the hypothesis test's null
#'   hypothesis with \code{id == id}.
#'   }
#'   \item{\code{get_null_hypothesis_ids()}}{Get all null hypothesis ids as a
#'   \code{\link[base:character]{character}} vector.
#'   }
#'   \item{\code{get_sample(id)}}{Get the hypothesis tests's sample with
#'   \code{id == id}.
#'   }
#'   \item{\code{get_sample_ids()}}{Get all sample ids as a 
#'   \code{\link[base:character]{character}} vector.
#'   }
#'   \item{\code{get_id()}}{Get the hypothesis test's id.
#'   }
#'   \item{\code{get_name()}}{Get the hypothesis test's name.
#'   }
#'   \item{\code{remove_null_hypothesis(id)}}{Remove the null hypothesis with
#'   \code{id == id}.
#'   }
#'   \item{\code{remove_sample(id)}}{Remove the sample with \code{id == id}.
#'   }
#'   \item{\code{set_name(name)}}{Set the hypothesis test's name.
#'   }
#' }
#' 
#' @name HypothesisTest
NULL

#' @export
HypothesisTest <- R6::R6Class(
  classname = "HypothesisTest",
  public = list(
    initialize = function() {
      if (!("counter" %in% names(self$static))) {
        self$static$counter <- 1
      } else {
        self$static$counter <- self$static$counter + 1
      }
      private$id <- as.character(self$static$counter)
      
      private$sample_storage <- QWUtils::ObjectStorage$new(
        allowed_classes = "HypothesisTestSample"
      )
      
      private$null_hypothesis_storage <- QWUtils::ObjectStorage$new(
        allowed_classes = "NullHypothesis"
      )
    },
    
    static = new.env(),
    
    add_null_hypothesis = function(null_hypothesis) {
      private$null_hypothesis_storage$add_object(null_hypothesis)
    },
    
    add_sample = function(sample) {
      private$sample_storage$add_object(sample)
    },
    
    get_null_hypothesis = function(id) {
      private$null_hypothesis_storage$get_object(id)
    },
    
    get_null_hypothesis_ids = function() {
      private$null_hypothesis_storage$get_ids()
    },
    
    get_sample = function(id) {
      private$sample_storage$get_object(id)
    },
    
    get_sample_ids = function() {
      private$sample_storage$get_ids()
    },
    
    get_id = function() {
      private$id
    },
    
    get_name = function() {
      private$id
    },
    
    remove_null_hypothesis = function(id) {
      private$null_hypothesis_storage$remove_object(id)
    },
    
    remove_sample = function(id) {
      private$sample_storage$remove_object(id)
    },
    
    set_name = function(name) {
      private$name(name)
    }
  ),
  private = list(
    id = character(),
    null_hypothesis_storage = NULL,
    sample_storage = NULL
  )
)