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
#'   \item{\code{get_null_hypothesis_storage()}{Get the hypothesis test's
#'     null hypothesis storage, which is an \code{\link{ObjectStorage}} object.
#'   }
#'   \item{\code{get_sample_storage()}{Get the hypothesis test's sample storage,
#'     which is an \code{\link{ObjectStorage}} object.
#'   }
#'   \item{\code{get_id()}}{Get the hypothesis test's id.
#'   }
#'   \item{\code{get_name()}}{Get the hypothesis test's name.
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
  inherit = QWUtils::Object,
  public = list(
    initialize = function() {
      super$initialize()

      private$sample_storage <- QWUtils::ObjectStorage$new(
        allowed_classes = "HypothesisTestSample"
      )

      private$null_hypothesis_storage <- QWUtils::ObjectStorage$new(
        allowed_classes = "NullHypothesis"
      )
    },

    static = new.env(),

    get_null_hypothesis_storage = function() {
      private$null_hypothesis_storage
    },

    get_sample_storage = function() {
      private$sample_storage
    },

    get_test_datatable = function() {
      test_output <- self$get_test_output()

      test_df <- tibble(
        param = QWUtils::label_lang(
          de = c("Teststatistik", "Freiheitsgrade", "p-Wert", "Konfidenzintervall"),
          en = c("Test statistic", "Degrees of freedom", "p value", "Confidence interval")
        ),
        value = c(
          test_output$statistic, test_output$parameter, test_output$p.value,
          paste0(
            "[",
            paste(format(test_output$conf.int, digits = 8), collapse = ", "),
            "]"
          )
        )
      )

      names(test_df) <- QWUtils::label_lang(
        de = c("Parameter", "Wert"),
        en = c("Parameter", "Value")
      )

      DT::datatable(test_df)
    },

    get_test_output = function() {
      t.test(
        x = QWUtils::handle_fun(private$x),
        y = QWUtils::handle_fun(private$y),
        alternative = QWUtils::handle_fun(private$alternative),
        mu = QWUtils::handle_fun(private$mu),
        paired = QWUtils::handle_fun(private$paired),
        var.equal = QWUtils::handle_fun(private$var.equal),
        conf.level = QWUtils::handle_fun(private$conf.level)
      )
    },

    get_test_related = function(
      what = c("x", "y", "alternative", "mu", "paired", "var.equal", "conf.level")
    ) {
      what <- match.arg(what)

      private[[what]]
    },

    set_test_related = function(
      what = c("x", "y", "alternative", "mu", "paired", "var.equal", "conf.level"),
      value
    ) {
      what <- match.arg(what)

      private[[what]] <- QWUtils::reactive_member(value)
    }
  ),
  private = list(
    alternative = NULL,
    conf.level = NULL,
    id = character(),
    mu = NULL,
    paired = NULL,
    null_hypothesis_storage = NULL,
    sample_storage = NULL,
    var.equal = NULL,
    x = NULL,
    y = NULL
  )
)
