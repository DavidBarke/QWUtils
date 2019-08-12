#' @export
TTest <- R6::R6Class(
  classname = "TTest",
  inherit = QWUtils::Test,
  public = list(
    initialize = function() {
      super$initialize()

      sample_size_error_factory <- function(name = "") {
        ErrorController$new(
          fun = function(x) {
            print("SAMPLE")
            print(x)
            length(x) < 2
          },
          ui_true = QWUtils::label_lang(
            de = paste(name, "muss mindestens zwei Elemente enthalten."),
            en = paste(name, "must contain at least two elements.")
          ),
          error = TRUE
        )
      }

      private$error_controller_list_list <- list(
        x = ErrorControllerList$new()$add_error_controller(
          sample_size_error_factory(
            name = QWUtils::label_lang(
              de = "Erste Stichprobe",
              en = "First sample"
            )
          )
        ),

        y = ErrorControllerList$new()$add_error_controller(
          sample_size_error_factory(
            name = QWUtils::label_lang(
              de = "Zweite Stichprobe",
              en = "Second sample"
            )
          )
        ),

        alternative = ErrorControllerList$new(),

        mu = ErrorControllerList$new(),

        paired = ErrorControllerList$new(),

        var.equal = ErrorControllerList$new(),

        conf.level = ErrorControllerList$new()
      )
    },

    get_output = function() {
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

    get_datatable_output = function() {
      test_output <- self$get_output()

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

    set = function(
      what = c("x", "y", "alternative", "mu", "paired", "var.equal", "conf.level"),
      value
    ) {
      what <- match.arg(what)

      private[[what]] <- QWUtils::reactive_member(value)

      private$error_controller_list_list[[what]]$set_value(private[[what]])
    }
  ),
  private = list(
    alternative = NULL,
    conf.level = NULL,
    mu = NULL,
    paired = NULL,
    var.equal = NULL,
    x = NULL,
    y = NULL
  )
)
