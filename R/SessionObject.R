#' @export
SessionObject <- R6::R6Class(
  classname = "SessionObject",
  inherit = QWUtils::Object,
  public = list(
    initialize = function(name = "", session = shiny::getDefaultReactiveDomain()) {
      super$initialize(name)

      private$session <- session
    },

    get_session = function() {
      private$session
    }
  ),
  private = list(
    session = NULL
  )
)
