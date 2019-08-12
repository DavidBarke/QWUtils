#' @export
Test <- R6::R6Class(
  classname = "Test",
  inherit = QWUtils::Object,
  public = list(
    initialize = function() {
      super$initialize()
    },

    get_output = function() {
      "Test Output"
    },

    get_datatable_output = function() {
      DT::datatable(data.frame(`Test Output` = c("Empty")))
    },

    get_error_controller_list_list = function() {
      private$error_controller_list_list
    }
  ),
  private = list(
    error_controller_list_list = list()
  )
)
