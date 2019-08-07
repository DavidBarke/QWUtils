#' Observed vector input
#'
#' @inheritParams checked_text_input
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @param .error_controller_text \code{\link{ErrorController}} or
#'   \code{\link{ErrorControllerList}} for controlling the text of the
#'   \code{\link[shiny:textInput]{textInput}}.
#' @param .error_controller_value \code{\link{ErrorController}} or
#'   \code{\link{ErrorControllerList}} for controlling the module's value
#'   (\code{\link[base:numeric]{numeric}} vector).
#'
#' @export
observed_vector_input <- function(
  input, output, session, .values, .parent, .label = NULL,
  .value = "", .error_controller_text = NULL,
  .error_controller_value = NULL
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("observed_vector_input"), .parent, session)

  output$text_input <- shiny::renderUI({

    shiny::textInput(
      inputId = ns("vector_text"),
      label = QWUtils::handle_fun(.label),
      value = QWUtils::handle_fun(.value)
    )
  })

  output$wrong_char <- shiny::renderUI({
    if (wrong_char()) {
      ui <- tagList(
        QWUtils::label_lang(
          de = paste0(
            "Erlaubte Zeichen sind Ziffern, ',', '.' und ' '. Das letzte",
            "Zeichen muss eine Ziffer sein."
          ),
          en = paste0(
            "Allowed signs are digits, ',', '.', and ' '. The last sign has to",
            "be a digit."
          )
        )
      )
      return(ui)
    }
  })

  output$error_controller_text <- shiny::renderUI({
    if (purrr::is_null(.error_controller_text)) {
      ui <- NULL
    } else {
      .error_controller_text$set_value(input$vector_text)
      ui <- .error_controller_text$get_ui()
    }

    ui
  })

  output$error_controller_value <- shiny::renderUI({
    if (purrr::is_null(.error_controller_value)) {
      ui <- NULL
    } else {
      # value gets checked only if text is alright
      if (!text_error()) {
        .error_controller_value$set_value(vector_text_to_numeric(input$vector_text))
      }
      ui <- .error_controller_value$get_ui()
    }

    ui
  })

  # Only switch to non-editable mode if the user input was valid
  wrong_char <- shiny::reactive({
    if (purrr::is_null(input$vector_text)) {
      return(FALSE)
    }

    if (!stringr::str_detect(
      input$vector_text,
      # Matches positive and negative numbers separated by comma and an
      # arbitrary number of whitespaces
      "^([-]?[0-9]+[\\.]?[0-9]*[,]{1}[\\s]+)*([-]?[0-9]+[\\.]?[0-9]*[\\s]*){1}$"
    )) {
      wrong_char <- TRUE
    } else {
      wrong_char <- FALSE
    }

    wrong_char
  })

  x <- shiny::reactive({
    if (purrr::is_null(input$vector_text)) {
      vector_text_to_numeric(QWUtils::handle_fun(initial_value))
    } else {
      vector_text_to_numeric(input$vector_text)
    }
  })

  error_controller_text_error <- shiny::reactive({
    if (purrr::is_null(.error_controller_text)) {
      return(FALSE)
    } else {
      return(.error_controller_text$has_error())
    }
  })

  error_controller_value_error <- shiny::reactive({
    if (purrr::is_null(.error_controller_value)) {
      return(FALSE)
    } else {
      return(.error_controller_value$has_error())
    }
  })

  text_error <- shiny::reactive({
    wrong_char() || error_controller_text_error()
  })

  error <- shiny::reactive({
    text_error() || error_controller_value_error()
  })

  return_list <- list(
    x = x,
    error = error
  )

  return(return_list)
}

#' @rdname observed_vector_input
#'
#' @export
observed_vector_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::tagList(
      shiny::uiOutput(
        outputId = ns("text_input")
      ),
      shiny::uiOutput(
        outputId = ns("wrong_char")
      ),
      shiny::uiOutput(
        outputId = ns("error_controller_text")
      ),
      shiny::uiOutput(
        outputId = ns("error_controller_value")
      )
    )
  )
}
