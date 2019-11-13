#' Checked text input
#'
#' @description
#' textInput whose value is checked for being useable as an object name
#'
#' @param input,output,session Called by \code{\link[shiny]{callModule}}.
#' @param .values The \code{.values} list.
#' @param .parent The parent \code{\link{Node}} object.
#' @param .label The contents of the button or link-usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param .value Function without an argument (e.g
#'   \code{\link[shiny:reactive]{reactive}}) or vector containing the initial
#'   value of the text input.
#' @param .allow_reset If \code{\link[base:logical]{TRUE}}, reset to the initial
#'   value via an \code{\link[shiny]{actionButton}} is enabled.
#' @param .reset \code{\link[shiny:reactive]{Reactive}}. See 'Details'.
#' @param .update_value \code{\link[shiny:reactive]{Reactive}}. See 'Details'.
#' @param .error_controller An \code{\link{ErrorController}} or
#'   \code{\link{ErrorControllerList}}
#'
#' @details
#' When the value of the reactive passed to \code{.reset} changes, the textInput
#' is reset to the last updated value. When the value of the reactive passed to
#' \code{.update_value} changes the current value of the text input gets the
#' last updated value.
#'
#' @return
#'
#' \code{checked_text_input} returns a list of \code{\link[shiny:reactive]{reactives}}:
#'
#' \tabular{ll}{
#'   \code{name} \tab Current value of the text input. \cr
#'   \code{null_name} \tab If an error occured \code{\link[base:NULL]{NULL}}, else
#'   the current value of the text input. \cr
#'   \code{error} \tab If an error occured \code{\link[base:logical]{TRUE}},
#'   else \code{\link[base:logical]{FALSE}}.
#' }
#'
#' @export
checked_text_input <- function(
  input, output, session, .values, .parent, .label = NULL, .value = "",
  .allow_reset = TRUE, .reset = NULL, .update_value = NULL,
  .error_controller = NULL, .inline_block = FALSE
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("name_text_input"), .parent, session)

  rvs <- shiny::reactiveValues(
    # The value is retrieved from the ui the first time the input renders
    value = NULL
  )

  output$text_input <- shiny::renderUI({
    label <- QWUtils::handle_fun(.label)

    if (.allow_reset) {
      label <- div(
        label,
        QWUtils::actionButtonQW(
          inputId = ns("reset"),
          label = NULL,
          icon = icon("arrow-left"),
          tooltip = QWUtils::label_lang(
            de = "Zurücksetzen",
            en = "Reset"
          )
        )
      )
    }

    if (.inline_block) {
      style <- "display: inline-block"
    } else {
      style <- ""
    }

    htmltools::div(
      style = style,
      shiny::textInput(
        inputId = ns("name_text"),
        label = label,
        value = QWUtils::handle_fun(.value)
      )
    )
  })

  output$name_error <- shiny::renderUI({
    if (name_is_empty()) {
      name_is_empty <- QWUtils::label_lang(
        de = "Name muss mindestens aus einem Zeichen bestehen",
        en = "Name has to consist of at least one symbol"
      )
    } else {
      name_is_empty <- NULL
    }

    if (invalid_character()) {
      invalid_character <- QWUtils::label_lang(
        de = "Name darf nur A-Z, a-z, 0-9, ' ' und '-' enthalten.",
        en = "Name has to consist of A-Z, a-z, 0-9, ' ' and '-'"
      )
    } else {
      invalid_character <- NULL
    }

    tagList(
      name_is_empty,
      invalid_character
    )
  })

  output$error_controller <- shiny::renderUI({
    if (purrr::is_null(.error_controller)) {
      ui <- NULL
    } else {
      .error_controller$set_value(input$name_text)
      ui <- .error_controller$get_ui()
    }

    ui
  })

  error <- shiny::reactive({
    name_is_empty() || invalid_character() || error_controller_error()
  })

  name_is_empty <- shiny::reactive({
    if (purrr::is_null(input$name_text)) {
      return(FALSE)
    } else {
      return(input$name_text == "")
    }
  })

  # Name is only null if the module is invoked but the ui not shown yet. Thus
  # it is not considered to be an error.
  name_is_null <- shiny::reactive({
    purrr::is_null(input$name_text)
  })

  invalid_character <- shiny::reactive({
    if (!name_is_null()) {
      !all(
        stringr::str_split(input$name_text, "")[[1]] %in%
          .values$data$valid_name_characters
      )
    } else {
      FALSE
    }
  })

  error_controller_error <- shiny::reactive({
    if (purrr::is_null(.error_controller)) {
      error <- FALSE
    } else {
      error <- .error_controller$has_error()
    }

    error
  })

  shiny::observeEvent(input$reset, {
    updateTextInput(
      session = session,
      inputId = "name_text",
      value = QWUtils::handle_fun(.value)
    )
  })

  if (!purrr::is_null(.reset)) {
    shiny::observeEvent(.reset(), {
      shiny::updateTextInput(
        session = session,
        inputId = "name_text",
        value = rvs$value
      )
    })
  }

  name <- shiny::reactive({
    # If UI hasn't been rendered once, return the initial value
    fallback(input$name_text, QWUtils::handle_fun(.value))
  })

  null_name <- shiny::reactive({
    if (error()) {
      return(NULL)
    } else {
      return(name())
    }
  })

  return_list <- list(
    name = name,
    null_name = null_name,
    error = error
  )

  return(return_list)
}

#' @param id The module's id.
#'
#' @rdname checked_text_input
#'
#' @export
checked_text_input_ui <- function(
  id
) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("text_input"),
      inline = TRUE
    ),
    shiny::uiOutput(
      outputId = ns("name_error"),
      inline = TRUE,
      class = "error-msg"
    ),
    shiny::uiOutput(
      outputId = ns("error_controller"),
      inline = TRUE,
      class = "error-msg"
    )
  )
}
