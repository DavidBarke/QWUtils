#' @rdname single_rename
#'
#' @export
single_rename_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("module_ui"),
    inline = TRUE
  )
}

#' Handle the rename of a name
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @inheritParams checked_text_input
#' @param .name \code{\link[shiny:reactive]{Reactive}} returning the name to rename.
#' @param .rename \code{\link[base:function]{Function}}, which is called when the
#' user confirms the rename.
#' @param .rename_tooltip Tooltip for the button, which enables the
#' rename mode.
#'
#' @name single_rename
#'
#' @export
single_rename <- function(
  input, output, session, .data, .values, .parent, .name, .rename,
  .error_controller = NULL, .rename_tooltip = NULL
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("single_rename"), .parent, session)

  rvs <- shiny::reactiveValues(
    mode_rename = FALSE
  )

  output$module_ui <- shiny::renderUI({
    if (rvs$mode_rename) {
      ui <- htmltools::tagList(
        QWUtils::checked_text_input_ui(
          id = ns("id_checked_text_input")
        ),
        shiny::uiOutput(
          outputId = ns("confirm_rename"),
          inline = TRUE
        )
      )
    } else {
      ui <- htmltools::tagList(
        .name(),
        QWUtils::actionButtonQW(
          inputId = ns("rename"),
          label = NULL,
          icon = shiny::icon("edit"),
          tooltip = QWUtils::handle_fun(.rename_tooltip)
        )
      )
    }

    ui
  })

  output$confirm_rename <- shiny::renderUI({
    if (!checked_text_input_return$error()) {
      QWUtils::actionButtonQW(
        inputId = ns("confirm_rename"),
        label = NULL,
        icon = shiny::icon("check"),
        tooltip = QWUtils::label_lang(
          de = "Neuen Namen bestätigen",
          en = "Confirm new name"
        )
      )
    }
  })

  shiny::observeEvent(input$rename, {
    rvs$mode_rename <- TRUE
  })

  shiny::observeEvent(input$confirm_rename, {
    rvs$mode_rename <- FALSE
    .rename(checked_text_input_return$name())
  })

  checked_text_input_return <- shiny::callModule(
    module = QWUtils::checked_text_input,
    id = "id_checked_text_input",
    .data = .data,
    .values = .values,
    .parent = self,
    .value = .name,
    .error_controller = .error_controller,
    .allow_reset = FALSE,
    .inline_block = TRUE
  )
}
