#' @export
multiple_observed_text_inputs_ui <- function(id) {
  ns <- NS(id)

  uiOutput(
    outputId = ns("module_ui")
  )
}

#' @export
multiple_observed_text_inputs <- function(
  input, output, session, .data, .values, .parent, initial_values,
  allow_reset = FALSE, .reset = NULL, .update_values = NULL
) {

  ns <- session$ns

  self <- Node$new(ns("multiple_observed_text_inputs"), .parent, session)

  rvs <- reactiveValues(
    count = 0,
    observed_text_input_return_list = list(),
    reset_all = 0,
    update_values = 0
  )

  output$module_ui <- renderUI({
    ui <- map2(
      handle_fun(initial_values), seq_along(handle_fun(initial_values)),
      function(name, index) {
        if (index > rvs$count) {
          rvs$count <- index
          rvs$observed_text_input_return_list[[index]] <- callModule(
            module = QWUtils::checked_text_input,
            id = "id_observed_text_input" %_% index,
            .data = .data,
            .values = .values,
            .parent = self,
            .reset = if (allow_reset) reactive({rvs$reset_all}) else NULL,
            .update_value = update_values,
            .label = NULL,
            .value = name
          )
        }
        QWUtils::checked_text_input_ui(
          id = ns("id_observed_text_input" %_% index)
        )
      }
    )
    ui <- tagList(
      div(
        style = "margin: 0px 0px 5px 0px",
        actionButtonQW(
          inputId = ns("reset_all"),
          label = NULL,
          icon = icon("arrow-left"),
          tooltip = label_lang(
            de = "Alle zurücksetzen",
            en = "Reset all"
          )
        )
      ),
      ui
    )
  })

  observeEvent(input$reset_all, {
    rvs$reset_all <- rvs$reset_all + 1
  })

  if (!is.null(.reset)) {
    observeEvent(.reset(), {
      rvs$reset_all <- rvs$reset_all + 1
    })
  }

  if (!is.null(.update_values)) {
    observeEvent(.update_values(), {
      rvs$update_values <- rvs$update_values + 1
    })
  }

  update_values <- reactive({rvs$update_values})

  length_initial_values <- reactive({
    length(handle_fun(initial_values))
  })

  error <- reactive({
    req(
      length(rvs$observed_text_input_return_list) >=
        length_initial_values()
    )

    name_error <- map_lgl(
      seq_len(length_initial_values()),
      function(index) {
        rvs$observed_text_input_return_list[[index]]$error()
      }
    )
    any(name_error)
  })

  new_names <- reactive({
    map_chr(seq_len(length_initial_values()), function(index) {
      rvs$observed_text_input_return_list[[index]]$name()
    })
  })

  return_list <- list(
    error = error,
    new_names = new_names
  )

  return(return_list)
}
