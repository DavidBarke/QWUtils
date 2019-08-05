#' @rdname data_selector_group_dropdown
#'
#' @param id The module's id.
#'
#' @export
data_selector_group_dropdown_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: inline",
    dropdown(
      inputId = ns("group_dropdown"),
      icon = icon("sliders-h"),
      style = "material-flat",
      size = "xs",
      up = TRUE,
      actionButtonQW(
        inputId = ns("information"),
        label = "Information",
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("change_group_name"),
        label = label_lang(
          de = "Umbenennen",
          en = "Rename"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("copy_group"),
        label = label_lang(
          de = "Kopieren",
          en = "Copy"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("remove_group"),
        label = label_lang(
          de = "Entfernen",
          en = "Remove"
        ),
        dropdown = TRUE
      )
    )
  )
}

#' Dropdown for group actions
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @inheritParams observed_vector_input
#' @param group_name \code{\link[shiny:reactive]{Reactive}}
#'   containing the group selected by the user.
#'
#' @export
data_selector_group_dropdown <- function(
  input, output, session, .values, .parent, group_name
) {

  ns <- session$ns

  self <- Node$new(ns("data_selector_group_dropdown"), .parent, session)

  rvs <- reactiveValues(
    new_group_name = NULL
  )

  observeEvent(input$information, {
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = paste0("Gruppe: ", group_name()),
          en = paste0("Group: ", group_name())
        ),
        value = group_name(),
        textOutput(
          outputId = ns(group_name() %_% "group_information")
        )
      )
    )

    if (!(group_name() %_% "group_information") %in% names(output)) {
      group_name <- group_name()
      output[[group_name %_% "group_information"]] <- renderText({
        n_datasets <- length(.values$data$get_dataset_names(group_name))

        label_lang(
          de = paste0("Anzahl Datensaetze: ", n_datasets),
          en = paste0("Number of datasets: ", n_datasets)
        )
      })
    }
  })

  observeEvent(input$change_group_name, {
    showModal(modalDialog(
      easyClose = TRUE,
      textInput(
        inputId = ns("new_group_name"),
        label = label_lang(
          de = paste0("Neuer Gruppenname ", group_name()),
          en = paste0("New group name ", group_name())
        )
      ),
      footer = uiOutput(
        outputId = ns("modal_footer_name")
      )
    ))
  })

  output$modal_footer_name <- renderUI({
    req(input$new_group_name)
    tagList(
      actionButtonQW(
        inputId = ns("confirm_new_group_name"),
        label = label_lang(
          de = "Bestätigen",
          en = "Confirm"
        )
      ),
      actionButtonQW(
        inputId = ns("dismiss_new_group_name"),
        label = label_lang(
          de = "Abbrechen",
          en = "Dismiss"
        )
      )
    )
  })

  observeEvent(input$confirm_new_group_name, {
    .values$data$rename_group(group_name(), input$new_group_name)
    rvs$new_group_name <- input$new_group_name
    removeModal()
  })

  observeEvent(input$dismiss_new_group_name, {
    removeModal()
  })

  observeEvent(input$copy_group, {
    showModal(modalDialog(
      textInput(
        inputId = ns("copy_group_name"),
        label = label_lang(
          de = "Neuer Gruppenname",
          en = "New group name"
        )
      ),
      footer = uiOutput(
        outputId = ns("modal_footer_copy")
      )
    ))
  })

  output$modal_footer_copy <- renderUI({
    req(input$copy_group_name)
    tagList(
      actionButtonQW(
        inputId = ns("confirm_copy_group"),
        label = label_lang(
          de = "Bestätigen",
          en = "Confirm"
        )
      ),
      actionButtonQW(
        inputId = ns("dismiss_copy_group"),
        label = label_lang(
          de = "Abbrechen",
          en = "Dismiss"
        )
      )
    )
  })

  observeEvent(input$confirm_copy_group, {
    .values$data$copy_group(group_name(), input$copy_group_name)
    removeModal()
  })

  observeEvent(input$dismiss_copy_group, {
    removeModal()
  })

  observeEvent(input$remove_group, {
    showModal(modalDialog(
      title = label_lang(
        de = "Bestätigen",
        en = "Confirm"
      ),
      footer = tagList(
        actionButtonQW(
          inputId = ns("confirm_remove_group"),
          label = label_lang(
            de = "Bestätigen",
            en = "Confirm"
          )
        ),
        actionButtonQW(
          inputId = ns("dismiss_remove_group"),
          label = label_lang(
            de = "Abbrechen",
            en = "Dismiss"
          )
        )
      )
    ))
  })

  observeEvent(input$confirm_remove_group, {
    .values$data$remove_group(group_name())
    removeModal()
  })

  observeEvent(input$dismiss_remove_group, {
    removeModal()
  })
}
