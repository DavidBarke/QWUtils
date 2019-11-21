#' @rdname data_selector_dataset_dropdown
#'
#' @param id The module's id.
#'
#' @export
data_selector_dataset_dropdown_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: inline",
    dropdown(
      inputId = ns("dataset_dropdown"),
      icon = icon("sliders-h"),
      style = "material-flat",
      size = "xs",
      up = TRUE,
      actionButtonQW(
        inputId = ns("open_dataset"),
        label = label_lang(
          de = "Öffnen",
          en = "Open"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("information"),
        label = "Information",
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("change_dataset_name"),
        label = label_lang(
          de = "Umbennen",
          en = "Rename"
        ),
        dropdown = TRUE
      ),
      # TODO
      # actionButtonQW(
      #   inputId = ns("change_dataset_column_names"),
      #   label = label_lang(
      #     de = "Spaltennamen",
      #     en = "Column names"
      #   ),
      #   dropdown = TRUE
      # ),
      actionButtonQW(
        inputId = ns("remove_dataset"),
        label = label_lang(
          de = "Entfernen",
          en = "Remove"
        ),
        dropdown = TRUE
      )
    )
  )
}

#' Dropdown for dataset actions
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @inheritParams observed_vector_input
#' @param group_name,dataset_name \code{\link[shiny:reactive]{Reactives}}
#'   containing the group and dataset selected by the user.
#'
#' @export
data_selector_dataset_dropdown <- function(
  input, output, session, .values, .parent, group_name, dataset_name
) {

  ns <- session$ns

  self <- shiny::isolate({
    .parent$add_child(
      object = SessionObject$new("data_selector_dataset_dropdown", session),
      removable = FALSE,
      return = "child"
    )
  })

  rvs <- reactiveValues(
    open_dataset_counter = 0
  )

  observeEvent(input$open_dataset, {
    rvs$open_dataset_counter <- rvs$open_dataset_counter + 1
    open_dataset_counter <- rvs$open_dataset_counter
    selected_group <- group_name()
    selected_dataset <- dataset_name()
    tab_values <- .values$viewer$data$get("tab_values")
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = paste0(selected_group, ": ", selected_dataset),
        fluidRow(
          column(
            width = 3,
            downloadButton(
              outputId = ns("csv_download" %_% open_dataset_counter),
              label = "CSV"
            )
          ),
          column(
            width = 3,
            downloadButton(
              outputId = ns("xlsx_download" %_% open_dataset_counter),
              label = "Excel-CSV"
            )
          )
        ),
        dataTableOutput(
          outputId = ns("open_dataset" %_% open_dataset_counter)
        ),
        value = ns(selected_group %_% selected_dataset)
      )
    )

    output[["open_dataset" %_% open_dataset_counter]] <- renderDataTable({
      # Verweis auf selected_group und selected_dataset anstelle der Inputs, um
      # die Reaktivität aufzulösen
      datatable(.values$data$get_dataset_value(selected_group, selected_dataset))
    })

    root_filepath <- selected_group %_% selected_dataset %_% Sys.Date()

    output[["csv_download" %_% open_dataset_counter]] <- downloadHandler(
      filename = function() {
        paste0(root_filepath, ".csv")
      },
      content = function(file) {
        write_csv(.values$data$get_dataset_value(selected_group, selected_dataset), file)
      },
      contentType = "text/csv"
    )

    output[["xlsx_download" %_% open_dataset_counter]] <- downloadHandler(
      filename = function() {
        paste0(root_filepath, ".csv")
      },
      content = function(file) {
        write_excel_csv(
          .values$data$get_dataset_value(selected_group, selected_dataset),
          file
        )
      },
      contentType = "text/csv"
    )
  })

  observeEvent(input$information, {
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = paste0("Datensatz: ", dataset_name()),
          en = paste0("Dataset: ", dataset_name())
        ),
        value = group_name() %_% dataset_name(),
        htmlOutput(
          outputId = ns(group_name() %_% dataset_name() %_% "dataset_information")
        )
      )
    )

    if (!group_name() %_% dataset_name() %_% "dataset_information" %in% names(output)) {
      group_name <- group_name()
      dataset_name <- dataset_name()

      output[[group_name %_% dataset_name %_% "dataset_information"]] <- renderUI({
        column_names <- .values$data$get_column_names(
          group_name = group_name,
          dataset_name = dataset_name
        )

        classes <- .values$data$get_dataset_class(
          group_name = group_name,
          dataset_name = dataset_name
        )

        HTML(
          label_lang(
            de = "Spaltennamen: ",
            en = "Column names: "
          ),
          paste0(column_names, collapse = ", "), "<br/>",
          label_lang(
            de = "Klassen: ",
            en = "Classes: "
          ),
          paste0(classes, collapse = ", ")
        )
      })
    }
  })

  observeEvent(input$change_dataset_name, {
    showModal(modalDialog(
      title = label_lang(
        de = paste0("Benenne ", dataset_name(), " um"),
        en = paste0("Rename ", dataset_name())
      ),
      textInput(
        inputId = ns("new_dataset_name"),
        label = label_lang(
          de = "Neuer Name",
          en = "New name"
        )
      ),
      footer = uiOutput(
        outputId = ns("modal_footer_rename_dataset")
      )
    ))
  })

  output$modal_footer_rename_dataset <- renderUI({
    req(input$new_dataset_name)
    tagList(
      actionButtonQW(
        inputId = ns("confirm_new_dataset_name"),
        label = label_lang(
          de = "Bestätigen",
          en = "Confirm"
        )
      ),
      actionButtonQW(
        inputId = ns("dismiss_new_dataset_name"),
        label = label_lang(
          de = "Abbrechen",
          en = "Dismiss"
        )
      )
    )
  })

  observeEvent(input$confirm_new_dataset_name, {
    .values$data$rename_dataset(
      group_name(),
      dataset_name(),
      input$new_dataset_name
    )
    removeModal()
  })

  observeEvent(input$dismiss_new_dataset_name, {
    removeModal()
  })

  observeEvent(input$change_dataset_column_names, {
    showModal(modalDialog(
      "TODO"
    ))
  })

  observeEvent(input$remove_dataset, {
    showModal(modalDialog(
      title = label_lang(
        de = paste0("Entferne ", dataset_name()),
        en = paste0("Remove ", dataset_name())
      ),
      footer = tagList(
        actionButtonQW(
          inputId = ns("confirm_remove_dataset"),
          label = label_lang(
            de = "Bestätigen",
            en = "Confirm"
          )
        ),
        actionButtonQW(
          inputId = ns("dismiss_remove_dataset"),
          label = label_lang(
            de = "Abbrechen",
            en = "Dismiss"
          )
        )
      )
    ))
  })

  observeEvent(input$confirm_remove_dataset, {
    .values$data$remove_dataset(
      group_name(),
      dataset_name()
    )
    removeModal()
  })

  observeEvent(input$dismiss_remove_dataset, {
    removeModal()
  })
}
