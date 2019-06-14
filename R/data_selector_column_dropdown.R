#' @rdname data_selector_column_dropdown_ui
#'
#' @param id The module's id
#'
#' @export
data_selector_column_dropdown_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: inline",
    dropdown(
      inputId = ns("column_dropdown"),
      icon = icon("sliders-h"),
      style = "material-flat",
      size = "xs",
      up = TRUE,
      actionButtonQW(
        inputId = ns("open_column"),
        label = label_lang(
          de = "Öffnen",
          en = "Open"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("rename_column"),
        label = label_lang(
          de = "Umbenennen",
          en = "Rename"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("remove_column"),
        label = label_lang(
          de = "Entfernen",
          en = "Remove"
        ),
        dropdown = TRUE
      ),
      actionButtonQW(
        inputId = ns("show_class"),
        label = label_lang(
          de = "Klasse",
          en = "Class"
        ),
        dropdown = TRUE
      )
    )
  )
}

#' Dropdown for column actions
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @inheritParams observed_vector_input
#' @param group_name,dataset_name,column_name \code{\link[shiny:reactive]{Reactives}}
#'   containing the group, dataset and column selected by the user.
#'
#' @export
data_selector_column_dropdown <- function(
  input, output, session, .data, .values, .parent, group_name, dataset_name,
  column_name
) {

  ns <- session$ns

  self <- Node$new(ns("column_dropdown"), .parent, session)

  rvs <- reactiveValues(
    counter_column_datatable = 0
  )

  observeEvent(input$open_column, {
    counter <- rvs$counter_column_datatable
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = paste0(
          group_name(), ": ", dataset_name(), ": ",
          paste0(column_name(), collapse = ", ")
        ),
        value = group_name() %_% dataset_name() %_% paste0(
          column_name(), collapse = ""
        ),
        dataTableOutput(
          outputId = ns("column_datatable" %_% counter)
        )
      )
    )

    output[["column_datatable" %_% counter]] <- renderDataTable({
      isolate({
        datatable(.data$get_column_value(
          group_name = group_name(),
          dataset_name = dataset_name(),
          column_name = column_name(),
          as_data_frame = TRUE
        ))
      })
    })

    rvs$counter_column_datatable <- rvs$counter_column_datatable + 1
  })

  observeEvent(input$rename_column, {
    showModal(modalDialog(
      title = label_lang(
        de = paste0("Benenne ", column_name(), " um"),
        en = paste0("Rename ", column_name())
      ),
      QWUtils::checked_text_input_ui(
        id = ns("id_new_column_name")
      ),
      footer = uiOutput(
        outputId = ns("rename_column_footer")
      )
    ))
  })

  output$rename_column_footer <- renderUI({
    ui <- tagList(
      actionButtonQW(
        inputId = ns("dismiss_rename_column"),
        label = .values$label$modal$dismiss
      )
    )
    if (!new_column_name_return$error()) {
      ui <- c(
        tagList(
          actionButtonQW(
            inputId = ns("confirm_rename_column"),
            label = .values$label$modal$confirm
          )
        ),
        ui
      )
    }
    ui
  })

  observeEvent(input$confirm_rename_column, {
    .data$rename_columns(
      group_name = group_name(),
      dataset_name = dataset_name(),
      new_column_names = new_column_name_return$name(),
      old_column_names = column_name()
    )
    removeModal()
  })

  observeEvent(input$dismiss_rename_column, {
    removeModal()
  })

  observeEvent(input$remove_column, {
    showModal(modalDialog(
      title = label_lang(
        de = paste0("Entferne ", column_name()),
        en = paste0("Remove ", column_name())
      ),
      footer = tagList(
        actionButtonQW(
          inputId = ns("confirm_remove_column"),
          label = .values$label$modal$confirm
        ),
        actionButtonQW(
          inputId = ns("dismiss_remove_column"),
          label = .values$label$modal$dismiss
        )
      )
    ))
  })

  observeEvent(input$confirm_remove_column, {
    .data$remove_columns(group_name(), dataset_name(), column_name())
    removeModal()
  })

  observeEvent(input$dismiss_remove_column, {
    removeModal()
  })

  observeEvent(input$show_class, {
    group_name <- group_name()
    dataset_name <- dataset_name()
    column_name <- column_name()

    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = paste0(
          label_lang(
            de = "Klasse: ",
            en = "Class: "
          ),
          group_name, ": ", dataset_name, ": ", column_name
        ),
        dataTableOutput(
          outputId = ns(
            "class" %_% group_name %_% dataset_name %_% column_name
          )
        ),
        span(
          selectInput(
            inputId = ns(
              "select_test_class" %_% group_name %_% dataset_name %_% column_name
            ),
            label = label_lang(
              de = "Zu testende Klasse",
              en = "Class to get tested"
            ),
            choices = names(.data$get_column_class(
              group_name = group_name,
              dataset_name = dataset_name,
              column_name = column_name,
              verbose = TRUE
            ))
          ),
          actionButtonQW(
            inputId = ns(
              "test_class" %_% group_name %_% dataset_name %_% column_name
            ),
            label = "Test"
          )
        )
      )
    )

    if (!("class" %_% group_name %_% dataset_name %_% column_name) %in%
        names(output)) {
      output[[
        "class" %_% group_name %_% dataset_name %_% column_name
      ]] <- renderDT({
        column_class <- .data$get_column_class(
          group_name = group_name,
          dataset_name = dataset_name,
          column_name = column_name,
          verbose = TRUE
        )

        df <- tibble(
          class = names(column_class),
          value = column_class
        )

        names(df) <- label_lang(
          de = c("Klasse", "Wert"),
          en = c("Class", "Value")
        )

        datatable(df)
      })

      observeEvent(
        input[["test_class" %_% group_name %_% dataset_name %_% column_name]], {
          .data$column_class_allowed(
            group_name = group_name,
            dataset_name = dataset_name,
            column_name = column_name,
            class = input[["select_test_class" %_%
                group_name %_% dataset_name %_% column_name]]
          )
        }
      )
    }
  })

  new_column_name_return <- callModule(
    module = QWUtils::checked_text_input,
    id = "id_new_column_name",
    .data = .data,
    .values = .values,
    .parent = self,
    .label = label_lang(
      de = "Neuer Spaltenname",
      en = "New column name"
    ),
    .value = column_name
  )
}
