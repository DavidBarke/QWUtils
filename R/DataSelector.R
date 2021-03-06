#' DataSelector
#'
#' \code{\link[R6:R6Class]{R6Class}} for generating customised selections of
#' groups, datasets and columns. After initialing the DataSelector an arbitrary
#' amout of elements may be added in the \code{\link[shiny:shiny-package]{shiny}}
#' server function. After adding all elements the DataSelector gets activated
#' by invoking the method \code{this$callModule()}. The display of the elements
#' is handled by \code{\link{data_selector_output}}. For each element of a
#' \code{\link{DataSelector}} \code{\link{data_selector_output}} has to be called
#' either in the \code{ui} function or inside of
#' \code{\link[shiny:renderUI]{renderUI}} to display the UI of the corresponding
#' element.
#'
#' @section Usage:
#' \preformatted{DataSelector$new(
#'   id = "data_selector"
#' )$Group(
#'   id = "group",
#'   label = "Select group"
#' )$Dataset(
#'   id = "dataset",
#'   group_id = "group",
#'   label = "Select dataset"
#' )$Column(
#'   id = "column_1",
#'   group_id = "group",
#'   dataset_id = "dataset",
#'   label = "Select column 1",
#'   setdiff_id = "column_2"
#' )$Column(
#'   id = "column_2",
#'   group_id = "group",
#'   dataset_id = "dataset",
#'   label = "Select column 2",
#'   setdiff_id = "column_1"
#' )$callModule(
#'   .values = values,
#'   .parent = parent
#' )
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id)}}{Initialize the data_selector.
#'     \tabular{ll}{
#'       \code{id} \tab The id of the data_selector.
#'     }
#'   }
#'   \item{\code{callModule(.values, .parent)}}{Activate the data_selector
#'     inside the server function. Before calling this function NO ui will be
#'     displayed.
#'     \tabular{ll}{
#'       \code{.values} \tab List of \code{\link[shiny:reactive]{reactive}}-like objects
#'       as specified by XXX. \cr
#'       \code{.parent} \tab \code{\link{Node}}.
#'     }
#'   }
#'   \item{\code{Column(id, group_id, dataset_id, label, extended = TRUE,
#'     setdiff_id = NULL, multiple = FALSE)}}{Add a
#'     \code{\link[shiny:selectInput]{selectInput}} for selecting a column
#'     of a dataset.
#'     \tabular{ll}{
#'       \code{id} \tab \code{\link[base:character]{Character}}. The id of the
#'       column element. \cr
#'       \code{group_id} \tab \code{\link[base:character]{Character}}. The id
#'       of the parent group element. \cr
#'       \code{dataset_id} \tab \code{\link[base:character]{Character}}. The id
#'       of the parent dataset element. \cr
#'       \code{label} \tab The label of the
#'         \code{\link[shiny:selectInput]{selectInput}}. \cr
#'       \code{extended} \tab \code{\link[base:logical]{Logical}}. If
#'         \code{\link[base:logical]{TRUE}}, the label of the
#'         \code{\link[shiny:selectInput]{selectInput}} is extended by a
#'         \code{\link{data_selector_column_dropdown}}. \cr
#'       \code{setdiff_id} \tab \code{\link[base:character]{Character}} vector.
#'         The  value of the \code{\link[shiny:selectInput]{selectInput}} is
#'         compared to the values of all column elements with
#'         \code{id \%in\% setdiff_id}. If one of these elements has the same
#'         value, this element's \code{\link[shiny:selectInput]{selectInput}}
#'         gets updated to the first free column.
#'         If \code{multiple =} \code{\link[base:logical]{TRUE}}, no comparison
#'         is done.
#'         \cr
#'       \code{multiple} \tab \code{\link[base:logical]{Logical}}. If
#'         \code{\link[base:logical]{TRUE}}, multiple columns can be selected.
#'     }
#'   }
#'   \item{\code{Dataset(id, group_id, label, extended = TRUE)}}{Add a
#'     \code{\link[shiny:selectInput]{selectInput}} for selecting a dataset
#'     of a group.
#'     \tabular{ll}{
#'       \code{id} \tab The id of the dataset element. \cr
#'       \code{group_id} \tab The id of the parent group element. \cr
#'       \code{label} \tab The label of the
#'         \code{\link[shiny:selectInput]{selectInput}}. \cr
#'       \code{extended} \tab \code{\link[base:logical]{Logical}}. If
#'         \code{\link[base:logical]{TRUE}}, the label of the
#'         \code{\link[shiny:selectInput]{selectInput}} is extended by a
#'         \code{\link{data_selector_dataset_dropdown}}.
#'     }
#'   }
#'   \item{\code{Group(id, label, extended = TRUE)}}{Add a
#'     \code{\link[shiny:selectInput]{selectInput}} for selecting a group.
#'     \tabular{ll}{
#'       \code{id} \tab The id of the group element. \cr
#'       \code{label} \tab The label of the
#'         \code{\link[shiny:selectInput]{selectInput}}. \cr
#'       \code{extended} \tab \code{\link[base:logical]{Logical}}. If
#'         \code{\link[base:logical]{TRUE}}, the label of the
#'         \code{\link[shiny:selectInput]{selectInput}} is extended by a
#'         \code{\link{data_selector_column_dropdown}}.
#'     }
#'   }
#' }
#'
#' @name DataSelector

#' @export
DataSelector <- R6::R6Class(
  classname = "DataSelector",
  public = list(
    initialize = function(id) {
      private$id <- id
    },

    callModule = function(.values, .parent) {
      callModule(
        module = private$data_selector_module,
        id = private$id,
        .values = .values,
        .parent = .parent
      )
    },

    Column = function(
      id, group_id, dataset_id, label, extended = TRUE, setdiff_id = NULL,
      multiple = FALSE
    ) {
      private$elements_reactive_list[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        assign(
          id %_% "column_name",
          envir = .envir,
          reactive({
            req(input[[id %_% "select_column"]])
          })
        )

        selector_tree <- get("selector_tree", envir = .envir)
        selector_tree[[group_id]][[dataset_id]][[id]] <- list(1)
        assign(
          "selector_tree",
          envir = .envir,
          selector_tree
        )

        if (!multiple) {
          observeEvent(input[[id %_% "select_column"]], {
            for (sd_id in setdiff_id) {
              if (input[[sd_id %_% "select_column"]] ==
                  input[[id %_% "select_column"]]) {
                choices <- .values$data$get_column_names(
                  group_name <- get(
                    group_id %_% "group_name", envir = .envir
                  )(),
                  dataset_name <- get(
                    dataset_id %_% "dataset_name", envir = .envir
                  )()
                )

                selector_tree <- get("selector_tree", envir = .envir)
                taken_choices <- character()
                for (column_id in names(selector_tree[[group_id]][[dataset_id]])) {
                  taken_choices <- c(
                    taken_choices,
                    input[[column_id %_% "select_column"]]
                  )
                }

                selected <- setdiff(
                  choices,
                  taken_choices
                )[1]

                updateSelectInput(
                  session = session,
                  inputId = sd_id %_% "select_column",
                  selected = selected
                )
              }
            }
          })
        }
      }

      private$elements_server[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        ns <- session$ns

        if (extended) {
          label <- div(
            label,
            data_selector_column_dropdown_ui(
              id = ns(id %_% "column_dropdown")
            )
          )

          assign(
            id %_% "column_dropdown",
            envir = .envir,
            callModule(
              module = data_selector_column_dropdown,
              id = id %_% "column_dropdown",
              .values = .values,
              .parent = .parent,
              group_name = get(group_id %_% "group_name", envir = .envir),
              dataset_name = get(dataset_id %_% "dataset_name", envir = .envir),
              column_name = get(id %_% "column_name", envir = .envir)
            )
          )
        }

        output[[id %_% "ui"]] <- renderUI({
          choices <- .values$data$get_column_names(
            group_name = get(group_id %_% "group_name", envir = .envir)(),
            dataset_name = get(dataset_id %_% "dataset_name", envir = .envir)()
          )
          start_index <- get("start_index", envir = .envir)
          selected <- choices[start_index]
          assign(
            "start_index",
            envir = .envir,
            start_index + 1
          )
          selectInput(
            inputId = ns(id %_% "select_column"),
            label = label,
            choices = choices,
            selected = selected,
            multiple = multiple
          )
        })

        return_list <- list()

        return_list[["name"]] <- shiny::reactive({req(input[[id %_% "select_column"]])})

        return(return_list)
      }

      self
    },

    Dataset = function(id, group_id, label, extended = TRUE) {
      private$elements_reactive_list[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        assign(
          id %_% "dataset_name",
          envir = .envir,
          reactive({
            req(input[[id %_% "select_dataset"]])
          })
        )
      }

      private$elements_server[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        ns <- session$ns

        if (extended) {
          label <- div(
            label,
            data_selector_dataset_dropdown_ui(
              id = ns(id %_% "dataset_dropdown")
            )
          )

          assign(
            id %_% "dataset_dropdown",
            envir = .envir,
            callModule(
              module = data_selector_dataset_dropdown,
              id = id %_% "dataset_dropdown",
              .values = .values,
              .parent = .parent,
              group_name = get(group_id %_% "group_name", envir = .envir),
              dataset_name = get(id %_% "dataset_name", envir = .envir)
            )
          )
        }

        output[[id %_% "ui"]] <- renderUI({
          selectInput(
            inputId = ns(id %_% "select_dataset"),
            label = label,
            choices = .values$data$get_dataset_names(
              group_name = get(group_id %_% "group_name", envir = .envir)()
            )
          )
        })

        return_list <- list()

        return_list[["name"]] <- get(id %_% "dataset_name", envir = .envir)

        return(return_list)
      }

      self
    },

    Group = function(id, label, extended = TRUE) {
      private$elements_reactive_list[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        assign(
          id %_% "group_name",
          envir = .envir,
          reactive({
            req(input[[id %_% "select_group"]])
          })
        )
      }

      private$elements_server[[id]] <- function(
        input, output, session, .values, .parent, .envir
      ) {
        ns <- session$ns

        if (extended) {
          label <- div(
            label,
            data_selector_group_dropdown_ui(
              id = ns(id %_% "group_dropdown")
            )
          )

          assign(
            id %_% "group_dropdown",
            envir = .envir,
            callModule(
              module = data_selector_group_dropdown,
              id = id %_% "group_dropdown",
              .values = .values,
              .parent = .parent,
              group_name = get(id %_% "group_name", envir = .envir)
            )
          )
        }

        output[[id %_% "ui"]] <- renderUI({
          selectInput(
            inputId = ns(id %_% "select_group"),
            label = label,
            choices = .values$data$get_group_names()
          )
        })

        return_list <- list()

        return_list[["name"]] <- get(id %_% "group_name", envir = .envir)

        return(return_list)
      }

      self
    }
  ),
  private = list(
    data_selector_module = function(
      input, output, session, .values, .parent
    ) {

      ns <- session$ns

      self <- shiny::isolate({
        .parent$add_child(
          object = SessionObject$new("data_selector", session),
          removable = FALSE,
          return = "child"
        )
      })

      .envir <- environment()

      selector_tree <- list()

      start_index <- 1

      map(private$elements_reactive_list, function(element) {
        element(input, output, session, .values, self, .envir)
      })

      return_list <- map(private$elements_server, function(element) {
        element(input, output, session, .values, self, .envir)
      })

      return(return_list)
    },

    elements_reactive_list = list(),
    elements_server = list(),
    id = NULL
  )
)

#' data_selector_output
#'
#' @param id id of the corresponding \code{\link{DataSelector}}.
#' @param element_id id of the element to display
#'
#' @export
data_selector_output <- function(id, element_id) {
  ns <- NS(id)

  uiOutput(
    outputId = ns(element_id %_% "ui")
  )
}
