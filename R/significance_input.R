#' @rdname significance_input
#'
#' @param id The module's id.
#'
#' @export
significance_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("module_ui")
  )
}

#' Input module for level of significance
#'
#' User may input the level of significance or
#' confidence, choose between a \code{\link[shiny:sliderInput]{sliderInput}} or
#' a \code{\link[shiny:numericInput]{numericInput}} and gets a quick choice with
#' three \code{\link{actionButtonQW}}.
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @inheritParams observed_vector_input
#'
#' @return
#' \code{significance_input} returns a list of \code{\link[shiny:reactive]{reactives}}:
#' \tabular{ll}{
#'   \code{alpha} \tab The level of significance, which is one minus the level
#'   of confidence.
#' }
#'
#' @export
significance_input <- function(
  input, output, session, .values, .parent
) {

  ns <- session$ns

  self <- shiny::isolate({
    .parent$add_child(
      object = SessionObject$new("significance_input", session),
      removable = FALSE,
      return = "child"
    )
  })

  rvs <- shiny::reactiveValues(
    style_numeric = TRUE,
    is_significance = TRUE
  )

  output$module_ui <- shiny::renderUI({
    if (rvs$style_numeric) {
      ui <- shiny::numericInput(
        inputId = ns("alpha_numeric"),
        label = shiny::uiOutput(
          outputId = ns("label")
        ),
        value = isolate(alpha()),
        min = 0,
        max = 1
      )
    } else {
      ui <- shiny::sliderInput(
        inputId = ns("alpha_slider"),
        label = shiny::uiOutput(
          outputId = ns("label")
        ),
        value = isolate(alpha()),
        min = 0,
        max = 1,
        step = 0.01
      )
    }

    ui
  })

  output$label <- shiny::renderUI({
    if (rvs$style_numeric) {
      icon <- shiny::icon("sliders-h")
      tooltip <- QWUtils::label_lang(
        de = "Eingabe über Slider",
        en = "Input by slider"
      )
    } else {
      icon <- shiny::icon("calculator")
      tooltip <- QWUtils::label_lang(
        de = "Eingabe über Zahl",
        en = "Input by number"
      )
    }

    if (rvs$is_significance) {
      ui <- htmltools::tagList(
        QWUtils::actionButtonQW(
          inputId = ns("switch_significance"),
          label = QWUtils::label_lang(
            de = "Signifikanzniveau",
            en = "Level of significance"
          ),
          tooltip = QWUtils::label_lang(
            de = "Drücken für Konfidenzniveau",
            en = "Press for level of confidence"
          )
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.05"),
          label = "0.05",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.01"),
          label = "0.01",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.001"),
          label = "0.001",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("switch_style"),
          label = NULL,
          icon = icon,
          tooltip = tooltip
        )
      )
    } else {
      ui <- htmltools::tagList(
        QWUtils::actionButtonQW(
          inputId = ns("switch_significance"),
          label = QWUtils::label_lang(
            de = "Konfidenzniveau",
            en = "Level of confidence"
          ),
          tooltip = QWUtils::label_lang(
            de = "Drücken für Konfidenzniveau",
            en = "Press for level of confidence"
          )
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.95"),
          label = "0.95",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.99"),
          label = "0.99",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("alpha_0.999"),
          label = "0.999",
          color = "primary"
        ),
        QWUtils::actionButtonQW(
          inputId = ns("switch_style"),
          label = NULL,
          icon = icon,
          tooltip = tooltip
        )
      )
    }

    ui
  })

  shiny::observeEvent(input$switch_style, {
    if (rvs$style_numeric) {
      shiny::updateSliderInput(
        session = session,
        inputId = "alpha_slider",
        value = input$alpha_numeric
      )
    } else {
      shiny::updateNumericInput(
        session = session,
        inputId = "alpha_numeric",
        value = input$alpha_slider
      )
    }

    rvs$style_numeric <- !rvs$style_numeric
  })

  shiny::observeEvent(input$switch_significance, {
    if (rvs$style_numeric) {
      shiny::updateNumericInput(
        session = session,
        inputId = "alpha_numeric",
        value = 1 - input$alpha_numeric
      )
    } else {
      shiny::updateSliderInput(
        session = session,
        inputId = "alpha_slider",
        value = 1 - input$alpha_slider
      )
    }

    rvs$is_significance <- !rvs$is_significance
  })

  purrr::walk(
    c(
      "alpha_0.05", "alpha_0.01", "alpha_0.001", "alpha_0.95", "alpha_0.99",
      "alpha_0.999"
    ),
    function(id) {
      shiny::observeEvent(input[[id]], {
        if (rvs$style_numeric) {
          shiny::updateNumericInput(
            session = session,
            inputId = "alpha_numeric",
            value = as.numeric(stringr::str_extract(id, "\\d\\.[\\d]+"))
          )
        } else {
          shiny::updateSliderInput(
            session = session,
            inputId = "alpha_slider",
            value = as.numeric(stringr::str_extract(id, "\\d\\.[\\d]+"))
          )
        }
      })
    }
  )

  alpha <- shiny::reactive({
    if (rvs$style_numeric) {
      alpha <- QWUtils::fallback(input$alpha_numeric, 0.05)
    } else {
      alpha <- QWUtils::fallback(input$alpha_slider, 0.05)
    }

    if (!rvs$is_significance) {
      alpha <- 1 - alpha
    }

    alpha
  })

  return_list <- list(
    alpha = alpha
  )

  return(return_list)
}
