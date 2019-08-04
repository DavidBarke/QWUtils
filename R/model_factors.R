#' Generate formula for multiple linear regression
#'
#' Get a character representing the right side of a formula with interactions
#' between factors up to the specified interaction_level.
#'
#' @param factors Factor names.
#' @param interaction_level Maximum length of interaction between factors.
factors_to_formula_text <- function(factors, interaction_level = 0) {
  form_list <- purrr::map(seq_len(interaction_level + 1), function(level) {
    conbns <- utils::combn(factors, level)
    purrr::map(seq_len(ncol(combns)), function(col) {
      paste(combns[, col], collapse = ":")
    })
  })
  paste(unlist(form_list), collapse = "+")
}

#' Factory generating a formula for multiple linear regression
#'
#' Get a function representing the right side of a formula with interactions
#' between factors up to the specified interaction level. Call this function
#' with the actual factors to get an up-to-date formula.
#'
#' @param factor_indices Integer vector.
#' @param interaction_level Maximum length of interaction between factors.
#'
#' @export
factors_to_formula_factory <- function(factor_indices, interaction_level) {
  templates <- purrr::map_chr(factor_indices, function(index) {
    paste0("{factors[", index, "]}")
  })
  form_list <- purrr::map(seq_len(interaction_level + 1), function(level) {
    combns <- utils::combn(templates, level)
    purrr::map(seq_len(ncol(combns)), function(col) {
      paste(combns[, col], collapse = ":")
    })
  })
  text <- paste(unlist(form_list), collapse = "+")

  text <- paste0("{response}~", text)

  function(response, factors) {
    glue::glue(text, response = response, factors = factors)
  }
}

#' All possible effects for a simple linear model between factors
#'
#' @param factors A character vector containing the factor names.
#'
#' @export
all_effects <- function(factors) {
  form_list <- purrr::map(seq_len(length(factors)), function(level) {
    combns <- utils::combn(factors, level)
    purrr::map(seq_len(ncol(combns)), function(col) {
      paste(combns[, col], collapse = ":")
    })
  })

  unlist(form_list)
}

#' Get a string representing a simple linear model two-factor explanation of a
#' response variable.
#'
#' @param response_name Name of the response variable.
#' @param factor_1,factor_2 A \code{\link{FacDesignFactor}} object.
#' @param interactions If \code{\link[base:logical]{TRUE}}, the interaction
#' between both factors is included in the formula.
#'
#' @export
two_factor_formula <- function(
  response_name, factor_1, factor_2, interactions = FALSE
) {
  if (interactions) {
    lm_formula <- paste0(
      response_name, "~", factor_1$get_id(), "+",
      factor_2$get_id(), "+", factor_1$get_id(), ":", factor_2$get_id()
    )
  } else {
    lm_formula <- paste0(
      response_name, "~", factor_1$get_id(), "+",
      factor_2$get_id()
    )
  }
}
