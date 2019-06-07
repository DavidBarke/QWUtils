#' FacDesign
#'
#' R6Class representing a factorial design.
#'
#' @section Usage:
#' \preformatted{fac_design <- FacDesign$new(name, k)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name, k, rep = 1, center = 0, fac_names = LETTERS[seq_len(k)],
#'   response_name = "response", index_name = "index", response = NULL)}}{
#'   Initialize the factorial design.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. The factorial
#'       design's name \cr
#'       \code{k} \tab \code{\link[base:integer]{Integer}}. Number of variable
#'       factors. \cr
#'       \code{center} \tab \code{\link[base:integer]{Integer}}. Number of
#'       center points. \cr
#'       \code{fac_names} \tab \code{\link[base:character]{Character}} vector
#'       containing the names of the variable factors. \cr
#'       \code{response_name} \tab \code{\link[base:character]{Character}}. Name
#'       of the response variable. \cr
#'       \code{index_name} \tab \code{\link[base:character]{Character}}. Name of
#'       the index variable. \cr
#'       \code{response} \tab \code{\link[base:numeric]{Numeric}} vector
#'       containing the values of the response variable for the standardised
#'       factorial design. If \code{response = NULL}, you have to add a response
#'       later via \code{this$add_response()}.
#'     }
#'   }
#'   \item{\code{add_response(response)}}{Add or update the response of the
#'   factorial design.
#'     \tabular{ll}{
#'       \code{response} \tab \code{\link[base:numeric]{Numeric}} vector
#'       containing the values of the response variable for the standardised
#'       factorial design.
#'     }
#'   }
#'   \item{\code{get_name()}}{Get the factorial design's name.
#'   }
#'   \item{\code{get_names()}}{
#'   Get a list with the following entries:
#'     \tabular{ll}{
#'       fac \tab The variable factors' names. \cr
#'       index \tab The index' name. \cr
#'       name \tab The factorial design's name. \cr
#'       response \tab The factorial design response's name.
#'     }
#'    The arguments decide wheter a name is part of the list or not.
#'   }
#'   \item{\code{get_table(randomized = TRUE, index = TRUE, factors = TRUE,
#'   response = TRUE, center = FALSE)}}{Get a \code{\link[tibble:tibble]{tibble}}
#'   representing the factorial design.
#'     \tabular{ll}{
#'       \code{randomized} \tab If \code{\link[base:logical]{TRUE}}, the rows are
#'       ordered in run order and not in standardised order. \cr
#'       \code{index} \tab If \code{\link[base:logical]{TRUE}}, the index column
#'       is included in the output. \cr
#'       \code{factors} \tab If \code{\link[base:logical]{TRUE}}, the factor
#'       columns are included in the output. \cr
#'       \code{response} \tab If \code{\link[base:logical]{TRUE}}, the response
#'       column is included in the output. \cr
#'       \code{center} \tab If \code{\link[base:logical]{TRUE}}, the rows
#'       containing observations for the center points are included in the output.
#'     }
#'   }
#'   \item{\code{has_response()}}{Returns a \code{\link[base:logical]{logical}}
#'   indicating whether the factorial design has a response or not.
#'   }
#'   \item{\code{nrow()}}{Get the number of rows of the factorial design.
#'   }
#'   \item{\code{rename_fac_names(new_names, old_names = NULL)}}{Set the factors'
#'   names. If \code{old_names = NULL} the length of \code{new_names} must be
#'   equal to the number of factors, otherwise the length of \code{new_names}
#'   and \code{old_names} has to be equal.
#'     \tabular{ll}{
#'       \code{new_names} \tab \code{\link[base:character]{Character}}
#'       vector. \cr
#'       \code{old_names} \tab \code{\link[base:character]{Character}}
#'       vector or \code{\link[base:NULL]{NULL}}.
#'     }
#'   }
#'   \item{\code{rename_index_name(new_name)}}{Rename the index  with the
#'   \code{\link[base:character]{character}} \code{new_name}.
#'   }
#'   \item{\code{rename_name(new_name)}}{Rename the factorial design with the
#'   \code{\link[base:character]{character}} \code{new_name}.
#'   }
#'   \item{\code{rename_response_name(new_name)}}{Rename the response with the
#'   \code{\link[base:character]{character}} \code{new_name}.
#'   }
#' }
#'
#' @name FacDesign
NULL

#' @export
FacDesign <- R6::R6Class(
  classname = "FacDesignGenerator",
  public = list(
    initialize = function(
      name, k, rep = 1, center = 0, fac_names = LETTERS[seq_len(k)],
      response_name = "response", index_name = "index", response = NULL
    ) {
      private$names <- list(
        fac = reactiveVal(fac_names),
        index = reactiveVal(index_name),
        name = reactiveVal(name),
        response = reactiveVal(response_name)
      )

      fac_design_table <- tibble(
        index = seq_len((2^k)*rep)
      )

      for (i in seq_len(k)) {
        fac_design_table[[i + 1]] <- rep(
          rep(rep(c(1, -1), each = 2^(k - i)), times = 2^(i - 1)), times = rep
        )
      }

      fac_design_table$response <- if (is.null(response)) {
        private$.has_response <- reactiveVal(FALSE)
        NA
      } else {
        private$.has_response <- reactiveVal(TRUE)
        response
      }

      private$.nrow <- reactiveVal(nrow(fac_design_table))
      private$fac_design_table <- reactiveVal(fac_design_table)
    },

    add_response = function(response) {
      fac_design_table <- private$fac_design_table()
      fac_design_table$response <- response
      private$fac_design_table(fac_design_table)
      private$.has_response(TRUE)
    },

    get_name = function() {
      private$names$name()
    },

    get_names = function() {
      list(
        fac = private$names$fac(),
        index = private$names$index(),
        name = private$names$name(),
        response = private$names$response()
      )
    },

    get_table = function(
      randomized = TRUE, index = TRUE, factors = TRUE, response = TRUE,
      center = FALSE
    ) {
      fac_design_table <- private$fac_design_table()
      names(fac_design_table) <- c(
        private$names$index(), private$names$fac(), private$names$response()
      )
      return_names <- character()
      if (index) {
        return_names <- c(return_names, private$names$index())
      }
      if (factors) {
        return_names <- c(return_names, private$names$fac())
      }
      if (response) {
        return_names <- c(return_names, private$names$response())
      }
      fac_design_table[return_names]
    },

    has_response = function() {
      private$.has_response()
    },

    nrow = function() {
      private$.nrow()
    },

    rename_fac_names = function(new_names, old_names = NULL) {
      if (is.null(old_names)) {
        stopifnot(
          length(new_names) == length(private$names$fac()),
          length(unique(new_names)) == length(new_names)
        )
        private$names$fac(new_names)
      } else {
        stopifnot(all(old_names %in% private$names$fac()))
        fac_names <- private$names$fac()
        names(fac_names) <- fac_names
        fac_names[new_names] <- new_names
        names(fac_names) <- NULL
        private$names$fac(fac_names)
      }
    },

    rename_index_name = function(new_name) {
      private$names$index(new_name)
    },

    rename_name = function(new_name) {
      private$names$name(new_name)
    },

    rename_response_name = function(new_name) {
      private$names$response(new_name)
    }
  ),
  private = list(
    fac_design_table = NULL,
    .has_response = NULL,
    names = list(
      fac = NULL,
      index = NULL,
      name = NULL,
      response = NULL
    ),
    .nrow = NULL
  )
)
