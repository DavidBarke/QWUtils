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
#'   \item{\code{new(name, factor_storage, rep = 1, center = 0, response_name =
#'   "response", index_name = "index", response = NULL}}{
#'   Initialize the factorial design.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. The factorial
#'       design's name \cr
#'       \code{factor_nodes} \cr An object of class \code{\link{ExplorerNode}}
#'       with children of explorer_class "fac_design_factor_explorer_class"
#'       storing objects of class \code{\link{FacDesignFactor}}. \cr
#'       \code{rep} \tab \code{\link[base:numeric]{Numeric}} value giving the
#'       number of replicates per factor combination. \cr
#'       \code{center} \tab Currently defunct. \code{\link[base:integer]{Integer}}.
#'       Number of center points. \cr
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
  classname = "FacDesign",
  public = list(
    initialize = function(
      name, factor_nodes, rep = 1, center = 0,
      response_name = "response", index_name = "index", response = NULL
    ) {
      # factor_nodes children must not be group nodes
      purrr::walk(factor_nodes$children(), function(node) {
        if (node$is_group_node()) {
          stop("FacDesign: factor_nodes' children must not be group nodes")
        }
      })

      k <- length(factor_nodes$children())

      private$names <- list(
        index = reactiveVal(index_name),
        name = reactiveVal(name),
        response = reactiveVal(response_name)
      )

      fac_design_table <- tibble(
        index = seq_len((2^k)*rep + center)
      )

      private$factor_nodes <- factor_nodes
      private$linear_model_storage <- ObjectStorage$new(
        allowed_classes = "LinearModel"
      )

      for (i in seq_len(k)) {
        fac_permutation <- rep(
          rep(rep(c(-1, 1), each = 2^(k - i)), times = 2^(i - 1)), times = rep
        )
        fac_design_table[[i + 1]] <- c(
          fac_permutation,
          rep(0, times = center)
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

    add_lm = function(linear_model) {
      private$linear_model_storage$add_object(linear_model)
    },

    get_default_formula = function() {
      paste0(
        private$names$response(), "~",
        private$factor_nodes$children()[[1]]$get_object()$get_name()
      )
    },

    get_id = function() {
      private$names$name()
    },

    get_lm = function(id) {
      private$linear_model_storage$get_object(id)
    },

    get_lm_ids = function() {
      private$linear_model_storage$get_ids()
    },

    get_lm_storage = function() {
      private$linear_model_storage
    },

    get_name = function() {
      private$names$name()
    },

    get_fac_design_name = function() {
      private$names$name()
    },

    get_factor_nodes = function() {
      private$factor_nodes
    },

    get_index_name = function() {
      private$names$index()
    },

    get_response_name = function() {
      private$names$response()
    },

    get_table = function(
      randomized = TRUE, index = TRUE, factors = TRUE, response = TRUE,
      center = FALSE
    ) {
      fac_design_table <- private$fac_design_table()

      factor_names <- purrr::map_chr(private$factor_nodes$children(), function(node) {
        node$get_object()$get_name()
      })

      names(fac_design_table) <- c(
        private$names$index(), factor_names,
        private$names$response()
      )
      return_names <- character()
      if (index) {
        return_names <- c(return_names, private$names$index())
      }
      if (factors) {
        return_names <- c(return_names, private$factor_storage$get_ids())
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

    remove_lm = function(id) {
      private$linear_model_storage$remove_object(id)
    },

    rename_fac_names = function(new_names, old_names = NULL) {
      if (is.null(old_names)) {
        stopifnot(
          length(new_names) == length(private$factor_node$children()),
          length(unique(new_names)) == length(new_names)
        )
        purrr::walk2(
          private$factor_nodes$children(),
          new_names,
          function(node, name) {
            node$get_object()$set_name(name)
          }
        )
      } else {
        # REWRITE FOR FACTOR_STORAGE
        # stopifnot(all(old_names %in% private$names$fac()))
        # fac_names <- private$names$fac()
        # names(fac_names) <- fac_names
        # fac_names[old_names] <- new_names
        # names(fac_names) <- NULL
        # private$names$fac(fac_names)
      }
    },

    rename_index_name = function(new_name) {
      private$names$index(new_name)
    },

    rename_name = function(new_name) {
      private$names$name(new_name)
      private$names$name(new_name)
    },

    rename_response_name = function(new_name) {
      private$names$response(new_name)
    }
  ),
  private = list(
    fac_design_table = NULL,
    factor_nodes = NULL,
    .has_response = NULL,
    names = list(
      index = NULL,
      name = NULL,
      response = NULL
    ),
    .nrow = NULL,
    linear_model_storage = NULL
  )
)
