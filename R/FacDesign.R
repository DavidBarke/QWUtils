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
#'   \item{\code{new(name, factor_node, rep = 1, center = 0, response_name =
#'   "response", index_name = "index", response = NULL}}{
#'   Initialize the factorial design.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. The factorial
#'       design's name \cr
#'       \code{factor_node} \cr An object of class \code{\link{ExplorerNode}}
#'       with children of explorer_class "fac_design_factor"
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
#'
#' @name FacDesign
NULL

#' @export
FacDesign <- R6::R6Class(
  classname = "FacDesign",
  public = list(
    initialize = function(
      name, factor_node, rep = 1, center = 0,
      response_name = "response", index_name = "index", response = NULL
    ) {
      # factor_node children must not be group nodes
      purrr::walk(factor_node$get_children()$get_objects(), function(node) {
        if (!(node$get_explorer_class_id() == "fac_design_factor")) {
          stop("FacDesign: factor_node's children must be of explorer_class
               'fac_design_factor'")
        }
      })

      private$names <- list(
        index = reactiveVal(index_name),
        name = reactiveVal(name),
        response = reactiveVal(response_name)
      )

      private$center <- center
      private$rep <- rep

      private$factor_node <- factor_node
      private$linear_model_storage <- ObjectStorage$new(
        allowed_classes = "LinearModel"
      )

      if (purrr::is_null(response)) {
        private$.has_response <- reactiveVal(FALSE)
      } else {
        private$.has_response <- reactiveVal(TRUE)
      }

      private$response <- response
    },

    add_response = function(response) {
      private$response <- response
      private$.has_response(TRUE)
    },

    add_lm = function(linear_model) {
      private$linear_model_storage$add_object(linear_model)
    },

    get_default_formula = function() {
      paste0(
        private$names$response(), "~",
        private$factor_node$get_children()[[1]]$get_object()$get_name()
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

    get_factor_ids = function() {
      ids <- purrr::map_chr(private$factor_node$get_children()$get_objects(), function(node) {
        node$get_object()$get_id()
      })

      names(ids) <- self$get_factor_names()

      ids
    },

    get_factor_names = function() {
      purrr::map_chr(private$factor_node$get_children()$get_objects(), function(node) {
        node$get_object()$get_name()
      })
    },

    get_factor_node = function() {
      private$factor_node
    },

    get_factor_storage = function() {
      private$factor_node$get_child_objects()
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

      factor_ids <- self$get_factor_storage()$get_ids()

      names(fac_design_table) <- c(
        private$names$index(), factor_ids,
        private$names$response()
      )
      return_names <- character()
      if (index) {
        return_names <- c(return_names, private$names$index())
      }
      if (factors) {
        return_names <- c(return_names, self$get_factor_storage()$get_ids())
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
      nrow(private$fac_design_table())
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
          private$factor_node$children(),
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
    center = numeric(),
    fac_design_table = function() {
      k <- private$factor_node$get_child_objects()$get_length()

      fac_design_table <- tibble(
        index = seq_len((2^k)*private$rep + private$center)
      )

      for (i in seq_len(k)) {
        fac_permutation <- rep(
          rep(rep(c(-1, 1), each = 2^(k - i)), times = 2^(i - 1)), times = private$rep
        )
        fac_design_table[[i + 1]] <- c(
          fac_permutation,
          rep(0, times = private$center)
        )
      }

      fac_design_table$response <- if (private$.has_response()) {
        private$response
      } else {
        NA
      }

      fac_design_table
    },
    factor_node = NULL,
    .has_response = NULL,
    names = list(
      index = NULL,
      name = NULL,
      response = NULL
    ),
    linear_model_storage = NULL,
    rep = numeric(),
    response = NULL
  )
)
