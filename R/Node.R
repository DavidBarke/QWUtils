#' Store the session object of modules
#'
#' Use nodes to store the session objects of all modules in a shiny app in a tree-like
#' data-structure. A node object is an instantiation of the R6Class 'node'.
#'
#' Instantiate a new node with \code{node$new(name, parent, session)}. Passing the
#' arguments \code{name} and \code{session} is mandatory. The root or entry node to
#' a tree obviously has no parent.
#'
#' @section Usage:
#' \preformatted{node <- Node$new(name, parent, session)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name, parent, session)}}{Initialize the node. Arguments
#'   \code{name} and \code{session} are mandatory.
#'     \tabular{ll}{
#'       \code{name} \tab \code{\link[base:character]{Character}}. Node's name. \cr
#'       \code{parent} \tab A \code{Node} object. \cr
#'       \code{session} \tab A \code{\link[shiny:session]{Session}} object.
#'     }
#'   }
#'   \item{\code{add_child(child)}}{Add a child to the node. You usually don't
#'   need to call this function.
#'     \tabular{ll}{
#'        \code{child} \tab A \code{Node} object.
#'     }
#'   }
#'   \item{\code{child(path_to_child)}}{Get a child of the node determined by
#'   the character vector \code{path_to_child}. If there is a node in with the
#'   name of the first element of \code{path_to_child}, \code{child()} looks in
#'   the children of this node for an child node with the name of the second
#'   element of \code{path_to_child} and so on and returns the last child found.
#'   Calling this method without arguments has the same effect as
#'   }
#'   \item{\code{children_names()}}{Returns the names of all children nodes as a
#'   \code{\link[base:character]{character}} vector.
#'   }
#'   \item{\code{create_list()}}{Returns a list representing all child and
#'   child-child nodes.
#'   }
#'   \item{\code{get(what)}}{Returns the private field or method with the name
#'     \code{what}.
#'   }
#'   \item{\code{sibling(name)}}{Returns the sibling node with name \code{name}.
#'   Calling this method without arguments returns the names of all siblings.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#'   # Entry point of shiny app
#'   server <- function(input, output, session) {
#'     # root node has no parent
#'     self <- node$new("root", session = session)
#'
#'     shiny::callModule(module, "id_module", parent = self)
#'   }
#'
#'   # Module server function
#'   module <- function(input, output, session, parent) {
#'     self <- node$new("module", parent, session)
#'
#'     shiny::callModule(module, "id_module", parent = self)
#'   }
#'
#'   module_2 <- function(input, output, session, parent) {
#'     # Children can have the same name as their parent
#'     self <- node$new("module", parent, session)
#'   }
#' }
#'
#' # Instantiate nodes
#' root <- node$new("root", session = "session")
#' branch_1 <- node$new("branch_1", root, "session")
#' branch_2 <- node$new("branch_2", root, "session")
#' leave <- node$new("leave", branch_1, "session")
#'
#' # Returns leave node
#' root$child(c("branch_1", "leave"))
#'
#' # Returns branch_2 node
#' branch_1$sibling(branch_1$sibling()[[1]])
#' leave$get("parent")$sibling("branch_2")
#' leave$get("parent")$get("parent")$child("branch_2")
#'
#' @export
Node <- R6::R6Class(
  "Node",
  public = list(
    initialize = function(name, parent, session) {
      if (missing(name)) stop("argument with name 'name' is missing")
      if (missing(session)) stop("argument with name 'session' is missing")
      if (!missing(parent)) {
        if (!"Node" %in% class(parent)) stop("parent has to be a node object")
        if (name %in% parent$children_names())
          stop(paste0("parent node has already a child with name '", name, "'"))
        private$parent <- parent
        parent$add_child(self)
      }
      private$name <- name
      private$session <- session
    },

    add_child = function(child) {
      private$children <- c(private$children, child)
    },

    children_names = function() {
      names <- character(0)
      for (i in seq_along(private$children)) {
        names[[i]] <- private$children[[i]]$get("name")
      }
      names
    },

    child = function(path_to_child) {
      if (missing(path_to_child)) {
        return(self$children_names())
      }
      stopifnot(is.character(path_to_child))
      if (path_to_child[[1]] %in% self$children_names()) {
        if (length(path_to_child) == 1) {
          return(
            private$children[[
              which(self$children_names() == path_to_child[[1]])
            ]]
          )
        } else {
          return(
            private$children[[
              which(self$children_names() == path_to_child[[1]])
            ]]$child(path_to_child[-1])
          )
        }
      } else {
        stop("There is no node corresponding to the given path_to_child")
      }
    },

    create_list = function() {
      l <- list()
      for (child in private$children) {
        signif_name <- child$get_signif_name()
        n <- sum(str_detect(names(l), paste0(signif_name, "_\\d")))
        new_name <- signif_name %_% n
        l[[new_name]] <- child$create_list()
      }
      return(l)
    },

    get = function(what) {
      private[[what]]
    },

    get_signif_name = function() {
      str_extract(str_extract(private$name, "-?\\w+$"), "\\w+")
    },

    sibling = function(name) {
      if (missing(name)) {
        children_names <- private$parent$children_names()
        return(children_names[-which(children_names == private$name)])
      }
      sibling <- private$parent$child(name)
      if (is.null(sibling))
        stop(paste0("There is no sibling node with name '", name, "'"))
      return(sibling)
    }
  ),
  private = list(
    name = NULL,
    parent = NULL,
    children = NULL,
    session = NULL
  )
)
