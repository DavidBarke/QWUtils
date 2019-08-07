#' @export
`%_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

#' Turn the first letter of a string in upper case
#'
#' @param x A character vector.
#'
#' @export
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Handle NULL with fallback value
#'
#' This function is especially useful in \code{\link[shiny]{renderUI}} when an
#' input value has not yet been initialised
#'
#' @param x Possible NULL value
#' @param y Fallback value
#'
#' @export
fallback <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

#' Map value from a previous to a new range
#'
#' @param x Numeric value.
#' @param pmin Previous minimum.
#' @param pmax Previous maximum.
#' @param nmin New minimum.
#' @param nmax New maximum.
#'
#' @export
maprange <- function(x, pmin, pmax, nmin, nmax) {
  nmin + ((x - pmin) / (pmax - pmin)) * (nmax - nmin)
}

#' Handle a value that might be a function
#'
#' If \code{x} is a function, it is called with no arguments, otherwise \code{x}
#' will be returned. This function is useful for shiny modules that accept
#' either reactive or constant input.
#'
#' @param x Possible function.
#' @export
handle_fun <- function(x) {
  if (is.function(x)) {
    return(x())
  } else {
    return(x)
  }
}

#' Guarantee that attribute has the same length as the object.
#'
#' @param x An \code{R} object.
#' @param name The attribute's name.
#' @param value A vector of the same length as x, or \code{NULL}.
#'
#' @export
`length_attr<-` <- function(x, name, value) {
  len <- length(x)

  if (!(length(value) %in% c(1, len) || is.null(value))) {
    stop(paste0("Value has to be NULL or of length 1 or length ", len, "."))
  }

  # "Recycling"
  if (length(value) == 1) {
    value <- rep(value, times = len)
  }

  attr(x, name) <- value

  x
}

#' Convert string to numeric
#'
#' Convert a string consisting of numbers seperated by \code{,} to a numeric vector.
#'
#' @param x Character.
#'
#' @export
vector_text_to_numeric <- function(x) {
  as.numeric(unlist(str_split(x, ",")))
}

#' Convert numeric to string
#'
#' Convert a numeric vector to a string in which each number is seperated by
#' \code{,}.
#'
#' @param x Numeric vector.
#'
#' @export
numeric_to_vector_text <- function(x) {
  paste(x, collapse = ", ")
}
