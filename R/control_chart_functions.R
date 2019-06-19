#' Control Chart Functions
#'
#' \code{A1}, \code{A2}, \code{d2}, \code{D3} and \code{D4} currently use
#' hard-coded values for sample size \code{n = 5}.
#'
#' @param n Sample size.
#'
#' @name control_chart_function
NULL

#' @rdname control_chart_function
A1 <- function(n) {
  # n = 5
  1.596
}

#' @rdname control_chart_function
A2 <- function(n) {
  # n = 5
  .577
}

#' @rdname control_chart_function
A3 <- function(n) {
  3 / (c4(n) * sqrt(n))
}

#' @rdname control_chart_function
B3 <- function(n) {
  val <- 1 - ((3 / c4(n)) * sqrt(1 - c4(n) ^ 2))
  max(val, 0)
}

#' @rdname control_chart_function
B4 <- function(n) {
  1 + ((3 / c4(n)) * sqrt(1 - c4(n) ^ 2))
}

#' @rdname control_chart_function
c4 <- function(n) {
  ((2 / (n - 1)) ^ 0.5) * (gamma(n / 2) / gamma((n - 1) / 2))
}

#' @rdname control_chart_function
d2 <- function(n) {
  # n = 5
  2.326
}

#' @rdname control_chart_function
D3 <- function(n) {
  # n = 5
  0
}

#' @rdname control_chart_function
D4 <- function(n) {
  # n = 5
  2.115
}
