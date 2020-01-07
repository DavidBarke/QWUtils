#' Compute control line values
#'
#' @param data A \code{\link[tibble]{tibble}} with numeric columns \code{sample} and
#' \code{value}. All samples should be of the same size. Else the sample size is
#' determined by calculating the mean sample size.
#' @param quantile A numeric vector representing the quantiles for which the
#' control line values are calculated. If \code{type \%in\% c("xbar_R", "R")}
#' quantiles must be in \code{c(-3, 0, 3)}. If \code{type == "R"}, length of
#' quantile must be 1.
#' @param type Control chart's type.
#'
#' @export
control_line_value <- function(
  data, quantile = c(-3, 0, 3), type = c("xbar_R", "xbar_s", "R", "s", "p")
) {
  type <- match.arg(type)

  params <- control_chart_params(data, type)

  if (type %in% c("xbar_s", "s", "p")) {
    value <- switch(
      type,
      "xbar_s" = {
        params$x_bar_bar + quantile * params$s_bar /
          (c4(params$n) * sqrt(params$n))
      },
      "s" = {
        c4_n <- c4(params$n)
        params$s_bar + quantile * params$s_bar * sqrt(1 - c4_n^2) / c4_n
      },
      "p" = {
        params$p_bar + quantile * sqrt(
          params$p_bar * (1 - params$p_bar) / params$n
        )
      }
    )
  } else {
    if (!all(quantile %in% c(-3, 0, 3))) {
      stop("ControlChartControlLine: quantile has to be in c(-3, 0, 3)")
    }
    value <- switch(
      type,
      "xbar_R" = {
        quantile <- QWUtils::maprange(quantile, -3, 3, -1, 1)
        params$x_bar_bar + quantile * A2(params$n) * params$R_bar
      },
      "R" = {
        stopifnot(length(quantile) == 1)
        factor <- switch(
          as.character(quantile),
          "-3" = D3(params$n),
          "0" = 1,
          "3" = D4(params$n)
        )
        factor * params$R_bar
      }
    )
  }

  value
}
