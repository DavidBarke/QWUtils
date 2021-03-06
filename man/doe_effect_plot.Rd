% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doe_plots.R
\name{doe_effect_plot}
\alias{doe_effect_plot}
\title{Effect Plot}
\usage{
doe_effect_plot(fac_design, factor_ids = NULL)
}
\arguments{
\item{fac_design}{A \code{\link{FacDesign}} object.}

\item{factor_ids}{One or more ids of
\code{fac_design$get_factor_storage()$get_ids()}. If \code{\link[base]{NULL}},
all factors are selected.}
}
\description{
Visualize main effects of a full factorial design.
}
