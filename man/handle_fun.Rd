% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{handle_fun}
\alias{handle_fun}
\title{Handle a value that might be a function}
\usage{
handle_fun(x)
}
\arguments{
\item{x}{Possible function.}
}
\description{
If \code{x} is a function, it is called with no arguments, otherwise \code{x}
will be returned. This function is useful for shiny modules that accept
either reactive or constant input.
}
