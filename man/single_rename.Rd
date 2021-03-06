% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_rename.R
\name{single_rename_ui}
\alias{single_rename_ui}
\alias{single_rename}
\title{Handle the rename of a name}
\usage{
single_rename_ui(id)

single_rename(input, output, session, .values, .parent, .name, .rename,
  .error_controller = NULL, .rename_tooltip = NULL)
}
\arguments{
\item{id}{The module's id.}

\item{input, output, session}{Called by \code{\link[shiny:callModule]{callModule}}.}

\item{.values}{The \code{.values} list.}

\item{.parent}{The parent \code{\link{Node}} object.}

\item{.name}{\code{\link[shiny:reactive]{Reactive}} returning the name to rename.}

\item{.rename}{\code{\link[base:function]{Function}}, which is called when the
user confirms the rename.}

\item{.error_controller}{An \code{\link{ErrorController}} or
\code{\link{ErrorControllerList}}}

\item{.rename_tooltip}{Tooltip for the button, which enables the
rename mode.}
}
\description{
Handle the rename of a name
}
