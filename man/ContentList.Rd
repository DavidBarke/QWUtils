% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContentList.R
\name{ContentList}
\alias{ContentList}
\title{Content List}
\description{
\code{\link[R6:R6Class]{R6Class}} for storing content in a list. Note that
\code{this$container()} and \code{this$set_session(session)} must be called
in order for the content list to work properly.
}
\section{Usage}{

\preformatted{content_list <- ContentList$new("content")
}
}

\section{Methods}{

\describe{
  \item{\code{new(id, sortable = FALSE)}}{Initialize the content list.
    \tabular{ll}{
      \code{id} \tab \code{\link[base:character]{Character}}. The id of the
      content list's container. If the session associated with the content
      list is a session proxy (session inside of a module), the id gets
      namespaced. \cr
      \code{sortable} \tab Currently defunct.
    }
  }
  \item{\code{container()}}{Call this function in the UI definition. This
  function returns the div-element containing the content list.
  }
  \item{\code{set_session(session)}}{Call this function in the server function
  to connect the content list with a session.
    \tabular{ll}{
      \code{session} \tab A shiny \code{\link[shiny:session]{session}} object.
    }
  }
  \item{\code{add_content_element(content_element, hidden = TRUE)}}{Add a
  content element to the content list.
    \tabular{ll}{
      \code{content_element} \tab A content element object. \cr
      \code{hidden} \tab If \code{\link[base:logical]{TRUE}}, this content
        element starts in a hidden state. Use \code{this$show_content_element()}
        to show it.
    }
  }
  \item{\code{append_tab(content_element_id, tab, select = FALSE,
    closeable = FALSE)}}{Append a tab to the content element with
    \code{id == content_element_id}.
      \tabular{ll}{
        \code{content_element_id} \tab \code{\link[base:character]{Character}}.
        The id of the content element. \cr
        \code{tab} \tab The item to be added (must be created with
        \code{\link[shiny:tabPanel]{tabPanel}}). \cr
        \code{select} \tab If \code{\link[base:logical]{TRUE}} \code{tab} gets
        selected upon being inserted. \cr
        \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, tabPanel is
        closeable via an button with cross icon next to the tabPanel's title.
      }
  }
  \item{\code{get_content_element(content_element_id)}}{Get the content
  element with \code{id == content_element_id}.
  }
  \item{\code{get_content_element_ids()}}{Get the ids of the content elements
  as a \code{\link[base:character]{character}} vector.
  }
  \item{\code{hide_content_element(content_element_id)}}{Hide the content
  element with \code{id == content_element_id}.
  }
  \item{\code{remove_tab(content_element_id, target)}}{Dynamically remove a
    \code{\link[shiny]{tabPanel}} from an existing content element.
   \tabular{ll}{
     \code{content_element_id} \tab Id of content element. \cr
     \code{target} \tab The \code{value} of the \code{tabPanel} to be removed.
   }
  }
  \item{\code{show_content_element(content_element_id)}}{Show the content element
  with \code{id == content_element_id}.
  }
  \item{\code{update_tab(content_element_id, selected)}}{Update the selected
  tab of the content element with \code{id == content_element_id}}.
}
}

