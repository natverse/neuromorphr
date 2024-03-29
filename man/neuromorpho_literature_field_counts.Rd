% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/literature.R
\name{neuromorpho_literature_field_counts}
\alias{neuromorpho_literature_field_counts}
\title{Get the number of different articles for a neuromorpho literature field}
\usage{
neuromorpho_literature_field_counts(
  field = "tracingSystem",
  neuromorpho_url = "http://neuromorpho.org",
  ...
)
}
\arguments{
\item{field}{a valid neuron field, as returned by \code{neuromorpho_fields}}

\item{neuromorpho_url}{the base URL for querying the neuromorpho database, defaults to \url{http://neuromorpho.org}}

\item{...}{methods passed to \code{neuromorpho_async_req}, or in some cases, \code{neuromorphr:::neuromorpho_fetch}}
}
\description{
Returns a vector of counts for every field entry for a given neuromorpho literature field.
E.g. you can find out how the number of papers that used particular neuron reconstruction softwares.
}
\details{
Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp}.
}
\seealso{
Other literature functions: 
\code{\link{neuromorpho_get_article}()},
\code{\link{neuromorpho_literature_field_entries}()},
\code{\link{neuromorpho_literature_fields}()},
\code{\link{neuromorpho_literature_search}()}
}
\concept{literature functions}
