% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSearchPath.R
\name{getSearchPath}
\alias{getSearchPath}
\alias{getSearchPathNames}
\alias{getSearchPathContent}
\alias{getSearchPathDuplicates}
\title{Get the search path of an environment}
\usage{
getSearchPath(where = parent.frame())

getSearchPathNames(where = parent.frame())

getSearchPathContent(where = parent.frame())

getSearchPathDuplicates(where = parent.frame())
}
\arguments{
\item{where}{(environment | module | function) the object for the search path
should be investigated. If we supply a list with functions (e.g. a module),
the environment of the first function in that list is used.}
}
\description{
Returns a list with the environments or names of the environments on the
search path. These functions are used for testing, use \link{search} instead.
}
\examples{
getSearchPath()
getSearchPathNames()
getSearchPathContent()

m <- module({
  export("foo")
  import("stats", "median")
  foo <- function() "foo"
  bar <- function() "bar"
})

getSearchPathContent(m)

}
