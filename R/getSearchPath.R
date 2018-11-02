#' Get the search path of an environment
#'
#' Returns a list with the environments or names of the environments on the
#' search path. These functions are used for testing, use \link{search} instead.
#'
#' @param where (environment | module | function) the object for the search path
#'   should be investigated. If we supply a list with functions (e.g. a module),
#'   the environment of the first function in that list is used.
#'
#' @export
#' @rdname utilityFunctions
#'
#' @examples
#' getSearchPath()
#' getSearchPathNames()
#' getSearchPathContent()
#'
#' m <- module({
#'   export("foo")
#'   import("stats", "median")
#'   foo <- function() "foo"
#'   bar <- function() "bar"
#' })
#'
#' getSearchPathContent(m)
#'
getSearchPath <- function(where = parent.frame()) {
  if (is.function(where)) where <- environment(where)
  if (listWithFunction(where)) where <- environment(getFirstFunction(where))
  stopifnot(is.environment(where))
  if (identical(where, emptyenv())) list(where)
  else c(where, Recall(parent.env(where)))
}

#' @export
#' @rdname utilityFunctions
getSearchPathNames <- function(where = parent.frame()) {
  vapply(getSearchPath(where), environmentName, character(1))
}

#' @export
#' @rdname utilityFunctions
getSearchPathContent <- function(where = parent.frame()) {
  out <- lapply(getSearchPath(where), function(x) ls(envir = x))
  names(out) <- getSearchPathNames(where)
  class(out, c("SearchPathContent"))
}

#' @export
print.SearchPathContent <- function(x, ...) {
  str(x)
}

listWithFunction <- function(x) {
  if (!is.list(x)) return(FALSE)
  any(vapply(x, is.function, logical(1)))
}

getFirstFunction <- function(x) {
  Filter(is.function, x)[[1]]
}

#' @export
#' @rdname utilityFunctions
getSearchPathDuplicates <- function(where = parent.frame()) {
  sp <- getSearchPathContent(where)
  localNames <- sp[[1]]
  spNames <- unlist(sp[-1])
  duplicates <- localNames[is.element(localNames, spNames)]
  out <- lapply(duplicates, function(d) {
    names(sp[-1])[unlist(lapply(sp[-1], is.element, el = d))]
  })
  names(out) <- duplicates
  out
}
