#' Get the search path of an environment
#'
#' Returns a list with the environments or names of the environments on the
#' search path. These functions are used for testing, use \link{search} instead.
#'
#' @param where (environment)
#'
#' @export
#' @rdname utilityFunctions
#'
#' @examples
#' getSearchPath()
#' getSearchPathNames()
#'
getSearchPath <- function(where = parent.frame()) {
  if (identical(where, emptyenv())) list(where)
  else c(where, Recall(parent.env(where)))
}

#' @export
#' @rdname utilityFunctions
getSearchPathNames <- function(where = parent.frame()) {
  vapply(getSearchPath(where), environmentName, character(1))
}
