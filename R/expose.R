#' Expose module contents
#'
#' Use \code{expose} to copy the exported member of a module to the calling
#' environment. This is useful for a simple reexport of member functions and
#' generally for object composition.
#'
#' @param module (character | module) a module as file or folder name or a list
#'   representing a module.
#' @param ... (character, or unquoted expression) elements to be exposed.
#'   Defaults to all.
#' @param reInit (logical) whether to re-initialize module. This is only
#'   relevant if a module has \emph{state} which can be changed. This argument
#'   is passed to \link{as.module}.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#'
#' @details You call this function for its side effects. It is a variation of
#'   \link{use} where instead of returning a module as return value, the
#'   elements are copied to the calling environment.
#'
#' @export
#' @examples
#' m1 <- module({
#'   foo <- function() "foo"
#' })
#' m2 <- module({
#'   bar <- function() "bar"
#' })
#' # Now we create a module with 'foo' and 'bar' as member functions.
#' m3 <- module({
#'   expose(m1)
#'   expose(m2)
#' })
#' m3$foo()
#' m3$bar()
expose <- function(module, ..., reInit = TRUE, where = parent.frame()) {
  mc <- match.call(expand.dots = TRUE)
  mc[[1]] <- quote(modules::use)
  module <- eval(mc, where)
  makeAssignment(module, names(module), where)
  invisible(NULL)
}
