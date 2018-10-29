#' Define Augmented and Parameterized Modules
#'
#' \code{amodule} is a wrapper around \link{module} and changes the default
#' environment to which the module connects. In contrast to \code{module}
#' the top enclosing environment here is always \code{baseenv}. The second
#' important difference is that the environment in which a module is created has
#' meaning: all objects are made available to the module scope. This is
#' what is meant by \emph{augmented} or \emph{parameterized}. Best practice for
#' the use of this behavior is to return these modules from functions.
#'
#' @param expr (expression) a module declaration, same as \link{module}
#' @param envir (environment) environment used to detect 'parameters'
#' @param enclos (environment) the top enclosing environment of the module
#'   scope.
#' @param class (character) the module can have a class attribute for
#'   consistency. If you rely on S3 dispatch, e.g. to override the default print
#'   method, you should set this value explicitly.
#'
#' @examples
#' Constructor <- function(dependency) {
#'   amodule({
#'     fun <- function(...) dependency(...)
#'   })
#' }
#' instance <- Constructor(identity)
#' instance$fun(1)
#'
#' @export
amodule <- function(expr = {},
                    envir = parent.frame(), enclos = baseenv(),
                    class = NULL) {
  mc <- match.call()
  mc[[1]] <- quote(modules::module)
  mc$class <- NULL
  mc$topEncl <- quote(topEncl)
  mc$envir <- quote(envir)
  topEncl <- list2env(as.list(envir), parent = enclos)
  obj <- eval(mc)
  class(obj) <- c(class, class(obj))
  obj
}
