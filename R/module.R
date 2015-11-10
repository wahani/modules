#' Define Modules in R
#'
#' Use \code{module} to define self contained organizational units. Modules have
#' their own search path. \code{import} can be used to import single objects
#' from a package. \code{use} will attach collections or packages.
#'
#' @param expr an expression.
#' @param parent (environment) the root of the local search path.
#'
#' @rdname module
#' @export
module <- function(expr = {}, parent = if (interactive()) baseenv() else parent.frame()) {

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  expr <- match.call()[[2]]
  module <- ModuleScope(parent = parent)
  module <- evalInModule(module, expr)
  as.list(module)

}

#' @param from (character, or unquoten expression) a package name
#' @param ... (character, or unquoted expression) names to import from package
#' @param into (environment) the environment for imports. Always first on the
#'   search path.
#' @param module (character, list or environment) a module
#' @param where (environment)
#'
#' @rdname module
#'
#' @export
import <- function(from, ..., into = parent.env(parent.frame())) {

  deleteQuotes <- function(x) {
    gsub("\\\"|\\\'", "", x)
  }

  deparseImports <- function(mc) {
    args <- Map(deparse, mc)
    args[[1]] <- NULL
    args$from <- NULL
    args$into <- NULL
    args <- unlist(args)
    deleteQuotes(args)
  }

  deparseFrom <- function(mc) {
    from <- Map(deparse, mc)$from
    deleteQuotes(from)
  }

  objectsToImport <- deparseImports(match.call())
  from <- deparseFrom(match.call())

  lapply(objectsToImport, function(x) {
    delayedAssign(
      x,
      value = getExportedValue(from, x),
      assign.env = as.environment(into)
    )
  })

  invisible(NULL)

}

#' @export
#' @rdname module
use <- function(module, where = parent.frame()) {

  processModule(module, into) %g% standardGeneric("processModule")

  processModule(module ~ character, into ~ environment) %m% {
    do.call(
      import,
      c(list(module), as.list(getNamespaceExports(module)), into = into)
    )
  }

  processModule(module ~ list, into ~ environment) %m% {
    mapply(assign, names(module), module, MoreArgs = list(envir = into))
  }

  parentOfE <- parent.env(where) # E: calling environment
  grannyOfE <- parent.env(parentOfE)
  newGranny <- new.env(parent = grannyOfE)
  parent.env(parentOfE) <- newGranny
  processModule(module, newGranny) # this imports all objects into newGranny
  invisible(NULL)

}
