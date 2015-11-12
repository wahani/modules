#' Define Modules in R
#'
#' Use \code{module} to define self contained organizational units. Modules have
#' their own search path. \code{import} can be used to import single objects
#' from a package. \code{use} will attach collections or packages.
#'
#' @param expr an expression.
#' @param topEncl (environment) the root of the local search path.
#'
#' @rdname module
#' @export
module <- function(expr = {}, topEncl = if (interactive()) baseenv() else parent.frame()) {

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  getExports <- function(module) {
    exports <- get(nameExports(), envir = module, inherits = TRUE)
    paste0(exports, collapse = "|")
  }

  expr <- match.call()[[2]]
  module <- ModuleScope(parent = topEncl)
  module <- evalInModule(module, expr)
  stripSelf(retList(
    public = ls(module, pattern = getExports(module)),
    envir = module
  ))

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
use <- function(module, where = parent.env(parent.frame())) {

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

  grannyOfE <- parent.env(where) # E: calling env
  newGranny <- new.env(parent = grannyOfE)
  parent.env(where) <- newGranny
  processModule(module, newGranny) # this imports all objects into newGranny
  invisible(NULL)

}

#' @export
#' @rdname module
export <- function(..., where = parent.env(parent.frame())) {

  deparseExports <- function(mc) {
    args <- Map(deparse, mc)
    args[[1]] <- NULL
    args$where <- NULL
    args <- unlist(args)
    deleteQuotes(args)
  }

  objectsToExport <- deparseExports(match.call())
  assign(nameExports(), objectsToExport, envir = where)

  invisible(NULL)

}
