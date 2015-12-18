#' Define Modules in R
#'
#' Use \code{module} to define self contained organizational units. Modules have
#' their own search path. \code{import} can be used to import packages.
#' \code{use} can be used to import other modules.
#'
#' @param expr an expression
#' @param topEncl (environment) the root of the local search path
#'
#' @examples
#' \dontrun{
#' vignette("modulesInR", "module")
#' }
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
    if (length(exports) == 1 && grepl("\\^", exports)) ls(module, pattern = "^*")
    else exports
  }

  expr <- match.call()[[2]]
  module <- ModuleScope(parent = ModuleParent(topEncl))
  module <- evalInModule(module, expr)
  # browser()
  stripSelf(retList(
    public = getExports(module),
    envir = module
  ))

}

#' @param from (character, or unquoten expression) a package name
#' @param ... (character, or unquoted expression) names to import from package
#' @param where (environment) important for testing
#'
#' @rdname module
#'
#' @export
import <- function(from, ..., where = parent.frame()) {

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

  makeObjectsToImport <- function(mc, from) {
    objectsToImport <- deparseImports(mc)
    if (length(objectsToImport) == 0) getNamespaceExports(from)
    else objectsToImport
  }

  deparseFrom <- function(mc) {
    from <- Map(deparse, mc)$from
    deleteQuotes(from)
  }

  from <- deparseFrom(match.call())
  objectsToImport <- makeObjectsToImport(match.call(), from)

  addDependency(makeDelayedAssignment)(from, objectsToImport, where)

  invisible(NULL)

}

#' @param module (character | list) a module as filename or object
#' @param attach (logical) whether to attach the module to the search path
#'
#' @export
#' @rdname module
use <- function(module, attach = FALSE, where = parent.frame()) {
  module <- as.module(module)
  if (attach) addDependency(makeAssignment)(module, names(module), where)
  invisible(module)
}

#' @export
#' @rdname module
export <- function(..., where = parent.frame()) {

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
