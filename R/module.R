#' Define Modules in R
#'
#' Use \code{module} to define self contained organizational units. Modules have
#' their own search path. \code{import} can be used to import packages.
#' \code{use} can be used to import other modules.
#'
#' @param expr an expression
#' @param topEncl (environment) the root of the local search path
#' @param from (character, or unquoted expression) a package name
#' @param ... (character, or unquoted expression) names to import from package
#'   or names to export from module. For exports a character of length 1 with a
#'   leading "^" is interpreted as regular expression.
#' @param where (environment) important for testing
#' @param module (character | list) a module as file- or folder-name or a list
#'   representing a module.
#' @param attach (logical) whether to attach the module to the search path
#' @param x a module
#'
#' @details
#' \code{topEncl} is the environment where the search of the module ends. This
#' is  (most of the time) the base package. When
#' \code{identical(topenv(parent.frame()), globalenv())} is false it (most
#' likely) means that the module is part of a package. In that case the module
#' defines a sub unit within a package but has access to the packages namespace.
#' This is only relevant if you use the function module explicitly. Most likely
#' you will instead use the function 'use' or 'as.module' instead, where the top
#' enclosing environment is always base.
#'
#' \code{import} and \code{use} are no replacements for \link{library} and
#' \link{attach}. Both will work when called in the \code{.GlobalEnv} but should
#' only be used for development and debugging of modules.
#'
#' \code{export} will never export a function with a leading "." in its name.
#'
#' @examples
#' \dontrun{
#' vignette("modulesInR", "modules")
#' }
#'
#' @rdname module
#' @export
module <- function(expr = {}, topEncl = if (identical(topenv(parent.frame()), globalenv())) baseenv() else parent.frame()) {

  ModuleConst(match.call()$expr, topEncl)$new()

}

#' @export
#' @rdname module
print.module <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(names(x)[i], ":\n", attr(x[[i]], "formals"), sep = "")
    cat("\n\n")
  }
  invisible(x)
}

#' @rdname module
#' @export
import <- function(from, ..., where = parent.frame()) {

  deparseImports <- function(mc) {
    args <- Map(deparse, mc)
    args[[1]] <- NULL
    args$from <- NULL
    args$where <- NULL
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

  addDependency(from, objectsToImport, where, makeDelayedAssignment, from)

  invisible(NULL)

}

#' @export
#' @rdname module
use <- function(module, attach = FALSE, ..., where = parent.frame()) {
  name <- if (is.character(module)) module else as.character(substitute(module))
  module <- as.module(module, ...)
  if (attach) addDependency(
    module,
    names(module),
    where,
    makeAssignment,
    name
  )
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
