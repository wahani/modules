#' Import objects into module
#'
#' @param from a package name
#' @param ... names to import from package
#' @param into the environment where the thing is stored. By default the parent
#'   of the module
#' @param module a module, aka a list, or any named collection
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
#' @rdname import
use <- function(module, into = parent.env(parent.frame())) {
  lapply(names(module), function(x) {
    assign(
      x,
      value = module[[x]],
      envir = as.environment(into)
    )
  })
  invisible(NULL)
}
