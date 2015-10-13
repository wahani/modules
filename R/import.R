importFromPackage <- function(from, ..., into = parent.env(parent.frame())) {

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
