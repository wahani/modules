#' @rdname module
#' @export
import <- function(from, ..., attach = TRUE, where = parent.frame()) {
  where <- importCheckAttach(where, attach)
  pkg <- importGetPkgName(match.call())
  objectsToImport <- importGetSelection(match.call(), pkg)
  addDependency(pkg, objectsToImport, where, makeDelayedAssignment, pkg)
  invisible(parent.env(where))
}

importCheckAttach <- function(where, attach) {
  if (!attach) new.env(parent = baseenv()) else where
}

importGetPkgName <- function(mc) {
  pkg <- Map(deparse, mc)$from
  pkg <- deleteQuotes(pkg)
  importCheckInstall(pkg)
}

importCheckInstall <- function(pkg) {
  ind <- !is.element(pkg, installed.packages()[, "Package"])
  if (ind) stop(
    "'package:", pkg, "' is not installed!"
  ) else pkg
}

importGetSelection <- function(mc, pkg) {
  objectsToImport <- importDeparseEllipses(mc)
  if (length(objectsToImport) == 0) getNamespaceExports(pkg)
  else objectsToImport
}

importDeparseEllipses <- function(mc) {
  args <- Map(deparse, mc)
  args[[1]] <- NULL
  args$from <- NULL
  args$where <- NULL
  args$attach <- NULL
  args <- unlist(args)
  deleteQuotes(args)
}
