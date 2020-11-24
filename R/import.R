#' Import mechanism for modules
#'
#' You can declare imports similar to what we would do in a R package: we list
#' complete packages or single function names from a package. These listed
#' imports are made available inside the module scope.
#'
#' @param from (character, or unquoted expression) a package name
#' @param ... (character, or unquoted expression) names to import from package.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#' @param attach (logical) whether to attach the imports to the search path.
#'
#' @details
#' \code{import} and \link{use} can replace \link{library} and \link{attach}.
#'   However they behave differently and are only designed to be used within
#'   modules. Both will work when called in the \code{.GlobalEnv} but here they
#'   should only be used for development and debugging of modules.
#'
#' \code{import} adds a layer to a local search path. More precisely to the
#'   calling environment, which is the environment supplied by \code{where}.
#'   It will alter the state of the calling environment. This is very
#'   similar to how the \link{library} function and the \link{search} path are
#'   constructed in base R. Noticeable differences are that we can choose to
#'   only import particular functions instead of complete packages. Further we
#'   do not have to mutate the calling environment by setting attach to
#'   \code{FALSE}. Regardless of the \code{attach} argument, \code{import} will
#'   return an environment with the imports and can be bound to a name.
#'   \link{library} will also load packages in the 'Depends' field of a package,
#'   this is something \code{import} will not do.
#'
#' Only one \code{import} declaration per package is allowed. A second call to
#'   import will remove the previous one from the search path. Then the new
#'   import layer is added. If several smaller import declarations are
#'   desirable, use \code{attach = FALSE} and bind the return value of
#'   \code{import} to a name.
#'
#' @return An \link{environment} is returned invisibly comprising the imports.
#'
#' @export
#' @examples
#' m <- module({
#'   # Single object from package
#'   import("stats", "median")
#'   # Complete package
#'   import("stats")
#'   # Without side-effects
#'   stats <- import("stats", attach = FALSE)
#'   median <- function(x) stats$median(x)
#' })
import <- function(from, ..., attach = TRUE, where = parent.frame()) {
  where <- importCheckAttach(where, attach)
  pkg <- importGetPkgName(match.call())
  objectsToImport <- importGetSelection(match.call(), pkg)
  addDependency(pkg, objectsToImport, where, makeDelayedAssignment, pkg)
  invisible(parent.env(where))
}

#' @export
#' @rdname import
importDefaultPackages <- function(except = NULL, where = parent.frame()) {
  pkgs <- getOption(
    "defaultPackages",
    c("datasets", "utils", "grDevices", "graphics", "stats", "methods"))
  pkgs <- setdiff(pkgs, except)
  for (pkg in pkgs) do.call(modules::import, list(str2lang(pkg)), envir = where)
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
  if (length(objectsToImport) == 0) importGetNamespaceExports(pkg)
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

importGetNamespaceExports <- function(pkg) {
  nsExports <- getNamespaceExports(pkg)
  nsDatasets <- data(package = pkg)
  nsDatasets <- nsDatasets$results[, "Item"]
  nsDatasets <- gsub(" .*", "", nsDatasets)
  c(nsExports, nsDatasets)
}
