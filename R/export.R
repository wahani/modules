#' Export mechanism for modules
#'
#' You can declare exports very much like the export mechanism in R packages: you
#' define which objects from the module you make available to a user. All other
#' objects are kept private, local, to the module.
#'
#' @param ... (character, or unquoted expression) names to export from module. A
#'   character of length 1 with a leading "^" is interpreted as regular
#'   expression.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#'
#' @details A module can have several export declarations, e.g. directly in
#' front of each function definition. That means: exports stack up. When you
#' supply a regular expression, however, only one export pattern should be
#' declared. A regular expression is denoted, as a convention, as character
#' vector of length one with a leading "^".
#'
#' @examples
#' module({
#'   export("foo")
#'   foo <- function() "foo"
#'   bar <- function() "bar"
#' })
#'
#' module({
#'   export("foo")
#'   foo <- function() "foo"
#'   export("bar")
#'   bar <- function() "bar"
#' })
#'
#' module({
#'   export("foo", "bar")
#'   foo <- function() "foo"
#'   bar <- function() "bar"
#' })
#'
#' module({
#'   export("^f.*$")
#'   foo <- function() "foo"
#'   bar <- function() "bar"
#' })
#'
#' @export
export <- function(..., where = parent.frame()) {
  objectsToExport <- deparseEllipsis(match.call(), "where")
  currentExports <- exportGetCurrentValue(where)
  currentExports <- currentExports[currentExports != "^*"]
  assign(exportNameWithinModule(), c(currentExports, objectsToExport),
         envir = where)
  invisible(NULL)
}

exportNameWithinModule <- function() ".__exports__"

exportGetCurrentValue <- function(envir) {
  get(exportNameWithinModule(), envir = envir)
}

exportResolveFinalValue <- function(envir) {
  isRegEx <- function(s) length(s) == 1 && grepl("^\\^", s)
  exports <- exportGetCurrentValue(envir)
  if (isRegEx(exports)) ls(envir, pattern = exports)
  else exports
}

exportExtract2List <- function(envir) {
  objectsToExport <- exportResolveFinalValue(envir)
  module <- as.list(envir)
  if (any(ind <- !objectsToExport %in% names(module))) {
    stop(
      "exports not defined: ",
      paste(objectsToExport[ind], collapse = ", ")
    )
  } else {
    module[objectsToExport]
  }
}
