#' Export mechanism for modules
#'
#' You can declare exports very much like the export mechanism in R packages:
#' you define which objects from the module you make available to a user. All
#' other objects are kept private, local, to the module.
#'
#' @param ... (character, or unquoted expression) names to export from module. A
#'   character of length 1 with a leading "^" is interpreted as regular
#'   expression. Arguments can be named and used for renaming exports.
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
#' module({
#'   export(bar = foo)
#'   foo <- function() "foo"
#' })
#'
#' @export
export <- function(..., where = parent.frame()) {
  exportWarnOnNonStandardCalls(match.call())
  objectsToExport <- deparseEllipsis(match.call(), "where")
  currentExports <- exportGetCurrentValue(where)
  currentExports <- currentExports[currentExports != "^*"]
  assign(exportNameWithinModule(), c(currentExports, objectsToExport),
         envir = where)
  invisible(NULL)
}

exportWarnOnNonStandardCalls <- function(call) {
  # exporting with do.call is not working properly, so we throw a warning, in
  # case we can detect it. Consider the following examples:
  # m <- module({
  #   sm <- module({
  #     x <- 1
  #     fun <- function() x
  #   })
  #   do.call(export, list(fun = sm$fun))
  # })
  # It will not work, although `export(fun = sm$fun)` does work as expected.
  # This is extremely difficult to dubug and it seems to be better to turn it
  # off until someone can fix it.
  if (length(deparse(call[[1]])) > 1) warning(
    "Detected a non standard call to export. The export function relies heavily ",
    "on non standard evaluation and may not work as expected combined with 'do.call' ",
    "or 'lapply'. See the docs and https://github.com/wahani/modules/issues/19 for ",
    "a discussion."
  )
}

exportNameWithinModule <- function() ".__exports__"

exportGetCurrentValue <- function(envir) {
  get(exportNameWithinModule(), envir = envir)
}

exportExtract2List <- function(envir) {
  exports <- exportResolveFinalValue(envir)
  objectsAndNames <- Map(exportExtractElement(envir), exports, names(exports))
  module <- lapply(objectsAndNames, function(x) x$object)
  names(module) <- vapply(objectsAndNames, function(x) x$name, character(1))
  duplicateNames <- names(module)[duplicated(names(module))]
  if (length(duplicateNames) > 0) warning("Found duplicate names in exports!")
  module
}

exportResolveFinalValue <- function(envir) {
  isRegEx <- function(s) length(s) == 1 && grepl("^\\^", s)
  exports <- exportGetCurrentValue(envir)
  if (isRegEx(exports)) exports <- ls(envir, pattern = exports)
  if (is.null(names(exports))) names(exports) <-  rep("", length(exports))
  exports
}

exportExtractElement <- function(where) function(element, name) {
  name <- if (name == "") element else name
  object <- tryCatch(
    eval(parse(text = paste0("`", element, "`")), where, baseenv()),
    error = function(e) stop(call. = FALSE, sprintf(
      "unable to resolve export: %s\nfailed with\n%s", name, e)))
  list(name = name, object = object)
}
