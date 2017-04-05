#' @export
#' @rdname module
export <- function(..., where = parent.frame()) {
  objectsToExport <- deparseEllipsis(match.call(), "where")
  currentExports <- exportGetCurrentValue(where)
  currentExports <- currentExports[currentExports != "^*"]
  assign(exportNameWithinModule(), c(currentExports, objectsToExport), envir = where)
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
  as.list(envir)[exportResolveFinalValue(envir)]
}
