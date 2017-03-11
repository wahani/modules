#' @export
#' @rdname module
export <- function(..., where = parent.frame()) {
  objectsToExport <- deparseEllipsis(match.call(), "where")
  currentExports <- exportGetValue(where)
  currentExports <- currentExports[currentExports != "^*"]
  assign(exportNameWithinModule(), c(currentExports, objectsToExport), envir = where)
  invisible(NULL)
}

exportNameWithinModule <- function() ".__exports__"

exportGetValue <- function(envir) {
  get(exportNameWithinModule(), envir = envir)
}

exportResolveFinalValue <- function(envir) {
  isRegEx <- function(s) length(s) == 1 && grepl("^\\^", s)
  exports <- exportGetValue(envir)
  if (isRegEx(exports)) ls(envir, pattern = exports)
  else exports
}

exportExtract2List <- function(envir, exports) {
  as.list(envir)[exports]
}
