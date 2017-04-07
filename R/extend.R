#' @export
#' @rdname module
extend <- function(module, with) {
  extendCheckPrerequisites(module)
  originalExpr <- deparse(attr(module, "expr"))
  additionalExpr <- deparse(match.call()$with)
  newExpr <- parse(text = c("{", originalExpr, additionalExpr, "}"))[[1]]
  ModuleConst(newExpr, attr(module, "topEncl"))
}

extendCheckPrerequisites <- function(module) {
  hasAttributes <- c("expr", "topEncl") %in% names(attributes(module))
  stopifnot(all(hasAttributes))
}

extendWipeSrcMemory <- function(module) {
  expr <- attr(module, "expr")
  attributes(expr) <- NULL
  expr
}
