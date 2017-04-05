#' @export
#' @rdname module
expose <- function(module, ..., reInit = TRUE, where = parent.frame()) {
  mc <- match.call(expand.dots = TRUE)
  mc[[1]] <- quote(modules::use)
  module <- eval(mc, where)
  makeAssignment(module, names(module), where)
  invisible(NULL)
}
