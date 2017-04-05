#' @export
#' @rdname module
use <- function(module, ..., attach = FALSE, reInit = TRUE, where = parent.frame()) {

  name <- if (is.character(module)) module else as.character(substitute(module))
  module <- as.module(module, reInit = reInit)
  module <- useGetSelection(module, match.call(expand.dots = TRUE))

  if (attach) addDependency(
    module,
    names(module),
    where,
    makeAssignment,
    name
  )

  invisible(module)

}

useGetSelection <- function(module, mc) {
  namesToImport <- deparseEllipsis(mc, c("module", "attach", "reInit", "where"))
  if (length(namesToImport) == 0) module
  else module[namesToImport]
}
