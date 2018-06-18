#' @export
#' @rdname module
use <- function(module, ..., attach = FALSE, reInit = TRUE, where = parent.frame()) {

  moduleName <- as.character(substitute(module))
  module <- useTryFindModule(module, moduleName, where, match.call())
  name <- if (is.character(module)) module else moduleName
  module <- as.module(module, reInit = reInit, envir = where)
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

useTryFindModule <- function(module, moduleName, envir, mc) {
  m <- try(module, TRUE)
  if (is.error(m)) {
    m1 <- try(
      eval(mc$module, get(useTopenvNameWithinModule(), envir = envir)), TRUE)
    if (is.error(m1)) stop(simpleError(useGetErrorMessage(m), mc))
    else m <- m1
  }
  m
}

is.error <- function(x) {
  inherits(x, "try-error")
}

useGetErrorMessage <- function(x) {
  attributes(x)$condition$message
}

useGetSelection <- function(module, mc) {
  namesToImport <- deparseEllipsis(mc, c("module", "attach", "reInit", "where"))
  if (length(namesToImport) == 0) module
  else module[namesToImport]
}

useTopenvNameWithinModule <- function() {
  ".__topenv__"
}
