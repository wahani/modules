#' module
#' @param expr an expression.
#' @rdname module
#' @export
module <- function(expr = {}) {

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  expr <- match.call()[[2]]
  module <- ModuleScope()
  module <- evalInModule(module, expr)
  as.list(module)

}
