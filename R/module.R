module <- function(expr) {

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  expr <- match.call()[[2]]
  module <- ModuleEnv()
  importFromPackage("module", "importFromPackage", into = module)
  parent.env(as.environment(module))
  evalInModule(module, expr)

}

"%module%" <- function(moduleName, expr, where = parent.frame()) {
  moduleName <- as.character(substitute(moduleName))
  mc <- match.call()
  mc[[1]] <- quote(module)
  mc$moduleName <- NULL
  assign(moduleName, eval(mc, where), envir = where)
}
