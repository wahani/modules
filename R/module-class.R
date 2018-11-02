ModuleParent <- function(parent = baseenv()) {
  obj <- new.env(parent = parent)
  attr(obj, "name") <- "modules:internals"
  # The following objects are made available by default. They can be masked by
  # other imports.
  makeDelayedAssignment("modules", "depend", into = obj)
  makeDelayedAssignment("modules", "export", into = obj)
  makeDelayedAssignment("modules", "expose", into = obj)
  makeDelayedAssignment("modules", "import", into = obj)
  assign("attach", lattach, envir = obj)
  assign("library", llibrary, envir = obj)
  assign("source", lsource, envir = obj)
  makeDelayedAssignment("modules", "module", into = obj)
  makeDelayedAssignment("modules", "use", into = obj)
  obj
}

ModuleScope <- function(parent = ModuleParent(), topenv) {
  # This is the type to wrap a module. It is the enclosing env of all funs in a
  # module
  obj <- new.env(parent = parent)
  attr(obj, "name") <- "modules:root"
  # Here are also the flags. Because of imports it might be hard to find the
  # original name-value for the exports.
  assign(exportNameWithinModule(), "^*", envir = obj)
  assign(useTopenvNameWithinModule(), topenv, envir = obj)
  obj
}

ModuleConst <- function(expr, topEncl, topenv) {

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  addMetaData <- function(module) {
    # This adds attributes to give each new module the necessary
    # information to construct a sibling
    attr(module, "expr") <- expr
    attr(module, "topEncl") <- topEncl
    attr(module, "topenv") <- topenv
    module
  }

  module <- ModuleScope(parent = ModuleParent(topEncl), topenv = topenv)
  module <- evalInModule(module, expr)
  module <- exportExtract2List(module)
  module <- class(module, "module")
  addMetaData(module)

}
