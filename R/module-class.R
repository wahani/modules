ModuleParent <- function(parent = baseenv()) {

  obj <- new.env(parent = parent)

  makeDelayedAssignment("modules", "import", into = obj)
  makeDelayedAssignment("modules", "use", into = obj)
  makeDelayedAssignment("modules", "expose", into = obj)
  makeDelayedAssignment("modules", "export", into = obj)
  
  obj
  
}

ModuleScope <- function(parent = ModuleParent()) {
  # This is the type to wrap a module. It is the enclosing env of all funs in a
  # module
  obj <- new.env(parent = parent)
  # Here are also the flags. Because of imports it might be hard to find the
  # original name-value for the exports. And to avoid multiple copies it is
  # directly in the module env. The value can be changed by 'expose'.
  assign(exportNameWithinModule(), "^*", envir = obj)
  obj
}

ModuleConst <- function(expr, topEncl) {
  # expr
  # topEncl: environment

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  addMetaData <- function(module) {
    # This adds attributes to give each new module the necessary
    # information to construct a sibling
    attr(module, "expr") <- expr
    attr(module, "topEncl") <- topEncl
    module
  }

  new <- function() {
    
    module <- ModuleScope(parent = ModuleParent(topEncl))
    module <- evalInModule(module, expr)
    module <- exportExtract2List(module)
    module <- class(module, "module")
    addMetaData(module)

  }

  retList("ModuleConst", "new")

}
