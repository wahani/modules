environment : ModuleParent(parent = baseenv()) %type% {
  # This is the type for the initial parent of a module.
  # It knows of some functions:
  parent.env(.Object) <- .Object@parent
  makeDelayedAssignment("modules", "import", into = .Object)
  makeDelayedAssignment("modules", "use", into = .Object)
  makeDelayedAssignment("modules", "expose", into = .Object)
  makeDelayedAssignment("modules", "export", into = .Object)
  .Object
}

environment : ModuleScope(parent ~ ModuleParent) %type% {
  # This is the type to wrap a module. It is the enclosing env of all funs in a
  # module
  parent.env(.Object) <- .Object@parent
  # Here are also the flags. Because of imports it might be hard to find the
  # original name-value for the exports. And to avoid multiple copies it is
  # directly in the module env
  assign(nameExports(), "^*", envir = .Object)
  .Object
}

ModuleConst <- function(expr, topEncl) {
  # expr
  # topEncl: environment

  evalInModule <- function(module, code) {
    eval(code, envir = as.environment(module), enclos = emptyenv())
    module
  }

  getExports <- function(module) {
    exports <- get(nameExports(), envir = module, inherits = TRUE)
    if (length(exports) == 1 && grepl("\\^", exports)) ls(module, pattern = "^*")
    else exports
  }

  wrapModfun <- function(module) {
    # wrap all functions in a module with the class modfun.
    mapInEnv(module, modfun, is.function)
  }

  addModuleConst <- function(module) {
    attr(module, "moduleConst") <- moduleConst
    module
  }

  new <- function() {

    module <- ModuleScope(parent = ModuleParent(topEncl))
    module <- evalInModule(module, expr)
    module <- wrapModfun(module)
    module <- retList("module", public = getExports(module), envir = module)
    addModuleConst(module)

  }

  moduleConst <- stripSelf(retList("ModuleConst", c("new", "expr", "topEncl")))
  moduleConst

}
