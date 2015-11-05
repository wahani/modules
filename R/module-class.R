VIRTUAL : environment : ModuleEnv(parent ~ environment) %type% {
  # Type to organise scope for modules
  .Object@parent <- as.environment("package:stats")
  parent.env(.Object) <- .Object@parent
  .Object
}

ModuleEnv : ModuleScope() %type% {
  # This is the type to wrap a module
  .Object@parent <- ModuleParent()
  parent.env(.Object) <- .Object@parent
  .Object
}

show(object ~ ModuleScope) %m% {
  cat("Object of class 'ModuleScope'")
}

ModuleEnv : ModuleParent() %type% {
  # This is the type for the parent of a module
  # It knows of some functions:
  import("module", "import", into = .Object)
  import("module", "use", into = .Object)
  .Object
}

show(object ~ ModuleParent) %m% {
  cat("Object of class 'ModuleParent'")
  cat("\n  It organizes the search path of a module.")
  cat("\n\n  search path continues with:\n  ")
  cat(environmentName(object@parent))
}
