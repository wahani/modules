defaultParent <- function() {
  as.environment("package:stats")
}

environment : ModuleScope(parent = defaultParent()) %type% {
  # Class to organise inheritance for modules
  # By default the parent is 'package:stats' and by this sets the point
  # for the search path which you do not have to import into a module.
  parent.env(.Object) <- .Object@parent
  .Object
}

show(object ~ ModuleScope) %m% {
  cat("Object of class 'ModuleScope'")
  cat("\n  It organizes the scope of a module.")
  cat("\n\n  Parent of modules scope is:\n  ")
  cat(environmentName(object@parent))
}

ModuleScope : ModuleEnv(parent = ModuleScope()) %type% {
  # This is the class to wrap a module
  .Object
}

show(object ~ ModuleEnv) %m% {
  cat("Object of class 'ModuleEnv'")
}
