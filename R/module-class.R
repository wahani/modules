environment : ModuleParent(parent = baseenv()) %type% {
  # This is the type for the initial parent of a module.
  # It knows of some functions:
  parent.env(.Object) <- .Object@parent
  makeDelayedAssignment("module", "import", into = .Object)
  makeDelayedAssignment("module", "use", into = .Object)
  makeDelayedAssignment("module", "export", into = .Object)
  .Object
}

environment : ModuleScope(parent ~ ModuleParent) %type% {
  # This is the type to wrap a module. It is the enclosing env of all funs in a
  # module
  parent.env(.Object) <- .Object@parent
  # Here are also the flags. Because of imports it might be hard to find the
  # original name-value for the exports. And to avoid multiple copies it is
  # directly in the module env
  assign(nameExports(), "*", envir = .Object)
  .Object
}
