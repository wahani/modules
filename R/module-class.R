VIRTUAL : environment : ModuleEnv() %type% {
  # Type to organise scope for modules
  parent.env(.Object) <- baseenv()
  .Object
}

ModuleEnv : ModuleScope() %type% {
  # This is the type to wrap a module. It is the enclosing env of all funs in a
  # module
  parent.env(.Object) <- ModuleParent()
  .Object
}

ModuleEnv : ModuleParent() %type% {
  # This is the type for the parent of a module. This is also the env into which
  # dependencies are imported.
  # It knows of some functions:
  import("module", "import", into = .Object)
  import("module", "use", into = .Object)
  .Object
}
