# helper:

deleteQuotes <- function(x) {
  gsub("\\\"|\\\'", "", x)
}

nameExports <- function() ".__exports__"

addDependency <- function(assignFun) {
  # builds a function to add new dependencies to an existing search path
  force(assignFun)

  addDependencyLayer <- function(where, from) {
    parentOfWhere <- parent.env(where)
    newParent <- new.env(parent = parentOfWhere)
    attr(newParent, "name") <- paste0("import:", if (is.character(from)) from else "module")
    parent.env(where) <- newParent
    newParent
  }

  function(from, what, where) {
    # into is a reference to the (new) parent of where:
    into <- addDependencyLayer(where, from)
    assignFun(from, what, into)
  }

}

makeDelayedAssignment <- function(from, what, into) {
  # from: a package name
  # what: a character vector
  # into: an env
  lapply(what, function(x) {
    delayedAssign(
      x,
      value = getExportedValue(from, x),
      assign.env = as.environment(into)
    )
  })
}

makeAssignment <- function(from, what, into) {
  # from: a list of values
  # what: a character vector
  # into: an env
  mapply(assign, what, from, MoreArgs = list(envir = into))
}
