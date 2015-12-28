# helper:

mapInEnv <- function(x, f, p, ...) {
  env <- as.environment(x)
  ind <- unlist(eapply(env, p))
  objToBeChanged <- ls(x, sorted = FALSE)[ind]
  obj <- mget(objToBeChanged, env, "any")
  mapply(assign, x = objToBeChanged, value = lapply(obj, f),
         MoreArgs = list(envir = env))
  x
}

deleteQuotes <- function(x) {
  gsub("\\\"|\\\'", "", x)
}

nameExports <- function() ".__exports__"

addDependency <- function(from, what, where, assignFun, name) {
  # add new dependencies to an existing search path

  addPrefix <- function(name) paste0("modules:", name)

  addDependencyLayer <- function(where, from, name) {
    parentOfWhere <- parent.env(where)
    newParent <- new.env(parent = parentOfWhere)
    attr(newParent, "name") <- addPrefix(name)
    parent.env(where) <- newParent
    newParent
  }

  cleanSearchPath <- function(where, name) {

    sp <- getSearchPath(where)
    pos <- Position(function(el) identical(el, addPrefix(name)), lapply(sp, attr, "name"))

    if (is.na(pos)) return(NULL) # stop here
    else if (pos == 1) parent.env(where) <- sp[[2]]
    else parent.env(sp[[pos - 1]]) <- sp[[pos + 1]]

  }

  cleanSearchPath(where, name)
  # into is a reference to the (new) parent of where:
  into <- addDependencyLayer(where, from, name)
  assignFun(from, what, into)

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
  mapply(assign, what, from[what], MoreArgs = list(envir = into))
}
