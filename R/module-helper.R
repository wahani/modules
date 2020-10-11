# helper:

deparseEllipsis <- function(mc, exclude) {
  args <- Map(deparse, mc)
  args[[1]] <- NULL
  args[exclude] <- NULL
  args <- unlist(args)
  deleteQuotes(args)
}

deleteQuotes <- function(x) {
  gsub("\\\"|\\\'", "", x)
}

addDependency <- function(from, what, where, assignFun, name) {
  # add new dependencies to an existing search path
  #
  # from (list | env | pkg) a collection which is subset-able with [
  # what (character) names of values in from
  # where (environment) where the search path begins
  # assignFun (function) how to put 'from::what' 'into'
  # name (character) the name on the search path

  addPrefix <- function(name) paste0("modules:", name)

  addDependencyLayer <- function(where, name) {
    parentOfWhere <- parent.env(where)
    newParent <- new.env(parent = parentOfWhere)
    attr(newParent, "name") <- addPrefix(name)
    parent.env(where) <- newParent
    newParent
  }

  cleanSearchPath <- function(where, name) {
    sp <- getSearchPath(where)
    pos <- Position(
      function(el) identical(el, addPrefix(name)),
      lapply(sp, attr, "name")
    )
    if (is.na(pos)) return(NULL) # stop here
    else {
      packageStartupMessage(
        "Replacing attached import/use on search path for: ",
        addPrefix(name), ".")
      if (identical(globalenv(), where)) {
        detach(pos = pos)
      } else {
        if (pos == 1) parent.env(where) <- sp[[2]]
        else parent.env(sp[[pos - 1]]) <- sp[[pos + 1]]
      }
    }
  }

  messageDuplicates <- function(into) {
    duplicates <- getSearchPathDuplicates(into)
    if (length(duplicates) == 0) return(NULL)
    msg <- sprintf(
      "Masking (%s):\n%s",
      environmentName(into),
      paste(collapse = "\n", paste0(
        "  `", names(duplicates), "` ",
        "from: ", unlist(lapply(duplicates, paste, collapse = ", "))
      ))
    )
    packageStartupMessage(msg)
  }

  cleanSearchPath(where, name)
  # into is a reference to the (new) parent of where:
  into <- addDependencyLayer(where, name)
  res <- assignFun(from, what, into)
  messageDuplicates(into)
  res

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
