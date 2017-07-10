#' Define Modules in R
#'
#' Use \code{module} to define self contained organisational units. Modules have
#' their own search path. \code{import} can be used to import packages.
#' \code{use} can be used to import other modules. Use \code{export} to define
#' which objects to export. \code{expose} can be used to reuse function
#' definitions from another module.
#'
#' @param expr,with an expression
#' @param topEncl (environment) the root of the local search path. It is tried
#'   to find a good default via \link{autoTopEncl}.
#' @param from (character, or unquoted expression) a package name
#' @param ... (character, or unquoted expression) names to import from package
#'   or names to export from module. For exports a character of length 1 with a
#'   leading "^" is interpreted as regular expression.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#' @param module (character | module) a module as file or folder name or a list
#'   representing a module.
#' @param attach (logical) whether to attach the module to the search path
#' @param reInit (logical) whether to re-initialize module. This is only
#'   relevant if a module has \emph{state} which can be changed. This argument
#'   is passed to \link{as.module}.
#'
#' @details
#' \code{topEncl} is the environment where the search of the module ends.
#' \code{autoTopEncl} handles the different situations. In general it defaults
#' to the base environment or the environment from which \code{module} has been
#' called. If you are using \code{use} or \code{expose} referring to a module in
#' a file, it will always be the base environment. When
#' \code{identical(topenv(parent.frame()), globalenv())} is false it (most
#' likely) means that the module is part of a package. In that case the module
#' defines a sub unit within a package but has access to the packages namespace.
#' This is relevant when you use the function module explicitly. When you define
#' a nested module the search path connects to the environment of the enclosing
#' module.
#'
#' \code{import} and \code{use} can replace \link{library} and \link{attach}.
#' However they behave differently and are only designed to be used within
#' modules. Both will work when called in the \code{.GlobalEnv} but should only
#' be used for development and debugging of modules.
#'
#' \code{export} will never export a function with a leading "." in its name.
#'
#' \code{expose} is similar to \code{use} but instead of attaching a module it
#' will copy all elements into the calling environment. This means that
#' \emph{exposed} functions can be (re-)exported.
#'
#' \code{extend} can be used to extend an existing module definition. This
#' feature is meant to be used by a module author to split a module definition
#' into different files. You can also refer to a folder name in \code{use} which
#' is interpreted to use all files as sub-modules.
#'
#' @examples
#' \dontrun{
#' vignette("modulesInR", "modules")
#' }
#'
#' m <- module({
#'   fun <- function(x) x
#' })
#'
#' m$fun(1)
#'
#' m <- module({
#'
#'   import("stats", "median")
#'   export("fun")
#'
#'   fun <- function(x) {
#'     ## This is an identity function
#'     ## x (ANY)
#'     x
#'   }
#'
#' })
#'
#' m$fun
#' m
#'
#' @rdname module
#' @export
module <- function(expr = {}, topEncl = autoTopEncl(parent.frame())) {
  ModuleConst(match.call()$expr, topEncl)
}

#' @export
print.module <- function(x, ...) {

  getFormals <- function(fun) {
    formalsOfFun <- formals(fun)
    formalsOfFun[sapply(formalsOfFun, is.character)] <-
      lapply(formalsOfFun[sapply(formalsOfFun, is.character)], function(el){
        paste0("\"", el, "\"")
      })
    args <- ifelse(
      as.character(formalsOfFun) == "",
      names(formalsOfFun),
      paste(names(formalsOfFun), formalsOfFun, sep = " = ")
    )
    paste0("function(", paste(args, collapse = ", "), ")")
  }

  getDoc <- function(fun) {
    sourceOfFun <- stringr::str_trim(attr(fun, "srcref"))
    sourceOfFun <- sourceOfFun[grep("^##", sourceOfFun)]
    paste(sourceOfFun, collapse = "\n")
  }

  catFuns <- function(funs) {
    for (i in seq_along(funs)) {
      cat(names(funs)[i], ":\n", sep = "")
      cat(getFormals(funs[[i]]), "\n", sep = "")
      docString <- getDoc(funs[[i]])
      if (length(docString) > 0) cat(docString, "\n", sep = "")
      cat("\n", sep = "")
    }
  }

  catRemaining <- function(remaining) {
    for (i in seq_along(remaining)) {
      cat(paste0(names(remaining)[i], ":\n"))
      cat(str(remaining[[i]]))
      cat("\n")
    }
  }

  ind <- vapply(x, is.function, logical(1))
  catFuns(x[ind])
  catRemaining(x[!ind])

  invisible(x)

}

#' @export
#' @rdname module
autoTopEncl <- function(where) {
  # if .__exports__ exists I assume it is a nested module:
  if (exists(exportNameWithinModule(), where = where)) where
  else if (identical(topenv(where), globalenv())) baseenv()
  else where
}
