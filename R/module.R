#' Define Modules in R
#'
#' Use \code{module} to define self contained organisational units. Modules have
#' their own search path. \link{import} can be used to import packages.
#' \link{use} can be used to import other modules. Use \link{export} to define
#' which objects to export. \link{expose} can be used to reuse function
#' definitions from another module.
#'
#' @param expr an expression
#' @param topEncl (environment) the root of the local search path. It is tried
#'   to find a good default via \link{autoTopEncl}.
#' @param envir,where (environment) the environment from where \code{module} is
#'   called. Used to determine the top level environment and should not be
#'   supplied by the use.
#'
#' @details
#' \code{topEncl} is the environment where the search of the module ends.
#'   \code{autoTopEncl} handles the different situations. In general it defaults
#'   to the base environment or the environment from which \code{module} has
#'   been called. If you are using \code{use} or \code{expose} referring to a
#'   module in a file, it will always be the base environment. When
#'   \code{identical(topenv(parent.frame()), globalenv())} is false it (most
#'   likely) means that the module is part of a package. In that case the module
#'   defines a sub unit within a package but has access to the packages
#'   namespace. This is relevant when you use the function module explicitly.
#'   When you define a nested module the search path connects to the environment
#'   of the enclosing module.
#'
#' The use of \link{library}, \link{attach}, and \link{source} are discouraged
#'   within modules. They change the global state of an R session, the
#'   \link{.GlobalEnv}, and may not have the intended effect within modules.
#'   \link{import} and \link{use} can replace calls to \link{library} and
#'   \link{attach}. Both will work when called in the \code{.GlobalEnv}
#'   but here they should only be used for development and debugging of modules.
#'   \link{source} often is used to load additional user code into a session.
#'   This is what \link{use} is designed to do within modules. \link{use} will
#'   except files and folders to be used.
#'
#' \link{export} will never export a function with a leading "." in its name.
#'
#' \link{expose} is similar to \link{use} but instead of attaching a module it
#'   will copy all elements into the calling environment. This means that
#'   \emph{exposed} functions can be (re-)exported.
#'
#' \link{extend} can be used to extend an existing module definition. This
#'   feature is meant to be used by the module author. This can be very useful
#'   to write unit tests when they need to have access to private member
#'   functions of the module. It is not safe as a user of a module to use this
#'   feature: it breaks encapsulation. When you are looking for mechanisms for
#'   reuse \link{expose} and \link{use} should be favoured.
#'
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
module <- function(expr = {}, topEncl = autoTopEncl(envir), envir = parent.frame()) {
  ModuleConst(match.call()$expr, topEncl, topenv(envir))
}

#' @export
print.module <- function(x, ...) {

  getFormals <- function(fun) {
    formalsOfFun <- formals(fun)
    formalsOfFun[sapply(formalsOfFun, is.character)] <-
      lapply(formalsOfFun[sapply(formalsOfFun, is.character)], function(el) {
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
    sourceOfFun <- trimws(attr(fun, "srcref"))
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
