#' Use a module as dependency
#'
#' Use and/or register a module as dependency. The behaviour of use is similar
#' to \link{import} but instead of importing from packages, we import from a
#' module. A module can be defined in a file, or be an object.
#'
#' @param module (character, module) a file or folder name, or an object that
#'   can be interpreted as a module: any list-like object would do.
#' @param ... (character, or unquoted expression) names to use from module.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#' @param attach (logical) whether to attach the module to the search path.
#' @param reInit (logical) we can use a module as is, or reinitialize it. The
#'   default is to reinitialize. This is only relevant should the module be
#'   state-full.
#'
#' @details
#' \link{import} and \code{use} can replace \link{library} and \link{attach}.
#'   However they behave differently and are only designed to be used within
#'   modules. Both will work when called in the \code{.GlobalEnv} but here they
#'   should only be used for development and debugging of modules.
#'
#' \code{use} adds a layer to a local search path if \code{attach} is
#'   \code{TRUE}. More precisely to the calling environment, which is the
#'   environment supplied by \code{where}. Regardless of the \code{attach}
#'   argument, \code{use} will return the module invisibly.
#'
#' \code{use} supplies a special mechanism to find the argument \code{module}:
#'   generally you can supply a file name or folder name as character. You can
#'   also reference objects/names which 'live' outside the module scope. If
#'   names are not found within the scope of the module, they are searched for
#'   in the environment in which the module has been defined. This happens
#'   during initialization of the module, when the \code{use} function is
#'   called.
#'
#' Modules can live in files. \code{use} should be used to load them. A module
#'   definition in a file does not need to use the \link{module} constructor
#'   explicitly. Any R script can be used as the body of a module.
#'
#' When a folder is referenced in \code{use} it is transformed into a list of
#'   modules. This is represented as a nested list mimicking the folder
#'   structure. Each file in that folder becomes a module.
#'
#' @export
#' @examples
#' m1 <- module({
#'   foo <- function() "foo"
#' })
#' m2 <- module({
#'   use(m1, attach = TRUE)
#'   bar <- function() "bar"
#'   m1foo <- function() foo()
#' })
#' m2$m1foo()
#' m2$bar()
#'
#' \dontrun{
#' someFile <- tempfile(fileext = ".R")
#' writeLines("foo <- function() 'foo'", someFile)
#' m3 <- use(someFile)
#' m3$foo()
#' otherFile <- tempfile(fileext = ".R")
#' writeLines("bar <- function() 'bar'", otherFile)
#' m4 <- use(otherFile)
#' m4$bar()
#' m5 <- use(tempdir())
#' m5
#' }
use <- function(module, ..., attach = FALSE, reInit = TRUE, where = parent.frame()) {

  moduleName <- as.character(substitute(module))
  module <- useTryFindModule(module, moduleName, where, match.call())
  name <- if (is.character(module)) module else moduleName
  module <- as.module(module, reInit = reInit, envir = where)
  module <- useGetSelection(module, match.call(expand.dots = TRUE))

  if (attach) addDependency(
    module,
    names(module),
    where,
    makeAssignment,
    name
  )

  invisible(module)

}

useTryFindModule <- function(module, moduleName, envir, mc) {
  m <- try(module, TRUE)
  if (is.error(m)) {
    m1 <- try(
      eval(mc$module, get(useTopenvNameWithinModule(), envir = envir)), TRUE)
    if (is.error(m1)) stop(simpleError(useGetErrorMessage(m), mc))
    else m <- m1
  }
  m
}

is.error <- function(x) {
  inherits(x, "try-error")
}

useGetErrorMessage <- function(x) {
  attributes(x)$condition$message
}

useGetSelection <- function(module, mc) {
  namesToImport <- deparseEllipsis(mc, c("module", "attach", "reInit", "where"))
  if (length(namesToImport) == 0) module
  else module[namesToImport]
}

useTopenvNameWithinModule <- function() {
  ".__topenv__"
}
