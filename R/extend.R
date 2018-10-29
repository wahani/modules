#' Extend existing module definitions
#'
#' \link{extend} can be used to extend an existing module definition. This can
#' be very useful to write unit tests when they need to have access to private
#' member functions of the module. This function breaks encapsulation of modules
#' and should be used with great care. As a mechanism for reuse consider
#' 'composition' using \link{expose} and \link{use}.
#'
#' @param module (character | module) a module as file or folder name or a list
#'   representing a module.
#' @param with (expression) an expression to add to the module definition.
#'
#' @details
#' A module can be characterized by its source code, the top enclosing
#'   environment and the environment the module has been defined in.
#'   \link{extend} will keep the latter two intact and only change the source
#'   code. That means that the new module will have the same scope as the module
#'   to be extended. \link{import}, \link{use}, and \link{export} declarations
#'   can be added as needed.
#'
#' This approach gives access to all implementation details of a module and
#'   breaks encapsulation. Possible use cases are: unit tests, and hacking the
#'   module system when necessary. For general reuse of modules, consider using
#'   \link{expose} and \link{use} which are safer to use.
#'
#' Since \code{extend} will alter the source code, the state of the
#'   module is ignored and will not be present in the new module. A fresh
#'   instance of that new module is returned and can in turn be extended and/or
#'   treated like any other module.
#'
#' @export
#' @examples
#' m1 <- module({
#'   foo <- function() "foo"
#' })
#' m2 <- extend(m1, {
#'   bar <- function() "bar"
#' })
#' m1$foo()
#' m2$foo()
#' m2$bar()
#' # For unit tests consider using:
#' extend(m1, {
#'   stopifnot(foo() == "foo")
#' })
extend <- function(module, with) {
  extendCheckPrerequisites(module)
  originalExpr <- deparse(attr(module, "expr"))
  additionalExpr <- deparse(match.call()$with)
  newExpr <- parse(text = c("{", originalExpr, additionalExpr, "}"))[[1]]
  ModuleConst(newExpr, attr(module, "topEncl"), attr(module, "topenv"))
}

extendCheckPrerequisites <- function(module) {
  hasAttributes <- c("expr", "topEncl") %in% names(attributes(module))
  stopifnot(all(hasAttributes))
}

extendWipeSrcMemory <- function(module) {
  expr <- attr(module, "expr")
  attributes(expr) <- NULL
  expr
}
