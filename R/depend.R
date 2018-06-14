#' Declare dependencies of modules
#'
#' This function will check for a dependency and tries to make it available
#' if it is not. This is a generic function. Currently only a default method
#' exists which assumes a package name as argument. If a package is not
#' installed \code{depend} tries to install it.
#'
#' @param on (character) a package name
#' @param version (character) a version, defaults to 'any'
#' @param libPath (character | NULL) a path to the library (folder where
#'   packages are installed)
#' @param ... arguments passed to \link{install.packages}
#'
#' @return
#' \code{TRUE} if dependency is available or successfully installed. An error if
#'   dependency can not be installed and is not available.
#'
#' @export
#' @examples
#' # Depend on certain R version
#' depend("base", "3.0.0")
#' # Depend on package version
#' depend("modules", "0.6.0")
depend <- function(on, ...) UseMethod("depend")

#' @rdname depend
#' @export
depend.default <- function(on, version = "any", libPath = NULL, ...) {

  stopifnot(length(on) == 1 && is.character(on))
  stopifnot(is.character(version))
  stopifnot(is.null(libPath) || is.character(libPath))

  needsUpdate <- function(on) {
    if (!is.element(on, lib())) TRUE
    else if (version == "any") FALSE
    else if (pkgVersion(on) < version) TRUE
    else FALSE
  }

  lib <- function() {
    installed.packages(lib.loc = libPath)[, "Package", drop = TRUE]
  }

  pkgVersion <- function(on) {
    packageVersion(on, libPath)
  }

  if (needsUpdate(on)) install.packages(on, lib = libPath, ...)
  if (needsUpdate(on)) # check if we now have the correct version
    stop("'", on, "' package installation failed for version ", version)

  invisible(TRUE)

}
