# We mask some base functions within modules to signal the user potential
# problems. Problematic are
# - use and library: they change the global state of an R session and not the
#   search path of a module
# - source: most likely is used when 'use' would be a better choice

llibrary <- function(...) {
  # l(ocal)library
  warning(paste0(
    "Packages loaded with 'library' may not be available inside a module. ",
    "For loading packages in a module, use 'import' instead."))
  mc <- match.call()
  mc[[1]] <- quote(base::library)
  eval(mc, envir = parent.frame())
}

lattach <- function(...) {
  # l(ocal)attach
  warning(paste0(
    "Objects, including modules, loaded with 'attach' may not be available ",
    "inside a module. To attach an object to the search path, use 'use' with ",
    "'attach = TRUE' instead."))
  mc <- match.call()
  mc[[1]] <- quote(base::attach)
  eval(mc, envir = parent.frame())
}

lsource <- function(...) {
  # l(ocal)source
  message(paste0(
    "Using 'source' inside a module often can be replaced by 'use' or 'expose'. ",
    "Consider the examples in the 'scripts as modules' section in the vignette. ",
    "Deactivate this message with 'suppressMessages'."
  ))
  mc <- match.call()
  mc[[1]] <- quote(base::source)
  eval(mc, envir = parent.frame())
}
