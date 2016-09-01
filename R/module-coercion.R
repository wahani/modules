#' Coercion for Modules
#'
#' Interfaces to and from modules.
#'
#' @param x something which can be coerced into a module. \code{character} are
#'   interpreted as file / folder names.
#' @param ... arguments passed to \link{parse}
#' @param reInit (logical) if a module should be re-initialized
#' @inheritParams module
#'
#' @export
#' @rdname modulecoerce
#'
#' @examples
#' # as.module is used by 'use' so see the vignette for examples:
#' \dontrun{
#' vignette("modulesInR", "modules")
#' }
list : as.module(x, topEncl = baseenv(), reInit = TRUE, ...) %g% {
  as.list(x)
}

#' @export
#' @rdname modulecoerce
as.module(x ~ character, topEncl, reInit, ...) %m% {
  stopifnot(length(x) == 1)

  fileAsModule <- function(x, topEncl, reInit, ...) {
    files <- if (dir.exists(x)) list.files(x, "\\.(r|R)$", FALSE, TRUE, TRUE) else x
    modules <- lapply(files, function(x) {
      do.call(module, list(parse(x, ...), topEncl))
    })
    if (length(modules) == 1 && !dir.exists(x)) modules[[1]]
    else `names<-`(modules, gsub("\\.(r|R)$", "", sapply(files, basename)))
  }

  packageAsModule <- function(x) {
    pkgName <- sub("^package:", "", x)
    env <- new.env()
    eval(call("import", from = pkgName, where = env))
    as.list(parent.env(env), all.names = TRUE)
  }

  if (grepl("^package:.*$", x)) packageAsModule(x)
  else fileAsModule(x, topEncl, reInit, ...)
  
  
}

#' @export
#' @rdname modulecoerce
#' @include NAMESPACE.R
as.module(x ~ module, topEncl, reInit, ...) %m% {
  if (reInit) {
    x %invoke% new()
  } else {
    x
  }
}
