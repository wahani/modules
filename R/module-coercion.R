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
as.module <- function(x, ...) {
  UseMethod("as.module")
}

#' @export
as.module.default <- function(x, ...) {
  as.list(x)
}

#' @export
as.module.function <- function(x, ...) {
  x
}


#' @export
#' @rdname modulecoerce
as.module.character <- function(x, topEncl = baseenv(), reInit = TRUE, ...,
                                envir = parent.frame()) {
  stopifnot(length(x) == 1)

  dirAsModule <- function(x, topEncl, ...) {
    files <- list.files(x, "\\.(r|R)$", FALSE, TRUE, TRUE)
    modules <- lapply(files, fileAsModule, topEncl, ...)
    names(modules) <- gsub("\\.(r|R)$", "", sapply(files, basename))
    modules
  }

  fileAsModule <- function(x, topEncl, ...) {
    do.call(module, list(parse(x, ...), topEncl, envir))
  }

  urlAsModule <- function(x, topEncl, ...) {
    download.file(x, fileName <- tempfile(fileext = ".R"))
    fileAsModule(fileName, topEncl, ...)
  }

  is.url <- function(x) grepl("^(https?|ftp)://", x)

  if (dir.exists(x)) dirAsModule(x, topEncl, ...)
  else if (file.exists(x)) fileAsModule(x, topEncl, ...)
  else if (is.url(x)) urlAsModule(x, topEncl, ...)
  else stop("Can`t find ", x)

}

#' @export
#' @rdname modulecoerce
as.module.module <- function(x, reInit = TRUE, ...) {
  if (reInit) ModuleConst(attr(x, "expr"), attr(x, "topEncl"), attr(x, "topenv"))
  else x
}
