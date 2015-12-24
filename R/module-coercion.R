#' Coercion for Modules
#'
#' Interfaces to and from modules.
#'
#' @param x something which can be coerced into a module. \code{character} are
#'   interpreted as file names.
#' @param ... arguments passed to \link{parse}
#' @inheritParams module
#'
#' @export
#' @rdname modulecoerce
list : as.module(x, topEncl = baseenv(), ...) %g% {
  as.list(x)
}

#' @export
#' @rdname modulecoerce
as.module(x ~ character, topEncl, ...) %m% {
  stopifnot(length(x) == 1)
  x <- if (dir.exists(x)) list.files(x, "\\.(r|R)$", FALSE, TRUE, TRUE) else x
  modules <- lapply(x, function(x) {
    do.call(module, list(parse(x, ...), topEncl))
  })
  if (length(modules) == 1) modules[[1]]
  else `names<-`(modules, gsub("\\.(r|R)$", "", sapply(x, basename)))
}
