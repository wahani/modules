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
  do.call(module, list(parse(x, ...), topEncl))
}
