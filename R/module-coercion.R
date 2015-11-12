#' Coercion for Modules
#'
#' Interfaces to and from modules.
#'
#' @param filename (character or connection) a file to be sourced as module
#' @param ... arguments passed to \link{parse}
#' @inheritParams module
#'
#' @export
#' @rdname modulecoerce
as.module <- function(filename, topEncl = baseenv(), ...) {
  do.call(module, list(parse(filename, ...), topEncl))
}
