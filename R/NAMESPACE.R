#' @importFrom utils installed.packages str
NULL

retList <- function(class = NULL, public = ls(envir), super = list(), envir = parent.frame()) {
  ## This is a variation of aoos::retList. Without the former inheritance
  ## mechanism. Maybe this with a different name is sufficient to supply
  ## OO-features in this package.
  public <- unique(c(public, names(super)))
  classes <- c(class, class(super))
  envir$.self <- envir
  out <- super
  out[public] <- as.list(envir, all.names = TRUE)[public]
  class(out) <- classes
  out
}
