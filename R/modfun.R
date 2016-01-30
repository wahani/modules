#' Class modfun
#'
#' The constructor and a print method for the class 'modfun', a function in a
#' module. The only purpose is to have a print method for ad-hoc documentation.
#'
#' @param x (function)
#' @param ... (ignored)
#'
#' @examples
#' fun <- function() {
#'   ## This is documentation
#'   ## Triggered by '##'
#'   NULL
#' }
#'
#' modfun(fun)
#'
#' @export
#' @rdname modfun
modfun <- function(x) {

  getFormals <- function(fun) {
    args <- ifelse(
      as.character(formals(fun)) == "",
      names(formals(fun)),
      paste(names(formals(fun)), as.character(formals(fun)), sep = " = ")
    )
    paste0("function(", paste(args, collapse = ", "), ")")
  }

  getDoc <- function(fun) {
    sourceOfFun <- stringr::str_trim(attr(fun, "srcref"))
    sourceOfFun[grep("^##", sourceOfFun)]
  }

  # stopifnot(!is.null(attributes(x)$srcref))
  attr(x, "formals") <- getFormals(x)
  attr(x, "doc") <- getDoc(x)
  if (!isS4(x)) class(x) <- c("modfun", "function")
  x
}

#' @export
#' @rdname modfun
print.modfun <- function(x, ...) {
  cat(attr(x, "formals"))
  if (length(attr(x, "doc")) > 0) {
    lapply(attr(x, "doc"), function(x) cat("\n", x, sep = ""))
  }
  invisible(x)
}
