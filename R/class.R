class <- function(x, of) {
  if (missing(of)) base::class(x)
  else { class(x) <- c(of, base::class(x)); x }
}
