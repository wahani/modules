library(modules)

# We check that a module gets properly re attached, when we load a new version.
# this works inside of a module; but as reported not in .Globalenv: #24

m <- modules::module({
  fun <- function(x) x
})

use(m, attach = TRUE)

stopifnot(fun(1) == 1)

m <- modules::module({
  fun <- function(x) x + 1
})

use(m, attach = TRUE)

stopifnot(fun(1) == 2)
