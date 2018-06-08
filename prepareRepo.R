devtools::build_vignettes()
knitr::knit("inst/doc/modulesInR.Rmd", "README.md")

text <- c(
  "[![Build Status](https://travis-ci.org/wahani/modules.png?branch=master)](https://travis-ci.org/wahani/modules)",
  "[![codecov.io](https://codecov.io/github/wahani/modules/coverage.svg?branch=master)](https://codecov.io/github/wahani/modules?branch=master)",
  "[![CRAN](http://www.r-pkg.org/badges/version/modules)](https://cran.r-project.org/package=modules)",
  "![Downloads](http://cranlogs.r-pkg.org/badges/modules)",
  "# Modules in R",
  readLines(
    "README.md"
  )[-(1:9)]
)

writeLines(text, "README.md")

## TODO

## - import
##     - warning if duplicates on search path
## - pmodule
##     - inherits from module
##     - always has baseenv as toplevel by default
##     - use surrounding env as first dependency layer (function)
##     - has a class to be consistent

amodule <- function(expr = {},
                   envir = parent.frame(), enclos = baseenv(),
                   class = as.character(sys.call(1)[[1]])) {
  mc <- match.call()
  mc[[1]] <- quote(modules::module)
  mc$envir <- NULL
  mc$class <- NULL
  mc$topEncl <- quote(topEncl)
  topEncl <- list2env(as.list(envir), parent = enclos)
  obj <- eval(mc)
  class(obj) <- c(class, "module", "list")
  obj
}

y <- 2

Object <- function(y) {
  object({
    test <- function(x) y
  })
}

o <- Object(5)
o$test()
str(o)


library("modules")
library("parallel")
m <- module({
  import("base", "identity")
  identity
  fun <- function(x) {
    identity(x)
  }
})

mfun <- local(envir = new.env(parent = baseenv()), {
  modules::import("base", "identity")
  function(x) {
    base::identity(x)
  }
})

modules::getSearchPath(environment(m$fun)) # this setup is slow
modules::getSearchPath(environment(mfun)) # this setup is slow
##parent.env(parent.env(environment(m$fun))) <- baseenv() # now it is fast

system.time({
  cl <- makeCluster(2)
  clusterMap(cl, m$fun, 1:100)
  stopCluster(cl)
})

system.time({
  cl <- makeCluster(2)
  clusterMap(cl, mfun, 1:100)
  stopCluster(cl)
})

system.time({
  cl <- makeCluster(2)
  clusterMap(cl, identity, 1:100)
  stopCluster(cl)
})
