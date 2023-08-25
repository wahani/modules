library(modules)
devtools::install()
devtools::test()


devtools::build_vignettes()
knitr::knit("vignettes/modulesInR.Rmd", "README.md")

text <- c(
  "[![Build Status](https://github.com/wahani/modules/actions/workflows/rcmdcheck.yml/badge.svg?branch=main)](https://github.com/wahani/modules/actions)",
  "[![codecov.io](https://codecov.io/github/wahani/modules/coverage.svg?branch=master)](https://codecov.io/github/wahani/modules?branch=master)",
  "[![CRAN](http://www.r-pkg.org/badges/version/modules)](https://cran.r-project.org/package=modules)",
  "![Downloads](http://cranlogs.r-pkg.org/badges/modules)",
  "# Modules in R",
  readLines(
    "README.md"
  )[-(1:9)]
)

writeLines(text, "README.md")
https://m.tiktok.com/passport/email/unbind/index/?unbind_ticket=vRGFtnMfdskVwHyCwUPmXUEZEXdFZNtx&aid=1233&locale=en&language=en
## TODO

## - depend
##   - on .tar.gz


library("modules")
library("parallel")
m <- module({
  import("stats", "median")
})

m <- module({
  import("base", "identity", "search")
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
## parent.env(parent.env(environment(m$fun))) <- baseenv() # now it is fast

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


# #43



m <- modules::module({
  export("==.foo" = equals)
  equals <- function(left, right) {return(left == right)}
})

m$"==.foo"(1, 2)


m <- modules::module({
  export(true = !FALSE)
})
m$true


library(modules)
modules::module({
  "[" <- function(..., drop = FALSE) .Primitive("[")(..., drop = drop)
})
