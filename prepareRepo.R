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
## - export
##     - error if objects are unavailable for export


library("modules")
library("parallel")
m <- module({
  import("base", "identity")
  identity
  fun <- function(x) {
    identity(x)
  }
})

modules::getSearchPath(environment(m$fun)) # this setup is slow
##parent.env(parent.env(environment(m$fun))) <- baseenv() # now it is fast

system.time({
  cl <- makeCluster(2)
  clusterMap(cl, m$fun, 1:100)
  stopCluster(cl)
})



