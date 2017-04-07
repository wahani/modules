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
## - extend(module, with = { ... })
## - class(x, of) -- if missing(of) -> base::class


m <- modules::module({
  fun <- identity
})


m2 <- extend(extend(m, {
  fun1 <- function() 1
}), {
  fun2 <- function() 2
})

attr(m2, "expr")
