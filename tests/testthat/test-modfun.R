test_that("modfun", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  fun <- function() {
    ## This is documentation
    ## Triggered by '##'
    NULL
  }

  expectOutput(modfun(fun),
               "function\\(\\)\n## This is documentation\n## Triggered by '##'")

})

test_that("modfun in module", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  m <- module({
    oneLineDoc <- function(a, b = a) {
      ## comment
      a + b
    }
    multiLineDoc  <- function(a, b = a) {
      ## a numeric
      a + b
      ##
      ## Return:
    }
    oneLineFunction <- function(a, b = a) "test"
  })

  expectOutput(m$oneLineDoc,
               "function\\(a = , b = a\\)\n## comment")

  expectOutput(m$multiLineDoc,
               "function\\(a = , b = a\\)\n## a numeric\n##\n## Return:")

  expectOutput(m$oneLineFunction,
               "function\\(a = , b = a\\)")

})


test_that("modfun plays with S4", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  expecctIs <- function(x, is) {
    testthat::expect_is(x, is)
  }

  m <- module({
    import(methods)
    import(aoos)
    list : generic(x) %g% standardGeneric('generic')
    generic(x ~ ANY) %m% as.list(x)
  })

  expecctIs(m$generic, "standardGeneric")
  expectOutput(m, "generic:\nfunction\\(x = \\)")

})
