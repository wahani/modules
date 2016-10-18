test_that("modfun in module", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  m <- modules::module({
    
    oneLineDoc <- function(a, b = "a") {
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

    obj <- 1:10
    
  })

  expectOutput(
    print(m),
    'function\\(a, b = \"a\"\\)\n## comment'
  )

  expectOutput(
    print(m),
    "function\\(a, b = a\\)\n## a numeric\n##\n## Return:"
  )

  expectOutput(
    print(m),
    "function\\(a, b = a\\)"
  )

})
