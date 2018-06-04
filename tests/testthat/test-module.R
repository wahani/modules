test_that("Isolation of module", {
  # defined objects are local to module
  m <- module({
    fun <- function(x) x
  })
  expect_true(Negate(exists)("fun"))
  expect_true(exists("fun", m))

  # module does not know of the outside world. This is so in interactive mode.
  # In a package it is the enclosing env. The test env is not interactive.
  x <- 1
  m <- module({
    fun <- function() try(x, silent = TRUE)
  }, baseenv())
  expect_is(m$fun(), "try-error")
})

test_that("nested modules", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  val <- module({

    import(stats, median)
    import(modules, module)

    # The nested module should be able to figure out, that it is inside a nested
    # module and hence can connect:
    m <- module({
      fun <- function(x) median(x)
    })

  })$m$fun(1:10)

  expectEqual(
    val,
    5.5
  )

})

test_that("print method for modules", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  expectOutput(print(module({
    fun <- function() {
      ## doc
      NULL
    }
  })),
  "fun:\nfunction\\(\\)\n")

})

