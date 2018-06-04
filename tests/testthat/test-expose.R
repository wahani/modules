test_that("exposure of module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m <- module({
    import(modules, module)
    m <- module({
      .num <- NULL
      set <- function(val) .num <<- val
      get <- function() .num
    })
    expose(m, get, reInit = FALSE)
  })

  expectEqual(m$m$set(2), m$get())
  expectEqual(names(m), c("get", "m"))

})
