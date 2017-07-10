testthat::test_that("Construcors for parameterized modules", {
  dep <- oldDep <- 1
  moduleConst <- function(dep) {
    modules::module(topEncl = environment(), {
      fun <- function() dep
      checkForDep <- function() exists("dep")
      changeState <- function() dep <<- 2
    })
  }

  m <- moduleConst(dep)

  testthat::expect_equal(m$fun(), dep)
  testthat::expect_equal(m$checkForDep(), TRUE)
  testthat::expect_equal(m$changeState(), 2)
  testthat::expect_equal(dep, oldDep)

})
