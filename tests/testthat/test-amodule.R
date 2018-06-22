testthat::test_that("Constructors for augmented modules", {
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

testthat::test_that("Scoping of parameterized module", {

  dep <- oldDep <- 1
  moduleConst <- function(dep) {
    amodule({
      fun <- function() dep
      checkForDep <- function() exists("dep")
      changeState <- function() dep <<- 2
      topenv <- function() base::topenv()
    })
  }

  m <- moduleConst(dep)

  testthat::expect_equal(m$fun(), dep)
  testthat::expect_equal(m$checkForDep(), TRUE)
  testthat::expect_equal(m$changeState(), 2)
  testthat::expect_equal(dep, oldDep)
  testthat::expect_true(identical(m$topenv(), baseenv()))

})
