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

testthat::test_that("Scoping of parameterized module", {

  amodule <- function(expr = {},
                      envir = parent.frame(), enclos = baseenv(),
                      class = as.character(sys.call(1)[[1]])) {
    mc <- match.call()
    mc[[1]] <- quote(modules::module)
    mc$class <- NULL
    mc$topEncl <- quote(topEncl)
    mc$envir <- quote(envir)
    topEncl <- list2env(as.list(envir), parent = enclos)
    obj <- eval(mc)
    class(obj) <- c(class, "module", "list")
    obj
  }

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
