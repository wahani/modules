test_that("Attaching other module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m1 <- modules::module({

    import(modules, module)

    m <- module({
      fun <- function(x) x
    })

    use(m, attach = TRUE)

    funNew <- function(x) fun(x)

    m1 <- module({
      fun <- function(x) x
      fun1 <- function(x) x
    })

    use(m1, "fun1", attach = TRUE)

    funNew1 <- function(x) fun1(x)

  })

  expectEqual(m1$funNew(1), 1)
  expectEqual(m1$funNew1(1), 1)

})

test_that("file as module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m <- module({
    tmp <- tempfile()
    writeLines("import(stats)
               fun <- function(x) median(x)", tmp)
    use(tmp, attach = TRUE)
    funWithDep <- function(x) fun(x)
  })

  expectEqual(m$funWithDep(1:7), 4)

})

test_that("use finds object in global scope", {
  assign("m", list(f = identity), envir = topenv())
  tmp <- function() {
    module(topEncl = baseenv(), {
      lm <- use(m)
      tmp1 <- function() topenv()
      tmp2 <- function() exists("m")
      tmp3 <- function(x) lm$f(x)
    })
  }

  t <- tmp()
  testthat::expect_true(identical(t$tmp1(), baseenv()))
  testthat::expect_false(t$tmp2())
  testthat::expect_true(identical(t$lm$f, identity))
  rm(list = "m", envir = topenv())
})

test_that("use finds object in global scope", {
  m <- modules::module(topEncl = baseenv(), {
    error <- try(use(xyz), silent = TRUE)
  })
  testthat::expect_is(m$error, "try-error")
  testthat::expect_true(grepl("Error in use\\(module = xyz\\)", m$error))
})

test_that("Expose and use are working with package scope", {
  m <- modules:::TestModule2()
  testthat::expect_identical(m$foo, identity)
  testthat::expect_identical(m$b, c)
  testthat::expect_identical(m$c, 3)
  testthat::expect_identical(m$tm1$b, 3)
  testthat::expect_identical(m$tm1$foo, identity)
})

test_that("download of module works", {
  testthat::skip_on_cran()
  m <- use(
    "https://raw.githubusercontent.com/wahani/modules/master/tests/testModule.R"
  )
  testthat::expect_identical(m$fun, identity)
})

