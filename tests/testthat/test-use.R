test_that("Attaching other module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m1 <- module({

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

