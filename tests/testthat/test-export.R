test_that("Exports of special names #37", {
  m <- module({
    "%+%" <- function(lhs, rhs) lhs + rhs
  })
  m$`%+%`(1, 2)
})

test_that("Exports of module", {
  m <- module({

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  testthat::expect_true(all(c("fun", "pFun") %in% names(m)))
  testthat::expect_true(!(".fun" %in% names(m)))

  m <- module({

    export(fun)

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  testthat::expect_true("fun" %in% names(m))
  testthat::expect_true(!(".fun" %in% names(m)))
  testthat::expect_true(!("pFun" %in% names(m)))

  m <- module({

    export(fun, "pFun")

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  testthat::expect_true(all(c("fun", "pFun") %in% names(m)))
  testthat::expect_true(!(".fun" %in% names(m)))

  m <- module({

    export(fun)

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x
    export(pFun)

  })

  testthat::expect_true(all(c("fun", "pFun") %in% names(m)))
  testthat::expect_true(!(".fun" %in% names(m)))

})

test_that("Produce an error when 'export' is not available", {
  testthat::expect_error(
    modules::module({
      modules::export("fun", "fun1", "fun2")
      fun <- function(x) x
    }),
    "unable to resolve export: fun1"
  )
})

test_that("Rename exports", {
  m <- modules::module({
    export(
      foo,
      a = foo,
      b = "foo",
      c = function() foo()
    )
    foo <- function() "foo"
  })
  testthat::expect_equal(m$foo(), "foo")
  testthat::expect_equal(m$a(), "foo")
  testthat::expect_equal(m$b(), "foo")
  testthat::expect_equal(m$c(), "foo")
})

test_that("Export .names", {
  m <- modules::module({
    export(.foo = foo)
    foo <- function() "foo"
  })
  testthat::expect_equal(m$.foo(), "foo")
})

test_that("Warning on duplicate names", {
  testthat::expect_warning(
    modules::module({
      export(foo, "foo")
      foo <- function() "foo"
    }),
    "duplicate names in exports"
  )
})

test_that("Warn with do.call", {
  testthat::expect_warning(
    modules::module({
      do.call(export, list("foo"))
      foo <- function() "foo"
    }),
    "non standard call to export"
  )
})
