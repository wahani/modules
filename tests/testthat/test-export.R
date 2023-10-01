testthat::test_that("export can be called savely outside of module #47", {
  testthat::expect_warning(
    modules::export("something"),
    "Calling 'export' outside of a module has no effect."
  )
})

test_that("Exports of special names #43", {
  m <- module({
    "==.foo" <- function(lhs, rhs) base::`==`(lhs, rhs) # Exclude Linting
    "!.foo" <- function(lhs, rhs) base::`!=`(lhs, rhs) # Exclude Linting
  })
  testthat::expect_true(m$`==.foo`(1, 1))
  testthat::expect_true(m$`!.foo`(1, 2))
})

test_that("Exports of special names #37", {
  m <- module({
    "%+%" <- function(lhs, rhs) lhs + rhs # Exclude Linting
    "%add%" <- `%+%`
  })
  testthat::expect_true(m$`%+%`(1, 2) == 3)
  testthat::expect_true(m$`%add%`(1, 2) == 3)
})

test_that("Exports of special names #45", {
  m <- module({
    "[" <- `[`
    . <- "."
    "==" <- "=="
  })
  testthat::expect_true(is.primitive(m$"["))
  testthat::expect_true(is.null(m$.))
  testthat::expect_true(m$"==" == "==")
})

test_that("Exports of expressions", {
  m <- module({
    export(
      true = !FALSE,
      false = !T
    )
  })
  testthat::expect_true(m$true)
  testthat::expect_true(!m$false)
})

test_that("Exports of names with whitespace #39", {
  m <- module({
    "my fun" <- function(x) x # Exclude Linting
    "my long fun name" <- function(x) x # Exclude Linting
    "1 my fun1" <- function(x) x # Exclude Linting
  })
  testthat::expect_true(m$`my fun`(1) == 1)
  testthat::expect_true(m$`my long fun name`(1) == 1)
  testthat::expect_true(m$`1 my fun1`(1) == 1)
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
      c = function() foo(),
      bar = sub$bar
    )
    sub <- module({
      bar <- function() "bar"
    })
    foo <- function() "foo"
  })
  testthat::expect_equal(m$foo(), "foo")
  testthat::expect_equal(m$a(), "foo")
  testthat::expect_equal(m$b(), "foo")
  testthat::expect_equal(m$c(), "foo")
  testthat::expect_equal(m$bar(), "bar")
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
