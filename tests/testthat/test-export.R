test_that("Exports of module", {
  m <- module({

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true(all(c("fun", "pFun") %in% names(m)))
  expect_true(!(".fun" %in% names(m)))

  m <- module({

    export(fun)

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true("fun" %in% names(m))
  expect_true(!(".fun" %in% names(m)))
  expect_true(!("pFun" %in% names(m)))

  m <- module({

    export(fun, "pFun")

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true(all(c("fun", "pFun") %in% names(m)))
  expect_true(!(".fun" %in% names(m)))

  m <- module({

    export(fun)

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x
    export(pFun)

  })

  expect_true(all(c("fun", "pFun") %in% names(m)))
  expect_true(!(".fun" %in% names(m)))

})

test_that("Produce an error when 'export' is not available", {
  testthat::expect_error(
    modules::module({
      modules::export("fun", "fun1", "fun2")
      fun <- function(x) x
    }),
    "exports not defined: fun1, fun2"
  )
})


