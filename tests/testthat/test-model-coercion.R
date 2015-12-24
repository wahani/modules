test_that("as.module", {

  expectEqual <- function(x, y) {
    testthat::expect_equal(x, y)
  }

  tmp <- tempfile()
  writeLines("fun <- function() 1", tmp)

  expect_true(exists("fun", as.module(tmp)))
  expect_true(exists("fun", as.module("", text = "fun <- function() 1")))

  expect_equal(
    environmentName(parent.env(parent.env(environment(as.module(tmp)$fun)))),
    "base"
  )

  tmpDir <- tempdir()
  writeLines("fun1 <- function() 1", paste0(tmpDir, "/tmp1.R"))
  writeLines("fun2 <- function() 1", paste0(tmpDir, "/tmp2.r"))
  writeLines("fun2 <- function() 1", paste0(tmpDir, "/tmp3"))

  folder <- use(tmpDir)
  expectEqual(folder$tmp1$fun1(), 1)
  expectEqual(folder$tmp2$fun2(), 1)
  expectEqual(length(folder), 2)

})
