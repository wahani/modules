test_that("as.module", {

  expectEqual <- function(x, y) {
    testthat::expect_equal(x, y)
  }

  expectTrue <- function(x) {
    testthat::expect_true(x)
  }

  tmp <- tempfile()
  writeLines("fun <- function() 1", tmp)

  expectTrue(exists("fun", as.module(tmp)))
  expectTrue(exists("fun", as.module("", text = "fun <- function() 1")))

  expectEqual(
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
