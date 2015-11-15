test_that("as.module", {

  tmp <- tempfile()
  writeLines("fun <- function() 1", tmp)

  expect_true(exists("fun", as.module(tmp)))
  expect_true(exists("fun", as.module("", text = "fun <- function() 1")))

  expect_equal(
    environmentName(parent.env(parent.env(environment(as.module(tmp)$fun)))),
    "base"
  )

})
