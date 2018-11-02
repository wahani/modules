testthat::test_that("problematic base function calls throw warnings", {
  testthat::skip_on_cran()
  testthat::expect_warning(
    module({
      library(modules)
    })
  )
  testthat::expect_warning(
    module({
      attach(list())
    })
  )
  testthat::expect_message(
    module({
      writeLines("", file <- tempfile())
      source(file)
    })
  )
})
