testthat::context("depend")

testthat::test_that("Packages are installed", {
  testthat::skip_on_cran()
  utils::remove.packages("disposables")
  modules::depend("disposables", "1.0.3")
  testthat::expect_true(require("disposables"))
})

testthat::test_that("Throw errors", {

  testthat::skip_on_cran()

  testthat::expect_is(
    suppressWarnings(tmp <- try(modules::depend("disposables", "999"), TRUE)),
    "try-error"
  )
  testthat::expect_true(grepl("package installation failed", tmp))

  testthat::expect_is(
    suppressWarnings(tmp <- try(modules::depend("disposables999", "999"), TRUE)),
    "try-error"
  )
  testthat::expect_true(grepl("package installation failed", tmp))

})
