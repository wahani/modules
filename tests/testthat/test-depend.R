testthat::test_that("Packages are installed", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_on_travis()
  try(utils::remove.packages("knitr"))
  modules::depend("knitr", "1.0.3", repos = "https://cloud.r-project.org")
  testthat::expect_true(require("knitr"))
})

testthat::test_that("Throw errors", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_on_travis()

  testthat::expect_is(suppressWarnings(
    tmp <- try(modules::depend(
      "knitr", "999",
      repos = "https://cloud.r-project.org"),
      TRUE)
  ), "try-error")
  testthat::expect_true(grepl("package installation failed", tmp))

  testthat::expect_is(suppressWarnings(
    tmp <- try(modules::depend(
      "knitr999", "999",
      repos = "https://cloud.r-project.org"),
      TRUE)
  ), "try-error")
  testthat::expect_true(grepl("package installation failed", tmp))

})
