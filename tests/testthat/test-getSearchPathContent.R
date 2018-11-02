testthat::test_that("SearchPathContent", {
  m <- module({
    export("foo")
    import("stats", "median")
    foo <- function() "foo"
    bar <- function() "bar"
  })
  content <- getSearchPathContent(m)
  testthat::expect_equal(content[[1]], c("bar", "foo"))
  testthat::expect_equal(content[[2]], "median")
})

testthat::test_that("Identification of modules", {
  # https://github.com/wahani/modules/issues/9#issuecomment-435056847
  # we may have functions and data in some use cases
  m <- module({
    a <- NULL
    b <- function() NULL
  })
  content <- getSearchPathContent(m)
  testthat::expect_equal(content[[1]], letters[1:2])
})
