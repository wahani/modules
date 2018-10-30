context("getSearchPathContent")

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
