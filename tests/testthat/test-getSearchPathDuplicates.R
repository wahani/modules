testthat::test_that("find duplicates on search path", {

  midentity <- module({
    identity <- function(x) x
  })

  testthat::expect_message(
    regexp = ".*identity.*",
    module({
      import("base", "identity")
    })
  )

  testthat::expect_message(
    regexp = ".*identity.*search",
    module({
      import("base", "identity", "search")
    })
  )

  testthat::expect_message(
    regexp = "(modules:base|modules:midentity).*identity.*(modules:|)base",
    m <- module({
      import("base", "identity")
      use(midentity, attach = TRUE)
      identity <- function(x) x
    })
  )

  testthat::expect_true(all(
    getSearchPathDuplicates(m)$identity %in%
      c("modules:midentity", "modules:base", "base"))
  )

  testthat::expect_message(
    regexp = NA,
    module(topEncl = baseenv(), {
      import("stats", "median")
    })
  )

})
