testthat::test_that("extend: add method", {

  m <- modules::module({
    fun <- identity
  })

  m <- modules::extend(m, {
    fun1 <- identity
  })

  testthat::expect_equal(m$fun1(m$fun(1)), 1)
  
})

testthat::test_that("extend: override method", {

  m <- modules::module({
    fun <- identity
  })

  m <- modules::extend(m, {
    fun <- function(x) 2 * x
  })

  testthat::expect_equal(m$fun(1), 2)
  
})

testthat::test_that("extend: nested definitions", {

  m <- extend(
    extend(
      modules::module({
        fun <- identity
      }),
      {fun <- identity}
    ),
    {fun <- identity}
  )
  
  testthat::expect_equal(m$fun(1), 1)
  
})
