test_that("Scope of module", {

  # defined objects are local to module
  m <- module({
    fun <- function(x) x
  })
  expect_true(Negate(exists)("fun"))
  expect_true(exists("fun", m))

  # module does not know of the outside world
  x <- 1
  m <- module({
    fun <- function() try(x, silent = TRUE)
  })
  expect_is(m$fun(), "try-error")

  # imported objects are only available to module
  m <- module({
    import(module, module)
    localModule <- module({
      fun <- function(x) x
    })
  })
  expect_equal(m$localModule$fun(1), 1)
  # expect_true(exists("module", m))
  # expect_true(Negate(exists)("module", m, inherits = FALSE))

  # import and related functions are part of the parent scope. Not the module
  # itself.
  m <- module({})
  # expect_true(Negate(exists)("import", m, inherits = FALSE))
  # expect_true(exists("import", m))

})

test_that("delayed assignment", {
  # test for delayed assignment
  m <- module({
    import("delayed", "assignment") # does not exist!
    temp <- function() assignment
    checkExistens <- function() exists("assignment")
  })
  # When 'temp' is called, it should not find 'assignment'
  expect_error(m$temp())
  expect_true(m$checkExistens())

})

test_that("Inheritance of module", {
  m1 <- module({

    import(module, module)

    m <- module({
      fun <- function(x) x
    })

    use(m)

    funNew <- function(x) fun(x)
  })

  expect_equal(m1$funNew(1), 1)

})

test_that("package dependencies", {
  m <- module({
    use("package:aoos")
    deps <- function() exists("%g%")
  })
  expect_true(m$deps())

  m <- module({
    use(as.environment("package:aoos"))
    deps <- function() exists("%g%")
  })
  expect_true(m$deps())

})
