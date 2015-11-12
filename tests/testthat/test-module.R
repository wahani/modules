test_that("Scope of module", {

  # defined objects are local to module
  m <- module({
    fun <- function(x) x
  })
  expect_true(Negate(exists)("fun"))
  expect_true(exists("fun", m))

  # module does not know of the outside world. This is so in interactive mode.
  # In a apckage it is the enclosing env. The test env is not interactive.
  x <- 1
  m <- module({
    fun <- function() try(x, silent = TRUE)
  }, baseenv())
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
  m <- module({ fun <- function() 1 })
  expect_true(Negate(exists)("import", environment(m$fun), inherits = FALSE))
  expect_true(exists("import", environment(m$fun)))

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
    use("aoos")
    deps <- function() exists("%g%")
  })
  expect_true(m$deps())

})

test_that("cross package deps", {

  disposables::make_packages(

    imports = "module",

    M1 = {
      m1 <- module({
        fun <- function(x) x
      })
    },

    M2 = {
      m2 <- module({
        use(M1::m1)
        newFun <- function(...) fun(...)
      })
    }

  )

  m1 <- module(
    topEncl = baseenv(),
    fun <- function(x) x
  )

  expect_equal(
    environmentName(parent.env(parent.env(environment(M1::m1$fun)))),
    "M1"
  )

  expect_equal(
    environmentName(parent.env(parent.env(environment(m1$fun)))),
    "base"
  )

})

test_that("Exports of module", {
  m <- module({

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true(all(c("fun", "pFun") %in% names(m)))
  expect_true(!(".fun" %in% names(m)))

  m <- module({

    export(fun)

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true("fun" %in% names(m))
  expect_true(!(".fun" %in% names(m)))
  expect_true(!("pFun" %in% names(m)))

  m <- module({

    export(fun, "pFun")

    fun <- function(x) x
    .fun <- function(x) x
    pFun <- function(x) x

  })

  expect_true(all(c("fun", "pFun") %in% names(m)))
  expect_true(!(".fun" %in% names(m)))

})
