test_that("Imports of module", {
  # import and related functions are part of the parent scope. Not the module
  # itself.
  m <- module({ fun <- function() 1 })
  expect_true(Negate(exists)("import", environment(m$fun), inherits = FALSE))
  expect_true(exists("import", environment(m$fun)))
  expect_true(Negate(exists)("export", environment(m$fun), inherits = FALSE))
  expect_true(exists("export", environment(m$fun)))
  expect_true(Negate(exists)("use", environment(m$fun), inherits = FALSE))
  expect_true(exists("use", environment(m$fun)))

  # imported objects are only available to module
  m <- module({
    import(modules, module)
    localModule <- module({
      fun <- function(x) x
    })
  })
  expect_equal(m$localModule$fun(1), 1)
  expect_true(exists("module", environment(m$fun)))
  expect_true(Negate(exists)("module", environment(m$fun), inherits = FALSE))

  m <- module({
    here <- environment()
    m <- import("utils", ".S3methods", attach = FALSE)
    importPackage <- function() names(m) == ".S3methods"
    importPackageAttach <- function() !exists(".S3methods", where = here, inherits = FALSE)
  })

  testthat::expect_true(m$importPackage())
  testthat::expect_true(m$importPackageAttach())

})

test_that("delayed assignment", {
  # test for delayed assignment
  m <- module({
    import("base", "assignment") # does not exist!
    temp <- function() assignment
    checkExistens <- function() exists("assignment")
  })
  # When 'temp' is called, it should not find 'assignment'
  expect_error(m$temp())
  expect_true(m$checkExistens())

})
test_that("package dependencies", {

  m <- module({
    import("utils")
    deps <- function() exists("packageDescription")
  })

  testthat::expect_true(m$deps())
  testthat::expect_error(module({
    import("DoesNotExist")
  }), "'package:DoesNotExist' is not installed!")

})

test_that("cross package deps", {
  ## We skip this test on CRAN because it leads to errors on fedora and debian.
  ## I cannot reproduce these errors on ubuntu 18.10
  testthat::skip_on_cran()
  if (requireNamespace("disposables", quietly = TRUE)) {
    disposables::make_packages(

      imports = "modules",

      M1 = {
        m1 <- module({
          fun <- function(x) x
        })
      },

      M2 = {
        m2 <- module({
          import(M1, m1)
          newFun <- function(...) m1$fun(...)
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
    )}

})

test_that("duplications on search path", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  "%without%" <- function(x, set) {
    x[!(x %in% set)]
  }

  expectMessage <- function(obj) {
    testthat::expect_message(obj)
  }

  sp0 <- getSearchPathNames()

  m <- module({ })
  use(m, attach = TRUE)
  expectMessage(use(m, attach = TRUE))

  sp1 <- getSearchPathNames()

  tmp <- tempfile()
  writeLines("import(stats)
             fun <- function(x) median(x)", tmp)
  use(tmp, attach = TRUE)

  sp2 <- getSearchPathNames()

  import(stats)

  sp3 <- getSearchPathNames()

  expectMessage(use(m, attach = TRUE))

  sp4 <- getSearchPathNames()

  expectEqual(sp1[-1], c("modules:m", sp0[-1]))
  expectEqual(sp2[-1], c(paste0("modules:", tmp), sp1[-1]))
  expectEqual(sp3[-1], c("modules:stats", sp2[-1]))
  expectEqual(sp4[-1], c("modules:m", sp3[-1] %without% "modules:m"))

})
