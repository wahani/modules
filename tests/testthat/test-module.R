test_that("Isolation of module", {

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
})

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

test_that("Attaching other module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m1 <- module({

    import(modules, module)

    m <- module({
      fun <- function(x) x
    })

    use(m, attach = TRUE)

    funNew <- function(x) fun(x)

    m1 <- module({
      fun <- function(x) x
      fun1 <- function(x) x
    })

    use(m1, "fun1", attach = TRUE)

    funNew1 <- function(x) fun1(x)

  })

  expectEqual(m1$funNew(1), 1)
  expectEqual(m1$funNew1(1), 1)

})

test_that("exposure of module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m <- module({
    import(modules, module)
    m <- module({
      .num <- NULL
      set <- function(val) .num <<- val
      get <- function() .num
    })
    expose(m, get, reInit = FALSE)
  })

  expectEqual(m$m$set(2), m$get())
  expectEqual(names(m), c("get", "m"))

})

test_that("package dependencies", {
  m <- module({
    import("aoos")
    deps <- function() exists("%g%")
  })
  expect_true(m$deps())

})

test_that("cross package deps", {

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

test_that("file as module", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  m <- module({
    tmp <- tempfile()
    writeLines("import(stats)
               fun <- function(x) median(x)", tmp)
    use(tmp, attach = TRUE)
    funWithDep <- function(x) fun(x)
  })

  expectEqual(m$funWithDep(1:7), 4)

})

test_that("duplications on search path", {

  expectEqual <- function(a, b) {
    testthat::expect_equal(a, b)
  }

  "%without%" <- function(x, set) {
    x[!(x %in% set)]
  }

  sp0 <- getSearchPathNames()

  m <- module({ })
  use(m, attach = TRUE)
  use(m, attach = TRUE)

  sp1 <- getSearchPathNames()

  tmp <- tempfile()
  writeLines("import(stats)
             fun <- function(x) median(x)", tmp)
  use(tmp, attach = TRUE)

  sp2 <- getSearchPathNames()

  import(stats)

  sp3 <- getSearchPathNames()

  use(m, attach = TRUE)

  sp4 <- getSearchPathNames()

  expectEqual(sp1[-1], c("modules:m", sp0[-1]))
  expectEqual(sp2[-1], c(paste0("modules:", tmp), sp1[-1]))
  expectEqual(sp3[-1], c("modules:stats", sp2[-1]))
  expectEqual(sp4[-1], c("modules:m", sp3[-1] %without% "modules:m"))

})

test_that("print method for modules", {

  expectOutput <- function(x, expr) {
    testthat::expect_output(x, expr)
  }

  expectOutput(module({
    fun <- function() {
      ## doc
      NULL
    }
  }),
  "fun:\nfunction\\(\\)\n")

})

