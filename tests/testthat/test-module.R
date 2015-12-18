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
  m1 <- module({

    import(modules, module)

    m <- module({
      fun <- function(x) x
    })

    use(m, TRUE)

    funNew <- function(x) fun(x)

  })

  expect_equal(m1$funNew(1), 1)

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

test_that("effects on global env", {

  # tmp <- tempfile()
  # writeLines("module::import(module)
  # import(aoos)
  # m <- module({
  #   fun <- identity
  # })
  # use(m)
  # search()[2:4]", tmp)
  #
  # cl <- parallel::makeCluster(1)
  # parallel::clusterExport(cl, "tmp", environment())
  # res <- parallel::clusterEvalQ(cl, source(tmp))
  # parallel::stopCluster(cl)
  # attachedModules <- res[[1]]$value
  # expect_equal(attachedModules,
  #              c("import:module", "import:aoos", "import:module")
  # )

})

test_that("file as module", {

  m <- module({
    tmp <- tempfile()
    writeLines("import(stats)
               fun <- function(x) median(x)", tmp)
    use(tmp, attach = TRUE)
    funWithDep <- function(x) fun(x)

  })

  expect_equal(m$funWithDep(1:7), 4)

})
