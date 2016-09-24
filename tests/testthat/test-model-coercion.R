test_that("as.module", {

  expectEqual <- function(x, y) {
    testthat::expect_equal(x, y)
  }

  expectTrue <- function(x) {
    testthat::expect_true(x)
  }

  tmp <- tempfile()
  writeLines("fun <- function() 1", tmp)

  expectTrue(exists("fun", as.module(tmp)))
  expectTrue(exists(
    "fun1",
    as.module(tmp, text = "fun1 <- function() 1")
  ))

  expectEqual(
    environmentName(parent.env(parent.env(environment(as.module(tmp)$fun)))),
    "base"
  )

  tmpDir <- tempdir()
  writeLines("fun1 <- function() 1", paste0(tmpDir, "/tmp1.R"))
  writeLines("fun2 <- function() 1", paste0(tmpDir, "/tmp2.r"))
  writeLines("fun2 <- function() 1", paste0(tmpDir, "/tmp3"))

  folder <- use(tmpDir)
  expectEqual(folder$tmp1$fun1(), 1)
  expectEqual(folder$tmp2$fun2(), 1)
  expectEqual(length(folder), 2)


  file.remove(list.files(tmpDir, full.names = TRUE, pattern = "\\.(r|R)$"))
  writeLines("fun1 <- function() 1", paste0(tmpDir, "/tmp1.R"))
  folder <- use(tmpDir)

  expectEqual(class(folder), "list")
  expectEqual(length(folder), 1)
  expectEqual(class(folder$tmp1), c("module", "list"))

  m <- module({
    .num <- NULL
    set <- function(val) .num <<- val
    get <- function() .num
  })

  m$set(2)
  expectEqual(m$get(), 2)

  m1 <- as.module(m, reInit = TRUE)
  expectTrue(is.null(m1$get()))

  m2 <- as.module(m, reInit = FALSE)
  expectEqual(m2$get(), m$get())

})
