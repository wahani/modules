# File with test modules to test scoping inside a package.

TestModule1 <- function(a) module({
  foo <- identity
  b <- a
})

TestModule2 <- function() module(topEncl = baseenv(), {
  c <- 3
  expose(TestModule1(c))
  tm1 <- use(TestModule1)(c)
})
