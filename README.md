[![Build Status](https://travis-ci.org/wahani/module.png?branch=master)](https://travis-ci.org/wahani/module)
[![codecov.io](https://codecov.io/github/wahani/module/coverage.svg?branch=master)](https://codecov.io/github/wahani/module?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/module)](http://cran.rstudio.com/package=module)
[![Downloads](http://cranlogs.r-pkg.org/badges/module?color=brightgreen)](http://www.r-pkg.org/pkg/module)


# Modules in R
Provides a module in R to organise source code around. Modules enforce to be more rigerous when defining dependencies. They are to be used as a sub unit within packages.

## Installation

From this repo:

```r
library(devtools)
install_github("wahani/aoos")
```



## Modules


```r
m <- module({
  boringFunction <- function() cat("boring output")
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$boringFunction()
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

```r
str(m) # it's just a list
```

```
## Error in str(m): object 'm' not found
```

Modules have their own scope, and have no idea what's going on around them:


```r
hey <- "hey"
m <- module({
  isolatedFunction <- function() hey
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$isolatedFunction()
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

Unless you force them to rely on some unknown quantity outside their control:


```r
m <- module({
  isolatedFunction <- function() .GlobalEnv$hey
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$isolatedFunction()
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

You have to rely on exported things of packages at some point:


```r
m <- module({
  functionWithDep <- function(x) stats::median(x)
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$functionWithDep(1:10)
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

Or if you like to have more lines or state your dependencies on top:


```r
m <- module({
  import(stats, median)
  functionWithDep <- function(x) median(x)
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$functionWithDep(1:10)
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

You can also use other modules by making all their exported functions
available in your new module definition.


```r
m1 <- module({
  use(.GlobalEnv$m) # normally: pkgName::moduleName
  anotherFunction <- function(x) functionWithDep(x)
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
str(m1)
```

```
## Error in str(m1): object 'm1' not found
```

```r
m1$anotherFunction(1:10)
```

```
## Error in eval(expr, envir, enclos): object 'm1' not found
```

Because this needs to fit into my normal workflow things like this are also possible:


```r
m <- module({
  use("package:methods")
  use("package:aoos")
  gen(x) %g% cat("default method")
  gen(x ~ numeric) %m% cat("method for x ~ numerc")
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
m$gen("Hej")
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

```r
m$gen(1)
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```

This gets messy if you rely on packages, like aoos, which depend on other
packages to be on the search path. In case of the methods package there is no
way around this because it is needed for everything related to S4s object
orientation. With well written packages this is no problem.

One example where you may want to have more control of the enclosing environment 
of a function is when you parallelize your code:


```r
library(parallel)
dependsOn <- function(x) median(x)
fun <- function(x) dependsOn(x) 

cl <- makeCluster(2)
clusterMap(cl, fun, replicate(2, 1:10, simplify = FALSE))
```

```
## Error in checkForRemoteErrors(val): 2 nodes produced errors; first error: could not find function "dependsOn"
```

```r
stopCluster(cl)
```

There are of course other options to solve this, anyway, here we go:


```r
m <- module({
  import(stats, median)
  dependsOn <- function(x) median(x)
  fun <- function(x) dependsOn(x) 
})
```

```
## Error in eval(expr, envir, enclos): could not find function "module"
```

```r
cl <- makeCluster(2)
clusterMap(cl, m$fun, replicate(2, 1:10, simplify = FALSE))
```

```
## Error in serialize(data, node$con): object 'm' not found
```

```r
stopCluster(cl)
```
