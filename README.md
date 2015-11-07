[![Build Status](https://travis-ci.org/wahani/module.png?branch=master)](https://travis-ci.org/wahani/module)
[![codecov.io](https://codecov.io/github/wahani/module/coverage.svg?branch=master)](https://codecov.io/github/wahani/module?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/module)](http://cran.rstudio.com/package=module)
[![Downloads](http://cranlogs.r-pkg.org/badges/module?color=brightgreen)](http://www.r-pkg.org/pkg/module)


# Modules in R
Provides modules as an organizational unit for source code. Modules enforce to be more rigerous when defining dependencies and have something like a local search path. They are to be used as a sub unit within packages or in scripts.

## Installation

From this repo:

```r
library(devtools)
install_github("wahani/aoos")
```



## Modules


```r
library(module)
m <- module({
  boringFunction <- function() cat("boring output")
})

m$boringFunction()
```

```
## boring output
```

```r
str(m) # it's just a list
```

```
## List of 1
##  $ boringFunction:function ()  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 3 21 3 51 21 51 3 3
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x6fd24a0>
```

Modules have their own scope, and have no idea what's going on around them:


```r
hey <- "hey"
m <- module({
  isolatedFunction <- function() hey
})
m$isolatedFunction()
```

```
## Error in m$isolatedFunction(): object 'hey' not found
```

Unless you force them to rely on some unknown quantity outside their control:


```r
m <- module({
  isolatedFunction <- function() .GlobalEnv$hey
})
m$isolatedFunction()
```

```
## [1] "hey"
```

You have to rely on exported things of packages at some point:


```r
m <- module({
  functionWithDep <- function(x) stats::median(x)
})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

Or if you like to have more lines or state your dependencies on top:


```r
m <- module({
  import(stats, median)
  functionWithDep <- function(x) median(x)
})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

You can also use other modules by making all their exported functions
available in your new module definition.


```r
m1 <- module({
  use(.GlobalEnv$m) # normally: pkgName::moduleName
  anotherFunction <- function(x) functionWithDep(x)
})
str(m1)
```

```
## List of 1
##  $ anotherFunction:function (x)  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 3 22 3 51 22 51 3 3
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x5ffec18>
```

```r
m1$anotherFunction(1:10)
```

```
## [1] 5.5
```

Because this needs to fit into my normal workflow things like this are also possible:


```r
m <- module({
  use("methods")
  use("aoos")
  gen(x) %g% cat("default method")
  gen(x ~ numeric) %m% cat("method for x ~ numerc")
})
m$gen("Hej")
```

```
## default method
```

```r
m$gen(1)
```

```
## method for x ~ numerc
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

cl <- makeCluster(2)
clusterMap(cl, m$fun, replicate(2, 1:10, simplify = FALSE))
```

```
## [[1]]
## [1] 5.5
## 
## [[2]]
## [1] 5.5
```

```r
stopCluster(cl)
```
