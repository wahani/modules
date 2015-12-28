---
title: "Modules in R"
author: "Sebastian Warnholz"
date: "2015-12-28"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Modules in R}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[![Build Status](https://travis-ci.org/wahani/modules.png?branch=master)](https://travis-ci.org/wahani/modules)
[![codecov.io](https://codecov.io/github/wahani/modules/coverage.svg?branch=master)](https://codecov.io/github/wahani/modules?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/modules)](https://cran.r-project.org/package=modules)
![Downloads](http://cranlogs.r-pkg.org/badges/modules)

Provides modules as an organizational unit for source code. Modules enforce to be more rigorous when defining dependencies and have a local search path. They can be used as a sub unit within packages or in scripts.

## Installation

From GitHub:


```r
devtools::install_github("wahani/modules")
```

# Introduction

The key idea of this package is to provide a unit which is self contained, i.e. 
has it's own scope. The main and most reliable infrastructure for such 
organizational units of source code in the R ecosystem is a package. Compared to
a package modules can be considered ad hoc, but - in the sense of an R-package -
self contained. Furthermore modules consist of one file; in contrast to a
package which can wrap an arbitrary number of files.

There are two use cases. First when you use modules to develop scripts, which is
subject of this section; And then inside of packages where modules act more like
objects, as in object-oriented-programming. Outside of packages modules know
only of the base environment, i.e. within a module the base environment is the
only *package* on the *search path*. Also they are always represented as a list
inside R. Thus they can be treated as bags of functions.

In the following examples you will see the function `module` to define modules.
Typically you do not have to call that function explicitly but instead call
`use` to load a module into your current session.


```r
library(modules)
m <- module({
  boringFunction <- function() cat("boring output")
})

m$boringFunction()
```

```
## boring output
```

Since they are isolated from the `.GlobalEnv` the following object `hey` can not
be found:


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

## Imports

If you rely on exported objects of packages you can refer to them explicitly
using `::`:


```r
m <- module({
  functionWithDep <- function(x) stats::median(x)
})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

Or you can use `import` for *attaching* single objects or packages and `use` for
*attaching* or loading a module:


```r
m <- module({
 
  import(stats, median) # make median from package stats available
  
  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```


```r
m <- module({
  
  import(stats)
  
  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

## Exports

It may also be of interest to control which objects are visible for the client.
You can do that with the `export` function. Note that export accepts regular
expressions which are indicated by a leading '^'.


```r
m <- module({
  
  export("fun")
  
  .privateFunction <- identity
  privateFunction <- identity
  fun <- identity
  
})

names(m)
```

```
## [1] "fun"
```

## Example: Modules as Parallel Process

One example where you may want to have more control of the enclosing environment 
of a function is when you parallelize your code. First consider the case when a 
*naive* implementation fails.


```r
library(parallel)
dependency <- identity
fun <- function(x) dependency(x) 

cl <- makeCluster(2)
clusterMap(cl, fun, 1:2)
```

```
## Error in checkForRemoteErrors(val): 2 nodes produced errors; first error: could not find function "dependency"
```

```r
stopCluster(cl)
```

To make the function `fun` self contained we can define it in a module. 


```r
m <- module({
  dependency <- identity
  fun <- function(x) dependency(x) 
})

cl <- makeCluster(2)
clusterMap(cl, m$fun, 1:2)
```

```
## [[1]]
## [1] 1
## 
## [[2]]
## [1] 2
```

```r
stopCluster(cl)
```


# Scripts as modules

You can make scripts into modules with `as.module` or implicitly when you refer
to a file in a call to `use`. Inside such a script you can use `import` and
`use` in the same way you typically use `library`. A major difference is, that
library will not only attach the stated package but also all packages in the 
depends field of that dependency. This is something you have to do manually
(explicitly) with `import`. Consider the following example where we create a
module in a temporary file with its dependencies.


```r
code <- "
import(methods)
import(aoos)
# This is an example:
list : generic(x) %g% standardGeneric('generic')
generic(x ~ ANY) %m% as.list(x)
"

fileName <- tempfile(fileext = ".R")
writeLines(code, fileName)
```

Then we can load such a module into this session by the following:


```r
someModule <- use(fileName)
someModule$generic(1:2)
```

```
## [[1]]
## [1] 1
## 
## [[2]]
## [1] 2
```


# Documentation

If you want proper documentation for your functions or modules you really want a
package. However, there are some simple things you can do for ad-hoc
documentation of modules which is to use comments:


```r
m <- module({
  fun <- function(x) {
    ## A function for illustrating documentation
    ## x (numeric)
    x
  }
})
```

There are print methods for modules and functions within modules:


```r
m
```

```
## fun:
## function(x = )
```

```r
m$fun
```

```
## function(x = )
## ## A function for illustrating documentation
## ## x (numeric)
```


# Modules in Packages

...


# Modules with Object Orientation

## S3

S3 method dispatch will not work because of the special search mechanism of 
`UseMethod`. What will work, however, is attaching the module so `UseMethod` can
find the methods.


```r
m <- module({
  generic <- function(x) UseMethod("generic")
  generic.numeric <- function(x) cat("method for x ~ numeric")
})
# m$generic(1) # this won't work
use(m, attach = TRUE)
m$generic(1)
```

```
## method for x ~ numeric
```

## S4

More reliable is the dispatch in S4. By default the set functions of the methods
package have side effects in the top level environment. So you would have to set
the appropriate environemnt for the argument 'where'. 'aoos' provides syntactic
sugar (S4) and has side effects in the environment in which the constructor
functions are called so method dispatch will work:


```r
m <- module({
  import("methods")
  import("aoos")
  gen(x) %g% cat("default method")
  gen(x ~ numeric) %m% cat("method for x ~ numeric")
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
## method for x ~ numeric
```

S4 classes (or types in aoos) are not working correctly because `setClass` has
side effects in the top level environment which cannot be isolated. If you want
to have class/type definitions you need a package.
