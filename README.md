[![Build Status](https://travis-ci.org/wahani/module.png?branch=master)](https://travis-ci.org/wahani/module)

# Modules in R
Provides a module in R to organise source code around. Modules enforce to be more rigerous when defining dependencies. They are to be used as a sub unit within packages.

## Installation

From this repo:

```r
library(devtools)
install_github("wahani/aoos")
```


```
## Version on CRAN:  
## Development Version: 0.0.1 
## 
## Updates in package NEWS-file since last release to CRAN:
```

```
## Error in (function (query, package = "R", lib.loc = NULL, format = NULL, : invalid query
```

## Modules


```r
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
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 2 21 2 51 21 51 2 2
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x725f390>
```

Modules have their own scope, and have no idea whats going on around them:


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
  functionWithDep <- function() data.table::data.table(x = 1)
})
m$functionWithDep()
```

```
##    x
## 1: 1
```

Or if you like to have more lines or state your dependencies on top:


```r
m <- module({
  import(data.table, data.table)
  functionWithDep <- function() data.table(x = 1)
})
m$functionWithDep()
```

```
##    x
## 1: 1
```

You can also use other modules in that you make all their exported functions
available in your new module definition.


```r
m1 <- module({
  use(.GlobalEnv$m) # normally: pkgName::moduleName
  anotherFunction <- function() functionWithDep()
})
str(m1)
```

```
## List of 1
##  $ anotherFunction:function ()  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 3 22 3 49 22 49 3 3
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x685d9b0>
```

```r
m1$anotherFunction()
```

```
##    x
## 1: 1
```

