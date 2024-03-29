[![Build Status](https://github.com/wahani/modules/actions/workflows/rcmdcheck.yml/badge.svg?branch=main)](https://github.com/wahani/modules/actions)
[![codecov.io](https://codecov.io/github/wahani/modules/coverage.svg?branch=master)](https://codecov.io/github/wahani/modules?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/modules)](https://cran.r-project.org/package=modules)
![Downloads](http://cranlogs.r-pkg.org/badges/modules)
# Modules in R

Provides modules as an organizational unit for source code. Modules enforce to be more rigorous when defining dependencies and have a local search path. They can be used as a sub unit within packages or in scripts.

## Installation

From CRAN:

```r
install.packages("modules")
```

From GitHub:


```r
if (require("devtools")) install_github("wahani/modules")
```

# Introduction

The key idea of this package is to provide a unit of source code which has it's
own scope. The main and most reliable infrastructure for such organizational
units in the R ecosystem is a package. Modules can be used as stand alone,
ad-hoc substitutes for a package or as a sub-unit within a package.

When modules are defined inside of packages they act as bags of functions (like
objects as in object-oriented-programming). Outside of packages modules define
entities which only know of the base environment, i.e. within a module the base
environment is the only *package* on the *search path*. Also they are always
represented as a list inside R.

## Scoping of modules

We can create a module using the `modules::module` function. A module is similar
to a function definition; it comprises:

- the body of the module
- the environment in which it is created (defined implicitly)
- the environment used for the search path, in most cases `baseenv()` (defined
implicitly)

Similar to a function you may supply arguments to a module; see the vignette on
modules as objects on this topic.

To illustrate the very basic functionality of a module, consider the following
example:


```r
library("modules")
m <- module({
  foo <- function() "foo"
})
m$foo()
```

```
## [1] "foo"
```

Here `m` is the collection of objects created inside the module. This is a
`list` with the function `foo` as only element. We can do the same thing and define a module in a separate file:

**module.R**

```
foo <- function() "foo"
```

**main.R**


```r
m <- modules::use("module.R")
m$foo()
```

```
## [1] "foo"
```

The two examples illustrate the two ways in which modules can be constructed.
Since modules are isolated from the `.GlobalEnv` the following object `x` can
not be found:


```r
x <- "hey"
m <- module({
  someFunction <- function() x
})
m$someFunction()
```

```
## Error in m$someFunction(): object 'x' not found
```

```r
getSearchPathContent(m)
```

```
## List of 4
##  $ modules:root     : chr "someFunction"
##  $ modules:internals: chr [1:9] "attach" "depend" "export" "expose" ...
##  $ base             : chr [1:1244] "-" "-.Date" "-.POSIXt" ":" ...
##  $ R_EmptyEnv       : chr(0) 
##  - attr(*, "class")= chr [1:2] "SearchPathContent" "list"
```

Two features of modules are important at this point:

- We can keep the global workspace clean, by introducing a local scope
- We have no direct access to the global environment from modules by default,
enforcing discipline when using any form of dependency (objects and packages)

The following subsections explain how to work with these two features.

## Imports

If you rely on exported objects of a package you can refer to them explicitly
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

Or you can use `import` for *attaching* single objects or packages. Import acts as a substitute for `library` with an important difference: `library` has the side effect of changing the search path of the complete R session. `import` only changes the search path of the calling environment, i.e. the side effect is local to the module and does not affect the global state of the R session.


```r
m <- module({

  import("stats", "median") # make median from package stats available

  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

```r
getSearchPathContent(m)
```

```
## List of 5
##  $ modules:root     : chr "functionWithDep"
##  $ modules:stats    : chr "median"
##  $ modules:internals: chr [1:9] "attach" "depend" "export" "expose" ...
##  $ base             : chr [1:1244] "-" "-.Date" "-.POSIXt" ":" ...
##  $ R_EmptyEnv       : chr(0) 
##  - attr(*, "class")= chr [1:2] "SearchPathContent" "list"
```


```r
m <- module({

  import("stats")

  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

## Importing modules

To *import* other modules, the function `use` can be called. *use* really just means *import module*. With `use` we can load modules:

- defined in the calling environment of the module definition
- or defined in files or folders (see the corresponding vignette on this topic)

Consider the following example:


```r
mm <- module({
  m <- use(m)
  anotherFunction <- function(x) m$functionWithDep(x)
})
mm$anotherFunction(1:10)
```

```
## [1] 5.5
```

To load modules from a file we can refer to the file directly:


```r
module({
  m <- use("someFile.R")
  # ...
})
```

## Exports

Modules can help to isolate code from the state of the global environment. Now
we may have reduced the complexity in our global environment and moved it into a
module. However, to make it very obvious which parts of a module should be used
we can also define exports. Every non-exported object will not be accessible.

Properties of exports are:

- You can list the names of objects in a call to `export`.
- Exports stack up: you can have multiple calls to `export` in a module
definition, i.e. directly in front of each function you want to export.
- Exports can be defined as regular expressions which is indicated by a leading
'^'. In this case only one export declaration should be used.


```r
m <- module({

  export("fun")

  fun <- identity # public
  privateFunction <- identity

  # .named are always private
  .privateFunction <- identity

})

m
```

```
## fun:
## function(x)
```

# Example: Modules as Parallel Process

One example where you may want to have more control of the enclosing environment
of a function is when you parallelize your code. First consider the case when a
*naive* implementation fails.


```r
library("parallel")
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

Note that the parallel computing facilities in `R` always provide a way to
handle such situations. Here it is just a matter of organization if you believe
the function itself should handle its dependencies or the parallel interface.


# Related Projects

There exist several projects with similar goals. First of all, the package
[klmr/modules](https://github.com/klmr/modules) aims at providing a unit similar
to what [Python](https://www.python.org/)-modules are. This project is obviously
interesting for you when you have prior knowledge in Python. `klmr/modules`
modules aim for a full replacement of R-packages. Otherwise there is
considerable overlap of features between the two packages.

Second you may be interested in
[import](https://cran.r-project.org/package=import) which provides convenient
syntax for stating dependencies in script files. This is something which is also
covered here, although, when you are only interested in a replacement for
`library` the package `import` is more focused.

`modules` in this package can act as objects as in object-orientation. In
contrast to [R6](https://cran.r-project.org/package=R6) and reference classes
implemented in the methods package here these objects are immutable by default.
Furthermore it is not being made easy to change state of a module; but it is not
difficult to do that if you really want to: see the section on coupling below.
Furthermore inheritance is not a feature, instead you have various possibilities
for object composition.

The development of the `modules` package has been inspired by other languages:
[F#](https://fsharpforfunandprofit.com/posts/organizing-functions/),
[Erlang](https://learnyousomeerlang.com/modules/) and
[julia](https://docs.julialang.org/en/v1/manual/modules/index.html).


