## Possible
#library(ggplot2)

## But would be better:
#import::here(qplot, .from = ggplot2)

import::from("ggplot2", "qplot")

## Note this operator overload is not something you want to `source`!
`+` <- function(e1, e2)
  paste(e1, e2)

## Some function relying on the above overload:
a <- function(s1, s2)
  s1 + rep(s2, 3)

## Another value.
b <- head(iris, 10)

## A value created using a function exposed by attachment
p <- qplot(Sepal.Length, Sepal.Width, data = iris, color = Species)

## A function relying on a function exposed through attachment:
plot_it <- function()
  qplot(Sepal.Length, Sepal.Width, data = iris, color = Species)

test <- function() {
  qplot(Sepal.Length, Sepal.Width, data = iris, color = Species)
}
