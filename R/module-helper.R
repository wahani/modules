# helper:

deleteQuotes <- function(x) {
  gsub("\\\"|\\\'", "", x)
}

nameExports <- function() ".__exports__"
