Solving R namespace collision [![Build Status](https://travis-ci.org/robertzk/super.svg?branch=master)](https://travis-ci.org/robertzk/super.svg?branch=master) [![Coverage Status](https://coveralls.io/repos/robertzk/super/badge.svg?branch=master)](https://coveralls.io/r/robertzk/super)
===========

If two packages are loaded which contain namespace collisions (i.e., export a function
with the same name), R has no good strategy for providing the ability to call the
overwritten function. This package aims to solve that problem by defining a `super`
method which tries to find the next appropriate function to call. You should be
familiar with [how R looks for functions](http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/).

For example, consider the following scenario.

```r
# Exported in some attached package.
source <- function(file, ...) {
  cat("Sourcing file ", sQuote(file), "\n")
  base::source(file, ...)
}
```

By explicitly calling `base::source`, this package has effectively monopolized its
interception of the `source` function: if a newly loaded package were to take
the same approach, the first package's work would be permanently undone. Imagine a
second package were attached with the following overwrite.

```r
source <- function(file, ...) {
  if (otherpackage::has_cached(file, ...)) { otherpackage::cache(file, ...) }
  else { base::source(file, ...) }
}
```

Ideally, instead of calling `base::source` directly, it would call the next `source`
function in the attached search path.

To accomplish this, we can instead use `super`:

```r
source <- function(file, ...) {
  if (otherpackage::has_cached(file, ...)) { otherpackage::cache(file, ...) }
  else { super::super(file, ...) }
}
```

By using `super`, `base::source` will be selected if no other package has overwritten
`source` -- otherwise, the `source` exported by the nearest package in the search
path will be selected.

**Note**: Obviously, the function that `super` selects needs to be [commutative](http://en.wikipedia.org/wiki/Commutative_property) 
with the current function. If the attach order in the search path matters, there is no good 
solution as packages in general do not have control over their attachment order. In the
above example, this is already somewhat apparent: if the function which caches the `source`
call is called first, then "sourcing file" will only be printed once. On the other hand,
if the first package is attached closer to the global environment, then "sourcing file" will
be printed even on cache hits.

Installation
============

This package is in development and is not yet available on CRAN (as of February 28, 2015).
To get the latest development build directly from Github, use the following code snippet.

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/super")
```

