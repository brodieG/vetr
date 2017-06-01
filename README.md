<!-- README.md is generated from README.Rmd. Please edit that file -->


# vetr - Trust, but Verify

[![](https://travis-ci.org/brodieG/vetr.svg?branch=master)](https://travis-ci.org/brodieG/vetr)
[![](https://codecov.io/github/brodieG/vetr/coverage.svg?branch=master)](https://codecov.io/github/brodieG/vetr?branch=master)
[![](http://www.r-pkg.org/badges/version/vetr)](https://cran.r-project.org/package=vetr)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)


## Trust, but Verify

### Easily

When you write functions that operate on  S3 or unclassed objects you can either
trust that your inputs will be structured as expected, or tediously check that
they are.

`vetr` takes the tedium out of structure verification, so that you can trust,
but verify.  It lets you express structural requirements declaratively with
templates, and it auto-generates human-friendly error messages as needed.

### Quickly

`vetr` is written in C to minimize overhead from parameter checks in your
functions.  It has no dependencies.


## Declarative Checks with Templates

### Templates

Declare a template that an object should conform to, and let `vetr` take care of
the rest:


```r
tpl <- numeric(1L)
vet(tpl, 1:3)
## [1] "`1:3` should be length 1 (is 3)"
vet(tpl, "hello")
## [1] "`\"hello\"` should be type \"numeric\" (is \"character\")"
vet(tpl, 42)
## [1] TRUE
```

Zero length templates match any length:


```r
tpl <- integer()
vet(tpl, 1L:3L)
## [1] TRUE
vet(tpl, 1L)
## [1] TRUE
```

And for convenience short (<= 100 length) integer-like numerics are considered
integer:


```r
tpl <- integer(1L)
vet(tpl, 1)       # this is a numeric, not an integer
## [1] TRUE
vet(tpl, 1.0001)
## [1] "`1.0001` should be type \"integer-like\" (is \"double\")"
```

`vetr` can compare recursive objects, such as lists, data.frames,
data.frames in lists, and more:


```r
# Copies of built-in iris data set, one with corrupted levels

iris1 <- iris2 <- iris[1:10, ]
levels(iris2$Species)[3] <- "sibirica"

# Nest them in a list (NB: 0-row DF in template matches any number of rows):

tpl.nested <- list(a=character(1L), b=iris[0, ])   # 0-row iris
obj.nested1 <- list(a="I", b=iris1)
obj.nested2 <- list(a="I", b=iris2)

# And vet:

vet(tpl.nested, obj.nested1)
## [1] TRUE
vet(tpl.nested, obj.nested2)
## [1] "`levels(obj.nested2$b$Species)[3]` should be \"virginica\" (is \"sibirica\")"
```

Our template `list(a=character(1L), b=iris[0, ])` is equivalent to:


```r
stopifnot(
  is.list(obj.nested2), length(obj.nested2) == 2,
  identical(names(obj.nested2), c("a", "b")),
  is.character(obj.nested2$a), length(obj.nested2$a) == 1L,
  is.list(obj.nested2$b), inherits(obj.nested2$b, "data.frame"),
  length(obj.nested2$b) == 5, is.integer(attr(obj.nested2$b, 'row.names')),
  all(vapply(obj.nested2$b[1:4], is.numeric, logical(1L))),
  identical(
    names(obj.nested2$b),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  ),
  is.factor(obj.nested2$b$Species),
  identical(
    levels(obj.nested2$b$Species), c("setosa", "versicolor", "virginica")
  )
)
## Error: identical(levels(obj.nested2$b$Species), c("setosa", "versicolor",  .... is not TRUE
```

From the one line template `vetr` figures out all the required comparisons of
nested objects and attributes so that you do not have to.

### Auto-Generated Error Messages

Let's revisit the error message:


```r
vet(tpl.nested, obj.nested2)
## [1] "`levels(obj.nested2$b$Species)[3]` should be \"virginica\" (is \"sibirica\")"
```

It tells us:

* The reason for the failure
* What structure would be acceptable instead
* The location of failure `levels(obj.nested2$b$Species)[3]`

`vetr` does what it can to reduce the time from error to resolution.  Notice
that the location of failure is written so that you can easily copy it in part
or full to the R prompt for further examination.

## Vetting Expressions

You can combine templates with `&&` / `||`:


```r
vet(numeric(1L) || NULL, NULL)
## [1] TRUE
vet(numeric(1L) || NULL, 42)
## [1] TRUE
vet(numeric(1L) || NULL, "foo")
## [1] "`\"foo\"` should be \"NULL\", or type \"numeric\" (is \"character\")"
```

When you need to check values use `.` to reference the object:


```r
vet(numeric(1L) && . > 0, -42)
## [1] "`-42 > 0` is not TRUE (FALSE)"
vet(numeric(1L) && . > 0, 42)
## [1] TRUE
```

You can compose vetting expressions as language objects and combine them:


```r
scalar.num.pos <- quote(numeric(1L) && . > 0)
foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
vet.exp <- quote(scalar.num.pos || foo.or.bar)

vet(vet.exp, 42)
## [1] TRUE
vet(vet.exp, "foo")
## [1] TRUE
vet(vet.exp, "baz")
## [1] "At least one of these should pass:"                         
## [2] "  - `\"baz\"` should be type \"numeric\" (is \"character\")"
## [3] "  - `\"baz\" %in% c(\"foo\", \"bar\")` is not TRUE (FALSE)"
```

See [vignette][1] for additional details.

## `vetr` in Functions

If you are vetting function inputs, you can use the `vetr` function, which works
just like `vet` except that it is streamlined for use within functions:


```r
fun <- function(x, y) {
  vetr(numeric(1L), logical(1L))
  TRUE   # do work...
}
fun(1:2, "foo")
## Error in fun(x = 1:2, y = "foo"): For argument `x`, `1:2` should be length 1 (is 2)
fun(1, "foo")
## Error in fun(x = 1, y = "foo"): For argument `y`, `"foo"` should be type "logical" (is "character")
```

`vetr` automatically matches the vetting expressions to the corresponding
arguments and fetches the argument values from the function environment.

## Additional Documentation

* [`vetr` vignette][1]
* [`alike` vignette][2] for discussion of templates

## Installation

`vetr` is available on CRAN.


```r
install.packages('vetr')
```

## Related Packages

* [valaddin](https://github.com/egnha/valaddin) by Eugene Ha (see vignette for a
  more detailed comparison) has very similar objectives to `vetr`
* [ensurer](https://github.com/smbache/ensurer) by Stefan M Bache allows you to
  specify contracts for data validation and has an experimental implementation
  of type-safe functions.
* [validate](https://github.com/data-cleaning/validate) by Mark van der Loo and
  Edwin de Jonge provides tools for checking data
* [types](https://github.com/jimhester/types) by Jim Hester provides a mechanism
  for defining what types arguments should be, though it does not directly
  enforce them
* [argufy](https://github.com/gaborcsardi/argufy) by Gábor Csárdi adds
  parameter checks via Roxygen (not published to CRAN)

## Acknowledgments

Thank you to:

* R Core for developing such a wonderfully flexible language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository
* [Jim Hester](https://github.com/jimhester) because
  [covr](https://cran.r-project.org/package=covr) rocks.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, without which testing
  bugs on R-devel would be a nightmare.
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown).
* Stefan M. Bache for the idea of having a function for testing objects directly
  (originally `vetr` only worked with function arguments), which I took from
  ensurer.
* Hadley Wickham for [devtools](https://cran.r-project.org/package=devtools),
  and for pointing me to Stefan M. Bache's ensurer package.
* All open source developers out there that make their work freely available
  for others to use.
* [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
  [Codecov](https://codecov.io/), [Vagrant](https://www.vagrantup.com/),
  [Docker](https://www.docker.com/), [Ubuntu](https://www.ubuntu.com/),
  [Brew](https://brew.sh/) for providing infrastructure that greatly simplifies
  open source development.
* [Free Software Foundation](http://fsf.org/) for developing the GPL license and
  promotion of the free software movement.

## About the Author

Brodie Gaslam is a hobbyist programmer based on the US East Coast.

[1]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/development/inst/doc/vetr.html
[2]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/development/inst/doc/alike.html
