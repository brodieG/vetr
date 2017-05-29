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

It worked!!!!

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

`vetr` supports complex recursive templates:


```r
tpl <- list(numeric(1L), list(dat=matrix(numeric(), 3), mode=character(1L)))
```

That can be used to vet complex objects:


```r
## Make some objects
obj1 <- list(42, list(cbind(1:5, 1:5, 1:5), "foo"))          # missing names
obj2 <- list(42, list(dat=cbind(1:5, 1:5, 1:5), mode="foo")) # dat transposed
obj3 <- list(42, list(dat=rbind(1:5, 1:5, 1:5), mode="foo")) # correct
## Vet them:
vet(tpl, obj1)
## [1] "`names(obj1[[2]])` should be type \"character\" (is \"NULL\")"
vet(tpl, obj2)
## [1] "`obj2[[2]]$dat` should have 3 rows (has 5)"
vet(tpl, obj3)
## [1] TRUE
```

The auto-generated error message tells you:

* what the (sub-)object should be
* what it is
* the exact point of failure (e.g. `obj2[[2]]$dat`)

The point of failure is expressed such that you can copy and paste it into the
prompt to directly examine the problem.

### Vetting Expressions

You can augment templates with custom vetting tokens to check values with `.`:


```r
vet(numeric(1L) && . > 0, -42)
## [1] "`-42 > 0` is not TRUE (FALSE)"
vet(numeric(1L) && . > 0, 42)
## [1] TRUE
```

And you can compose vetting expressions as language objects and combine them:


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

## Vetting Function Parameters

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
