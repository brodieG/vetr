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
library(vetr)
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

`vetr` can compare recursive objects such as lists, or data.frames:


```r
tpl.iris <- iris[0, ]      # 0 row DF matches any number of rows in object
iris.fake <- iris
levels(iris.fake$Species)[3] <- "sibirica"   # tweak levels

vet(tpl.iris, iris)
## [1] TRUE
vet(tpl.iris, iris.fake)
## [1] "`levels(iris.fake$Species)[3]` should be \"virginica\" (is \"sibirica\")"
```

From our declared template `iris[0, ]`, `vetr` infers all the required checks.
In this case, `vet(iris[0, ], iris.fake, stop=TRUE)` is equivalent to:


```r
stopifnot_iris <- function(x) {
  stopifnot(
    is.list(x), inherits(x, "data.frame"),
    length(x) == 5, is.integer(attr(x, 'row.names')),
    identical(
      names(x),
      c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
    ),
    all(vapply(x[1:4], is.numeric, logical(1L))),
    typeof(x$Species) == "integer", is.factor(x$Species),
    identical(levels(x$Species), c("setosa", "versicolor", "virginica"))
  )
}
stopifnot_iris(iris.fake)
## Error: identical(levels(x$Species), c("setosa", "versicolor", "virginica")) is not TRUE
```

`vetr` saved us typing, and the time and thought needed to come up with the
things that need to be compared.

You could just as easily have created templates for nested lists, or data frames
in lists.  Templates are compared to objects with the `alike`.  For a
thorough description of templates and how they work see the [`alike`
vignette][2].  For template examples see `example(alike)`.

### Auto-Generated Error Messages

Let's revisit the error message:


```r
vet(tpl.iris, iris.fake)
## [1] "`levels(iris.fake$Species)[3]` should be \"virginica\" (is \"sibirica\")"
```

It tells us:

* The reason for the failure
* What structure would be acceptable instead
* The location of failure `levels(iris.fake$Species)[3]`

`vetr` does what it can to reduce the time from error to resolution.  The
location of failure is generated such that you can easily copy it in part or
full to the R prompt for further examination.

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

Templates only check structure.  When you need to check values use `.` to
refer to the object:


```r
vet(numeric(1L) && . > 0, -42)  # strictly positive scalar numeric
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

There are a number of predefined vetting tokens you can use in your
vetting expressions:


```r
vet(NUM.POS, -runif(5))    # positive numeric
## [1] "`-runif(5)` should contain only positive values, but has negatives"
vet(LGL.1, NA)             # TRUE or FALSE
## [1] "`NA` should not contain NAs, but does"
```

See `?vet_token` for a full listing, and for instructions on how to define your
own tokens with custom error messages.

Vetting expressions are designed to be intuitive to use, but their
implementation is complex.  We recommend you look at `example(vet)` for usage
ideas, or at the ["Non Standard Evaluation" section of the vignette][3] for the
gory details.


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

See [vignette][1] for additional details on how the `vetr` function works.

## Additional Documentation

* [`vetr` vignette][1], `?vet`, `?vetr`, `example(vet)`, `example(vetr)`
* [`alike` vignette][2], `?alike`, and `example(alike)` for discussion of
  templates

## Development Status

`vetr` is still in development, although most of the features are considered
mature.  The most likely area of change is the treatment of function and
language templates (e.g.  `alike(sum, max)`), and more flexible treatment of
list templates (e.g. in future lists may be allowed to be different lengths so
long as every named element in the template exists in the object).

## Installation


```r
install.packages('vetr')
```

Or for the development version:


```r
# install.packages('devtools')
devtools::install_github('brodieg/vetr@development')
```

## Related Packages

<ul>
  <li><a href='https://github.com/egnha/valaddin'>valaddin</a> by Eugene Ha (see
  <a
  href='http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/inst/doc/vetr.html#valaddin'>vignette</a>
  for a more detailed comparison) has very similar objectives to `vetr`

  <li><a href='https://github.com/smbache/ensurer'>ensurer</a> by Stefan M Bache
  allows you to specify contracts for data validation and has an experimental
  implementation of type-safe functions.
  <li><a href='https://github.com/data-cleaning/validate'>validate</a> by Mark van
  der Loo and Edwin de Jonge provides tools for checking data
  <li><a href='https://github.com/jimhester/types'>types</a> by Jim Hester
  provides a mechanism for defining what types arguments should be, though it does
  not directly enforce them
  <li><a href='https://github.com/gaborcsardi/argufy'>argufy</a> by Gábor Csárdi
  adds parameter checks via Roxygen (not published to CRAN)
</ul>

## Acknowledgments

Thank you to:

* R Core for developing and maintaining such a wonderful language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository, and Uwe Ligges in particular for maintaining
  [Winbuilder](http://win-builder.r-project.org/).
* [Jim Hester](https://github.com/jimhester) because
  [covr](https://cran.r-project.org/package=covr) rocks.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
  Csárdi](https://github.com/gaborcsardi) and the R-consortium for
  [Rhub](https://github.com/r-hub/rhub), without which testing bugs on R-devel
  and other platforms would be a nightmare.
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by extension
  John MacFarlane for [pandoc](http://pandoc.org/).
* Stefan M. Bache for the idea of having a function for testing objects directly
  (originally `vetr` only worked with function arguments), which I took from
  ensurer.
* Hadley Wickham for [devtools](https://cran.r-project.org/package=devtools),
  and for pointing me to Stefan M. Bache's ensurer package.
* Olaf Mersmann for [microbenchmark](https://cran.r-project.org/package=microbenchmark), because microsecond matter.
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

[1]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/inst/doc/vetr.html
[2]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/inst/doc/alike.html
[3]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/inst/doc/vetr.html#non-standard-evaluation
[4]: http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/inst/doc/vetr.html#in-functions
