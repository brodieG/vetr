<!-- README.md is generated from README.Rmd. Please edit that file -->


# vetr - Keep the Garbage Out

[![](https://travis-ci.org/brodieG/vetr.svg?branch=master)](https://travis-ci.org/brodieG/vetr)
[![](https://codecov.io/github/brodieG/vetr/coverage.svg?branch=master)](https://codecov.io/github/brodieG/vetr?branch=master)
[![](http://www.r-pkg.org/badges/version/vetr)](https://cran.r-project.org/package=vetr)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

## Garbage In...

R is flexible about data structures so any user-facing code you write should vet
inputs.  If you enforce structural requirements for function parameters your
code will be more robust.  It will also be easier to use since errors will be
reported by documented functions, not from deep in the bowels of your package.

`vetr` takes the tedium out of comprehensive vetting by allowing you to express
structural requirements in a declarative style with templates, and by
auto-generating human-friendly error messages.  It is written in C to minimize
the overhead of parameter checks in your functions.  It has no dependencies.

## Declarative Checks with Templates

Declare a template that an object should conform to, and let `vetr` take care of
the rest:


```r
vet(numeric(1L), 1:3)
## [1] "`1:3` should be length 1 (is 3)"
vet(numeric(1L), "hello")
## [1] "`\"hello\"` should be type \"numeric\" (is \"character\")"
vet(numeric(1L), 42)
## [1] TRUE
```

`vetr` supports complex recursive templates:


```r
tpl <- list(numeric(1L), list(dat=matrix(numeric(), 3), mode=character(1L)))
```

Let's make three objects that are supposed to conform to `tpl`:


```r
obj1 <- list(42, list(cbind(1:5, 1:5, 1:5), letters))
obj2 <- list(42, list(dat=cbind(1:5, 1:5, 1:5), mode=letters))
obj3 <- list(42, list(dat=rbind(1:5, 1:5, 1:5), mode="foo"))
```

And let's vet:


```r
vet(tpl, obj1)
## [1] "`names(obj1[[2]])` should be type \"character\" (is \"NULL\")"
vet(tpl, obj2)
## [1] "`obj2[[2]]$dat` should have 3 rows (has 5)"
vet(tpl, obj3)
## [1] TRUE
```

The auto-generated error message tells you:

* What the object (or sub-object) should be
* What it is
* What expression to use to find the exact location of the failure (e.g.
  `obj2[[2]]$dat`

While the message is not in plain English, it should be readily interpretable 

You can augment templates with custom vetting tokens to check values in addition
to structure:


```r
vet(numeric(1L) && . > 0, 1:3)
## [1] "`1:3` should be length 1 (is 3)"
vet(numeric(1L) && . > 0, -42)
## [1] "`-42 > 0` is not TRUE (FALSE)"
vet(numeric(1L) && . > 0, 42)
## [1] TRUE
```

And you can compose vetting expressions as language objects and combine them:


```r
scalar.num.pos <- quote(numeric(1L) && . > 0)
foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
vet.exp <- quote(NULL || scalar.num.pos || foo.or.bar)

vet(vet.exp, 42)
## [1] TRUE
vet(vet.exp, NULL)
## [1] TRUE
vet(vet.exp, "foo")
## [1] TRUE
vet(vet.exp, "baz")
## [1] "At least one of these should pass:"                                      
## [2] "  - `\"baz\" %in% c(\"foo\", \"bar\")` is not TRUE (FALSE)"              
## [3] "  - `\"baz\"` should be \"NULL\", or type \"numeric\" (is \"character\")"
```

See vignette for additional details.

## Streamlined Function Parameter Checks

If you are vetting function inputs, you can use the `vetr` function, which works
just like `vet` except that is streamlined for use within functions:


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

## Additional Documentation

* `vetr` vignette
* `alike` vignette for discussion of templates

## Installation

`vetr` is available on CRAN.  It has no dependencies.


```r
install.packages('vetr')
vignette('vetr', package='vetr')
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

## Acknowledgements


