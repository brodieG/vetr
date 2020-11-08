<!-- README.md is generated from README.Rmd. Please edit that file 

library(rmarkdown)
render('README.Rmd', output_format=html_vignette(css='vignettes/styles.css'))
render('README.Rmd', output_format=md_document())

-->
vetr
====

[![](https://travis-ci.org/brodieG/vetr.svg?branch=master)](https://travis-ci.org/brodieG/vetr)
[![](https://codecov.io/github/brodieG/vetr/coverage.svg?branch=master)](https://codecov.io/github/brodieG/vetr?branch=master)
[![](http://www.r-pkg.org/badges/version/vetr)](https://cran.r-project.org/package=vetr)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Dependencies
direct/recursive](https://tinyverse.netlify.app/badge/vetr)](https://tinyverse.netlify.app/)

Trust, but Verify
-----------------

### Easily

When you write functions that operate on S3 or unclassed objects you can
either trust that your inputs will be structured as expected, or
tediously check that they are.

`vetr` takes the tedium out of structure verification so that you can
trust, but verify. It lets you express structural requirements
declaratively with templates, and it auto-generates human-friendly error
messages as needed.

### Quickly

`vetr` is written in C to minimize overhead from parameter checks in
your functions. It has no dependencies.

Declarative Checks with Templates
---------------------------------

### Templates

Declare a template that an object should conform to, and let `vetr` take
care of the rest:

    library(vetr)
    tpl <- numeric(1L)
    vet(tpl, 1:3)
    ## [1] "`length(1:3)` should be 1 (is 3)"
    vet(tpl, "hello")
    ## [1] "`\"hello\"` should be type \"numeric\" (is \"character\")"
    vet(tpl, 42)
    ## [1] TRUE

The template concept is based on `vapply`, but generalizes to all S3
objects and adds some special features to facilitate comparison. For
example, zero length templates match any length:

    tpl <- integer()
    vet(tpl, 1L:3L)
    ## [1] TRUE
    vet(tpl, 1L)
    ## [1] TRUE

And for convenience short (&lt;= 100 length) integer-like numerics are
considered integer:

    tpl <- integer(1L)
    vet(tpl, 1)       # this is a numeric, not an integer
    ## [1] TRUE
    vet(tpl, 1.0001)
    ## [1] "`1.0001` should be type \"integer-like\" (is \"double\")"

`vetr` can compare recursive objects such as lists, or data.frames:

    tpl.iris <- iris[0, ]      # 0 row DF matches any number of rows in object
    iris.fake <- iris
    levels(iris.fake$Species)[3] <- "sibirica"   # tweak levels

    vet(tpl.iris, iris)
    ## [1] TRUE
    vet(tpl.iris, iris.fake)
    ## [1] "`levels(iris.fake$Species)[3]` should be \"virginica\" (is \"sibirica\")"

From our declared template `iris[0, ]`, `vetr` infers all the required
checks. In this case, `vet(iris[0, ], iris.fake, stop=TRUE)` is
equivalent to:

    stopifnot_iris <- function(x) {
      stopifnot(
        is.data.frame(x),
        is.list(x),
        length(x) == length(iris),
        identical(lapply(x, class), lapply(iris, class)),
        is.integer(attr(x, 'row.names')),
        identical(names(x), names(iris)),
        identical(typeof(x$Species), "integer"),
        identical(levels(x$Species), levels(iris$Species))
      )
    }
    stopifnot_iris(iris.fake)
    ## Error in stopifnot_iris(iris.fake): identical(levels(x$Species), levels(iris$Species)) is not TRUE

`vetr` saved us typing, and the time and thought needed to come up with
what needs to be compared.

You could just as easily have created templates for nested lists, or
data frames in lists. Templates are compared to objects with the `alike`
function. For a thorough description of templates and how they work see
the [`alike`
vignette](https://cran.r-project.org/package=vetr/vignettes/alike.html).
For template examples see `example(alike)`.

### Auto-Generated Error Messages

Let’s revisit the error message:

    vet(tpl.iris, iris.fake)
    ## [1] "`levels(iris.fake$Species)[3]` should be \"virginica\" (is \"sibirica\")"

It tells us:

-   The reason for the failure
-   What structure would be acceptable instead
-   The location of failure `levels(iris.fake$Species)[3]`

`vetr` does what it can to reduce the time from error to resolution. The
location of failure is generated such that you can easily copy it in
part or full to the R prompt for further examination.

Vetting Expressions
-------------------

You can combine templates with `&&` / `||`:

    vet(numeric(1L) || NULL, NULL)
    ## [1] TRUE
    vet(numeric(1L) || NULL, 42)
    ## [1] TRUE
    vet(numeric(1L) || NULL, "foo")
    ## [1] "`\"foo\"` should be `NULL`, or type \"numeric\" (is \"character\")"

Templates only check structure. When you need to check values use `.` to
refer to the object:

    vet(numeric(1L) && . > 0, -42)  # strictly positive scalar numeric
    ## [1] "`-42 > 0` is not TRUE (FALSE)"
    vet(numeric(1L) && . > 0, 42)
    ## [1] TRUE

You can compose vetting expressions as language objects and combine
them:

    scalar.num.pos <- quote(numeric(1L) && . > 0)
    foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
    vet.exp <- quote(scalar.num.pos || foo.or.bar)

    vet(vet.exp, 42)
    ## [1] TRUE
    vet(vet.exp, "foo")
    ## [1] TRUE
    vet(vet.exp, "baz")
    ## [1] "At least one of these should pass:"                         
    ## [2] "  - `\"baz\" %in% c(\"foo\", \"bar\")` is not TRUE (FALSE)" 
    ## [3] "  - `\"baz\"` should be type \"numeric\" (is \"character\")"

`all_bw` is available for value range checks (~10x faster than
`isTRUE(all(. >= x & . <= y))` for large vectors):

    vet(all_bw(., 0, 1), runif(5) + 1)
    ## [1] "`all_bw(runif(5) + 1, 0, 1)` is not TRUE (is chr: \"`1.369447` at index 1 not in `[0,1]`\")"

There are a number of predefined vetting tokens you can use in your
vetting expressions such as:

    vet(NUM.POS, -runif(5))    # positive numeric; see `?vet_token` for others
    ## [1] "`-runif(5)` should contain only positive values, but has negatives"

Vetting expressions are designed to be intuitive to use, but their
implementation is complex. We recommend you look at `example(vet)` for
usage ideas, or at the [“Non Standard Evaluation” section of the
vignette](https://cran.r-project.org/package=vetr/vignettes/vetr.html#non-standard-evaluation)
for the gory details.

`vetr` in Functions
-------------------

If you are vetting function inputs, you can use the `vetr` function,
which works just like `vet` except that it is streamlined for use within
functions:

    fun <- function(x, y) {
      vetr(numeric(1L), logical(1L))
      TRUE   # do work...
    }
    fun(1:2, "foo")
    ## Error in fun(x = 1:2, y = "foo"): For argument `x`, `length(1:2)` should be 1 (is 2)
    fun(1, "foo")
    ## Error in fun(x = 1, y = "foo"): For argument `y`, `"foo"` should be type "logical" (is "character")

`vetr` automatically matches the vetting expressions to the
corresponding arguments and fetches the argument values from the
function environment.

See
[vignette](https://cran.r-project.org/package=vetr/vignettes/vetr.html#in-functions)
for additional details on how the `vetr` function works.

Additional Documentation
------------------------

-   [`vetr`
    vignette](https://cran.r-project.org/package=vetr/vignettes/vetr.html),
    `?vet`, `?vetr`, `example(vet)`, `example(vetr)`.
-   [`alike`
    vignette](https://cran.r-project.org/package=vetr/vignettes/alike.html),
    `?alike`, and `example(alike)` for discussion of templates.
-   A survey of [parameter validation
    functions](http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/extra/compare.html).

Development Status
------------------

`vetr` is still in development, although most of the features are
considered mature. The most likely area of change is the treatment of
function and language templates (e.g. `alike(sum, max)`), and more
flexible treatment of list templates (e.g. in future lists may be
allowed to be different lengths so long as every named element in the
template exists in the object).

Installation
------------

This package is available on CRAN:

    install.packages('vetr')

It has no runtime dependencies.

For the development version use
`remotes::install_github('brodieg/vetr@development')` or:

    f.dl <- tempfile()
    f.uz <- tempfile()
    github.url <- 'https://github.com/brodieG/vetr/archive/development.zip'
    download.file(github.url, f.dl)
    unzip(f.dl, exdir=f.uz)
    install.packages(file.path(f.uz, 'vetr-development'), repos=NULL, type='source')
    unlink(c(f.dl, f.uz))

(Travis build status:
[![](https://travis-ci.org/brodieG/vetr.svg?branch=development)](https://travis-ci.org/brodieG/vetr)).
The master branch typically mirrors CRAN and should be stable.

Alternatives
------------

There are many alternatives available to `vetr`. We do a survey of the
following in our [parameter validation
functions](http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/extra/compare.html)
review:

-   `stopifnot` by R Core
-   [`vetr`](https://github.com/brodieG/vetr) by Yours Truly
-   [`asserthat`](https://github.com/hadley/assertthat) by Hadley
    Wickham
-   [`assertive`](https://www.r-pkg.org/pkg/assertive) by Richie Cotton
-   [`checkmate`](https://github.com/mllg/checkmate) by Michel Lang

The following packages also perform related tasks, although we do not
review them:

-   [`valaddin`](https://github.com/egnha/valaddin) v0.1.0 by Eugene Ha,
    a framework for augmenting existing functions with validation
    contracts. Currently the package is undergoing a major overhaul so
    we will add it to the comparison once the new release (v0.3.0) is
    out.
-   [`ensurer`](https://github.com/smbache/ensurer) v1.1 by Stefan M.
    Bache, a framework for flexibly creating and combining validation
    contracts. The development version adds an experimental method for
    creating type safe functions, but it is not published to CRAN so we
    do not test it here.
-   [`validate`](https://github.com/data-cleaning/validate) by Mark van
    der Loo and Edwin de Jonge, with a primary focus on validating data
    in data frames and similar data structures.
-   [`assertr`](https://github.com/ropensci/assertr) by Tony Fischetti,
    also focused on data validation in data frames and similar
    structures.
-   [`types`](https://github.com/jimhester/types) by Jim Hester, which
    implements but does not enforce type hinting.
-   [`argufy`](https://github.com/gaborcsardi/argufy) by Gábor Csárdi,
    which implements parameter validation via roxygen tags (not released
    to CRAN).

Acknowledgments
---------------

Thank you to:

-   R Core for developing and maintaining such a wonderful language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository, and Uwe Ligges in particular for
    maintaining [Winbuilder](http://win-builder.r-project.org/).
-   Users and others who have reported bugs and/or helped contribute
    fixes (see NEWS.md).
-   Tomas Kalibera for [rchk](https://github.com/kalibera/rchk) and
    rcnst to help detect errors in compiled code, and in particular for
    his infinite patience in helping me resolve the issues he identified
    for me.
-   [Jim Hester](https://github.com/jimhester) because
    [covr](https://cran.r-project.org/package=covr) rocks.
-   [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
    Boettiger](https://github.com/cboettig) for the
    [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
    Csárdi](https://github.com/gaborcsardi) and the
    [R-consortium](https://www.r-consortium.org/) for
    [Rhub](https://github.com/r-hub), without which testing bugs on
    R-devel and other platforms would be a nightmare.
-   [Winston Chang](https://github.com/wch) for the
    [r-debug](https://hub.docker.com/r/wch1/r-debug/) docker container,
    in particular because of the valgrind level 2 instrumented version
    of R.
-   [Hadley Wickham](https://github.com/hadley/) for
    [devtools](https://cran.r-project.org/package=devtools) and with
    [Peter Danenberg](https://github.com/klutometis) for
    [roxygen2](https://cran.r-project.org/package=roxygen2).
-   [Yihui Xie](https://github.com/yihui) for
    [knitr](https://cran.r-project.org/package=knitr) and [J.J.
    Allaire](https://github.com/jjallaire) etal for
    [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by
    extension John MacFarlane for [pandoc](https://pandoc.org/).
-   [Michel Lang](https://github.com/mllg) for pushing me to implement
    `all_bw` to compete with his own package
    [`checkmate`](https://cran.r-project.org/package=checkmate).
-   [Eugene Ha](https://github.com/egnha) for pointing me to several
    other relevant packages, which in turn led to the [survey of related
    packages](http://htmlpreview.github.io/?https://github.com/brodieG/vetr/blob/master/extra/compare.html).
-   Stefan M. Bache for the idea of having a function for testing
    objects directly (originally `vetr` only worked with function
    arguments), which I took from ensurer.
-   Olaf Mersmann for
    [microbenchmark](https://cran.r-project.org/package=microbenchmark),
    because microsecond matter, and [Joshua
    Ulrich](https://github.com/joshuaulrich) for making it lightweight.
-   All open source developers out there that make their work freely
    available for others to use.
-   [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
    [Codecov](https://codecov.io/),
    [Vagrant](https://www.vagrantup.com/),
    [Docker](https://www.docker.com/), [Ubuntu](https://ubuntu.com/),
    [Brew](https://brew.sh/) for providing infrastructure that greatly
    simplifies open source development.
-   [Free Software Foundation](https://www.fsf.org/) for developing the
    GPL license and promotion of the free software movement.

About the Author
----------------

Brodie Gaslam is a hobbyist programmer based on the US East Coast.
