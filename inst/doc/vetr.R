## ----global_options, echo=FALSE------------------------------------------
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(vetr)

## ------------------------------------------------------------------------
library(vetr)
tpl <- numeric(1L)
vet(tpl, 1:3)
vet(tpl, "hello")
vet(tpl, 42)

## ------------------------------------------------------------------------
tpl <- integer()
vet(tpl, 1L:3L)
vet(tpl, 1L)

## ------------------------------------------------------------------------
tpl <- integer(1L)
vet(tpl, 1)       # this is a numeric, not an integer
vet(tpl, 1.0001)

## ------------------------------------------------------------------------
tpl.iris <- iris[0, ]      # 0 row DF matches any number of rows in object
iris.fake <- iris
levels(iris.fake$Species)[3] <- "sibirica"   # tweak levels

vet(tpl.iris, iris)
vet(tpl.iris, iris.fake)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
vet(tpl.iris, iris.fake)

## ------------------------------------------------------------------------
vet(numeric(1L) || NULL, NULL)
vet(numeric(1L) || NULL, 42)
vet(numeric(1L) || NULL, "foo")

## ------------------------------------------------------------------------
vet(numeric(1L) && . > 0, -42)  # strictly positive scalar numeric
vet(numeric(1L) && . > 0, 42)

## ------------------------------------------------------------------------
scalar.num.pos <- quote(numeric(1L) && . > 0)
foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
vet.exp <- quote(scalar.num.pos || foo.or.bar)

vet(vet.exp, 42)
vet(vet.exp, "foo")
vet(vet.exp, "baz")

## ------------------------------------------------------------------------
vet(all_bw(., 0, 1), runif(5) + 1)

## ------------------------------------------------------------------------
vet(NUM.POS, -runif(5))    # positive numeric; see `?vet_token` for others

## ---- eval=FALSE---------------------------------------------------------
#  vet(. > 0, 1:3)

## ---- eval=FALSE---------------------------------------------------------
#  a <- quote(integer() && . > 0)
#  b <- quote(logical(1L) && !is.na(.))
#  c <- quote(a || b)
#  
#  vet(c, 1:3)

## ---- eval=FALSE---------------------------------------------------------
#  vet((integer() && . > 0) || (logical(1L) && !is.na(.)), 1:3)

## ---- eval=FALSE---------------------------------------------------------
#  vet(quote(x + y), my.call)       # notice `quote`

## ---- eval=FALSE---------------------------------------------------------
#  tpl.call <- quote(quote(x + y))  # notice `quote(quote(...))`
#  vet(tpl.call, my.call)

## ---- eval=FALSE---------------------------------------------------------
#  logical(1) || (numeric(1) && (. > 0 & . < 1))

## ------------------------------------------------------------------------
vet(. > 0, 1:3)

## ---- eval=FALSE---------------------------------------------------------
#  vet(logical(1) || (numeric(1) && (. > 0 & . < 1)), 42)
#  # becomes:
#  alike(logical(1L), 42) || (alike(numeric(1L), 42) && all(42 > 0 & 42 < 1))
#  # becomes:
#  FALSE || (TRUE && FALSE)
#  # becomes:
#  FALSE

## ------------------------------------------------------------------------
vet(logical(1) || (numeric(1) && (. > 0 & . < 1)), 42)

## ---- eval=FALSE---------------------------------------------------------
#  I(length(a) == length(b) && . %in% 0:1)

## ---- eval=FALSE---------------------------------------------------------
#  I(logical(1L) && my_special_fun(.))

## ------------------------------------------------------------------------
fun <- function(x, y, z) {
  vetr(
    matrix(numeric(), ncol=3),
    logical(1L),
    character(1L) && . %in% c("foo", "bar")
  )
  TRUE  # do work...
}
fun(matrix(1:12, 3), TRUE, "baz")
fun(matrix(1:12, 4), TRUE, "baz")
fun(matrix(1:12, 4), TRUE, "foo")

## ------------------------------------------------------------------------
fun <- function(x, y, z) {
  vetr(z=character(1L) && . %in% c("foo", "bar"))
  TRUE  # do work...
}
fun(matrix(1:12, 3), TRUE, "baz")
fun(matrix(1:12, 4), TRUE, "bar")

## ------------------------------------------------------------------------
vetr_iris <- function(x) vetr(tpl.iris)

bench_mark(times=1e4,
  vet(tpl.iris, iris),
  vetr_iris(iris),
  stopifnot_iris(iris)   # defined in "Templates" section
)

## ------------------------------------------------------------------------
bench_mark(data.frame(a=numeric()))

## ------------------------------------------------------------------------
df.tpl <- data.frame(a=numeric())

my_fun <- function(x) {
  vetr(x=df.tpl)
  TRUE    # do work
}

