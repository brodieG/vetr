## ----global_options, echo=FALSE------------------------------------------
knitr::opts_chunk$set(error=TRUE)
library(vetr)

## ----echo=FALSE----------------------------------------------------------
mb <- function(...) {
  if(require(microbenchmark, quietly=TRUE)) {
    mb.c <- match.call()
    mb.c[[1]] <- quote(microbenchmark::microbenchmark)
    res <- eval(mb.c, parent.frame())
    res.sum <- summary(res)
    cat(attr(res.sum, "unit"), "\n")
    print(res.sum[1:4])
  } else {
    warning("Package microbenchmark not available.")
  }
}

## ------------------------------------------------------------------------
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
vet_stopifnot <- function(x) {
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
vet_stopifnot(iris.fake)

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
vet(NUM.POS, -runif(5))    # positive numeric
vet(LGL.1, NA)             # TRUE or FALSE

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
#  alike(logical(1L), 42) || (alike(numeric(1L)) && all(42 > 0 & 42 < 1))
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
fun_w_vetr <- function(x) vetr(tpl.iris)

mb(  # wrapper around microbenchmark
  vet(tpl.iris, iris),
  fun_w_vetr(iris),
  vet_stopifnot(iris)
)

## ------------------------------------------------------------------------
mb(data.frame(a=numeric()))

## ------------------------------------------------------------------------
secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx

if(require(valaddin, quietly=TRUE)) {
  secant_valaddin <- valaddin::firmly(secant, list(~x, ~dx) ~ is.numeric)
} else {
  secant_valaddin <- function(...) warning("valaddin not available.\n")
}

secant_stopifnot <- function(f, x, dx) {
  stopifnot(is.numeric(x), is.numeric(dx))
  secant(f, x, dx)
}
secant_vetr <- function(f, x, dx) {
  vetr(x=numeric(), dx=numeric())
  secant(f, x, dx)
}

mb(
  secant_valaddin(log, 1, .1),
  secant_stopifnot(log, 1, .1),
  secant_vetr(log, 1, .1)
)

