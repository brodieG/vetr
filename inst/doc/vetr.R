## ----global_options, echo=FALSE---------------------------
knitr::opts_chunk$set(error=TRUE, hilang='r')
options(width=60)
library(vetr)

## ---------------------------------------------------------
tpl <- numeric(1L)
vet(tpl, 1:3)
vet(tpl, "hello")
vet(tpl, 42)

## ---------------------------------------------------------
tpl <- integer()
vet(tpl, 1L:3L)
vet(tpl, 1L)

## ---------------------------------------------------------
tpl <- integer(1L)
vet(tpl, 1)       # this is a numeric, not an integer
vet(tpl, 1.0001)

## ---------------------------------------------------------
tpl.iris <- iris[0, ]      # 0 row DF matches any number of rows in object
iris.fake <- iris
levels(iris.fake$Species)[3] <- "sibirica"   # tweak levels

vet(tpl.iris, iris[1:10, ])
vet(tpl.iris, iris.fake[1:10, ])

## ---------------------------------------------------------
stopifnot(
  is.list(iris.fake), inherits(iris.fake, "data.frame"),
  length(iris.fake) == 5, is.integer(attr(iris.fake, 'row.names')),
  identical(
    names(iris.fake),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  ),
  all(vapply(iris.fake[1:4], is.numeric, logical(1L))),
  typeof(iris.fake$Species) == "integer", is.factor(iris.fake$Species),
  identical(levels(iris.fake$Species), c("setosa", "versicolor", "virginica"))
)

## ---------------------------------------------------------
vet(tpl.iris, iris.fake[1:10, ])

## ---------------------------------------------------------
vet(numeric(1L) || NULL, NULL)
vet(numeric(1L) || NULL, 42)
vet(numeric(1L) || NULL, "foo")

## ---------------------------------------------------------
vet(numeric(1L) && . > 0, -42)  # strictly positive scalar numeric
vet(numeric(1L) && . > 0, 42)

## ---------------------------------------------------------
scalar.num.pos <- quote(numeric(1L) && . > 0)
foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
vet.exp <- quote(scalar.num.pos || foo.or.bar)

vet(vet.exp, 42)
vet(vet.exp, "foo")
vet(vet.exp, "baz")

## ---- eval=FALSE------------------------------------------
#  logical(1) || (numeric(1) && . %in% 0:1)

## ---------------------------------------------------------
vet(logical(1) || (numeric(1) && . %in% 0:1), TRUE)
vet(logical(1) || (numeric(1) && . %in% 0:1), 0)
vet(logical(1) || (numeric(1) && . %in% 0:1), "1")

## ---- eval=FALSE------------------------------------------
#  I(length(a) == length(b) && . %in% 0:1)

## ---- eval=FALSE------------------------------------------
#  I(logical(1L) && my_special_fun(.))

## ---------------------------------------------------------
TF <- quote(logical(1) && !anyNA(.))  # note `quote`

vet(TF, TRUE)
vet(TF, NA)
vet(TF, 1)

## ---------------------------------------------------------
ZERO_OR_ONE <- quote(numeric(1) && !is.na(.) && . %in% 0:1)
TF_ish <- quote(TF || ZERO_OR_ONE)

vet(TF_ish, 1)
vet(TF_ish, "0")

## ---------------------------------------------------------
NONA <- vet_token(!is.na(.), "%sshould not contain NAs")
TF <- quote(logical(1L) && NONA)
vet(TF, NA)

## ---------------------------------------------------------
vet(quote(x && y), quote(a || b))
vet(quote(x && y), quote(a && b))

## ---------------------------------------------------------
LANG.AND <- quote(quote(x && y))
vet(LANG.AND, quote(a || b))
vet(LANG.AND, quote(a && b))

## ---------------------------------------------------------
vet(NUM.1.POS, 5)
vet(NUM.1.POS, -3)
vet(NUM.1.POS, runif(5))
vet(CHR, letters)
vet(CHR, factor(letters))

## ---------------------------------------------------------
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

## ---------------------------------------------------------
fun <- function(x, y, z) {
  vetr(z=character(1L) && . %in% c("foo", "bar"))
  TRUE  # do work...
}
fun(matrix(1:12, 3), TRUE, "baz")
fun(matrix(1:12, 4), TRUE, "bar")

## ---------------------------------------------------------
library(microbenchmark)
microbenchmark(
  vet(laps.template, laps2),
  vet_stopifnot(laps2)
)

## ---------------------------------------------------------
microbenchmark(data.frame(a=numeric()))

## ---------------------------------------------------------
library(valaddin)

secant <- function(f, x, dx) (f(x + dx) - f(x)) / dx

secant_valaddin <- valaddin::firmly(secant, list(~x, ~dx) ~ is.numeric)
secant_stopifnot <- function(f, x, dx) {
  stopifnot(is.numeric(x), is.numeric(dx))
  secant(f, x, dx)
}
secant_vetr <- function(f, x, dx) {
  vetr(x=numeric(), dx=numeric())
  secant(f, x, dx)
}

microbenchmark(
  secant_valaddin(log, 1, .1),
  secant_stopifnot(log, 1, .1),
  secant_vetr(log, 1, .1)
)

