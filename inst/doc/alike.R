## ------------------------------------------------------------------------
library(vetr)
alike(integer(5), 1:5)      # different values, but same structure
alike(integer(5), 1:4)      # wrong size
alike(integer(26), letters) # same size, but different types

## ------------------------------------------------------------------------
alike(integer(), 1:5)
alike(integer(), 1:4)
alike(integer(), letters)  # type is still defined and must match

## ------------------------------------------------------------------------
alike(list(), data.frame())  # a data frame is a list with a attributes
alike(data.frame(), list())  # but a list does not have the data.frame attributes

## ------------------------------------------------------------------------
mx.tpl <- matrix(integer(), ncol=3)          # partially specified matrix
alike(mx.tpl, matrix(sample(1:12), nrow=4))  # any number of rows match
alike(mx.tpl, matrix(sample(1:12), nrow=3))  # but column count must match

## ------------------------------------------------------------------------
iris.tpl <- iris[0, ]                        # no rows, but structure is defined
alike(iris.tpl, iris[1:10, ])                # any number of rows match
alike(iris.tpl, CO2)                         # but column structure must match

## ------------------------------------------------------------------------
alike(1L, 1)     # `1` is not technically integer, but we treat it as such
alike(1L, 1.1)   # 1.1 is not integer-like
alike(1.1, 1L)   # integers can match numerics

## ---- eval=FALSE---------------------------------------------------------
#  stopifnot(length(x) == 1L && (is.integer(x) || is.numeric(x) && floor(x) == x))
#  stopifnot(alike(integer(1L), x))

## ------------------------------------------------------------------------
alike(list(NULL, NULL), list(1:10, letters))       # two NULLs match two length list
alike(list(NULL, NULL), list(1:10, letters, iris)) # but not three length list

## ------------------------------------------------------------------------
alike(NULL, 1:10)                   # NULL only matches NULL

## ------------------------------------------------------------------------
alike(quote(sum(a, b)), quote(sum(x, y)))   # calls are consistent
alike(quote(sum(a, b)), quote(sum(x, x)))   # calls are inconsistent
alike(quote(mean(a, b)), quote(sum(x, y)))  # functions are different

## ------------------------------------------------------------------------
fun <- function(a, b, c) NULL
alike(quote(fun(p, q, p)), quote(fun(y, x, x)))
alike(quote(fun(p, q, p)), quote(fun(b=y, x, x)))  # `match.call` re-orders arguments

## ------------------------------------------------------------------------
str(one.arg.tpl <- as.call(list(NULL, NULL)))
alike(one.arg.tpl, quote(log(10)))
alike(one.arg.tpl, quote(sd(runif(20))))
alike(one.arg.tpl, quote(log(10, 10)))

## ------------------------------------------------------------------------
alike(y ~ x ^ 2, a ~ b ^ 2)
alike(y ~ x ^ 2, a ~ b ^ 3)

## ------------------------------------------------------------------------
alike(print, print.default)   # print can be the generic for print.default
alike(print.default, print)   # but not vice versa

## ------------------------------------------------------------------------
alike(structure(logical(1L), a=integer(3L)), structure(TRUE, a=1:3, b=letters))
alike(structure(TRUE, a=1:3, b=letters), structure(logical(1L), a=integer(3L)))

## ------------------------------------------------------------------------
alike(setNames(integer(), character()), 1:3)
alike(setNames(integer(), character()), c(a=1, b=2, c=3))
alike(setNames(integer(3), c("", "", "Z")), c(a=1, b=2, c=3))
alike(setNames(integer(3), c("", "", "Z")), c(a=1, b=2, Z=3))

## ------------------------------------------------------------------------
mx.tpl <- matrix(integer(), ncol=3)                # partially specified matrix
alike(mx.tpl, matrix(sample(1:12), nrow=4))
alike(mx.tpl, matrix(sample(1:12), nrow=3))        # wrong number of columns
str(mx.tpl)    # notice 0 for 1st dimension

## ------------------------------------------------------------------------
mx.tpl <- matrix(integer(), ncol=3, dimnames=list(row.id=NULL, c("R", "G", "")))
mx.cur <- matrix(sample(0:255, 12), ncol=3, dimnames=list(row.id=1:4, rgb=c("R", "G", "Blue")))
mx.cur2 <- matrix(sample(0:255, 12), ncol=3, dimnames=list(1:4, c("R", "G", "b")))

alike(mx.tpl, mx.cur)
alike(mx.tpl, mx.cur2)

## ------------------------------------------------------------------------
names(dimnames(mx.tpl))

## ------------------------------------------------------------------------
tpl <- structure(NULL, class=c("a", "b", "c"))
cur <- structure(NULL, class=c("x", "a", "b", "c"))
cur2 <- structure(NULL, class=c("a", "b", "c", "x"))

alike(tpl, cur)
alike(tpl, cur2)

## ------------------------------------------------------------------------
int.scalar <- integer(1L)
int.mat.2.by.4 <- matrix(integer(), 2, 4)
df.chr.num.num <- structure(list(character(), numeric(), numeric()), class="data.frame")  # avoid having to specify column names

## ---- eval=FALSE---------------------------------------------------------
#  iris.tpl <- iris[0, ]
#  alike(iris.tpl, iris.sample.1)  # make sure they submit data correctly

## ---- eval=FALSE---------------------------------------------------------
#  iris.tpl <- abstract(iris)

## ------------------------------------------------------------------------
abstract(list(c(a=1, b=2, c=3), letters))

## ------------------------------------------------------------------------
df.dummy <- data.frame(x=runif(3), y=runif(3), z=runif(3))
mdl.tpl <- abstract(lm(y ~ x + z, df.dummy))
alike(mdl.tpl, lm(Sepal.Length ~ Sepal.Width + Petal.Width, iris))  # TRUE, expecting bi-variate model
cat(alike(mdl.tpl, lm(Sepal.Length ~ Sepal.Width, iris)))           # `cat` here to make error message legible

## ------------------------------------------------------------------------
library(microbenchmark)
type_and_len <- function(a, b) typeof(a) == typeof(b) && length(a) == length(b)  # for reference
microbenchmark(
  identical(rivers, rivers),
  alike(rivers, rivers),
  type_and_len(rivers, rivers)
)

## ------------------------------------------------------------------------
microbenchmark(
  identical(mtcars, mtcars),
  alike(mtcars, mtcars)
)

## ------------------------------------------------------------------------
mdl.tpl <- abstract(lm(y ~ x + z, data.frame(x=runif(3), y=runif(3), z=runif(3))))
mb <- microbenchmark(unit="us",
  alike(mdl.tpl, mdl.tpl),       # compare mdl.tpl to itself to ensure success in all three scenarios
  all.equal(mdl.tpl, mdl.tpl),   # for reference
  identical(mdl.tpl, mdl.tpl)
)
summary(mb)[1:4]  # note: in microseconds

## ------------------------------------------------------------------------
df.tpl <- data.frame(a=integer(), b=numeric())
df.cur <- data.frame(a=1:10, b=1:10 + .1)

mb <- microbenchmark(unit="us",
  alike(df.tpl, df.cur),
  alike(data.frame(integer(), numeric()), df.cur)
)
summary(mb)[1:4]  # note: in microseconds

