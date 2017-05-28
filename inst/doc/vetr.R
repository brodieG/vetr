## ----global_options, echo=FALSE---------------------------
knitr::opts_chunk$set(error=TRUE, hilang='r')
options(width=60)
library(vetr)

## ---------------------------------------------------------
x <- 1:3
stopifnot(is.numeric(x), length(x) == 1L)

## ---------------------------------------------------------
vet(numeric(1L), x)

## ---------------------------------------------------------
fun <- function(x, y) {
  vetr(numeric(1L), logical(1L))
  # ... function code goes here
}
fun(1:2, "hello")
fun(1, "hello")

## ---------------------------------------------------------
laps.template <- structure(class="laps",
  list(car=character(1), data=data.frame(lap=numeric(), time=Sys.time()[0])
) )

## ---------------------------------------------------------
lap.times <- data.frame(lap=1:10, time=cumsum(rnorm(10, 120, 3)))
laps1 <- structure(list(lap.times), class="laps")
laps2 <- laps3 <-
  structure(list(car="corvette z06", data=lap.times), class="laps")
laps3$data <- transform(laps3$data, time=Sys.time() + time)

## ---------------------------------------------------------
vet(laps.template, laps1)   # Forgot to include car
vet(laps.template, laps2)   # Lap times should be in POSIXct
vet(laps.template, laps3)   # works

## ---------------------------------------------------------
vet_stopifnot <- function(x)
  stopifnot(
    is.list(x),
    inherits(x, "laps"),
    length(x) == 2L,
    identical(names(x), c("car", "data")),
    is.character(x$car),
    length(x$car) == 1L,
    is.data.frame(x$data),
    identical(names(x$data), c("lap", "time")),
    is.numeric(x$data$lap),
    identical(mode(x$data$time), "numeric"),
    inherits(x$data$time, c("POSIXct", "POSIXt"))
  )

## ---------------------------------------------------------
x <- 1:2
y <- c(1, NA)
vet(!anyNA(.), x)
vet(!anyNA(.), y)

## ---------------------------------------------------------
vet(numeric(2L) && !anyNA(.), x)
vet(numeric(2L) && !anyNA(.), y)
vet(numeric(2L) && !anyNA(.), 1:10)
vet(numeric(2L) && !anyNA(.) && . > 0L, -(1:2))

## ---------------------------------------------------------
vet((numeric(2L) && !anyNA(.)) || NULL, 1:2)
vet((numeric(2L) && !anyNA(.)) || NULL, NULL)
vet((numeric(2L) && !anyNA(.)) || NULL, letters)

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
NONA <- vet_token(!is.na(.), "%s should not contain NAs")
TF <- quote(logical(1L) && NONA)
vet(TF, NA)

## ---------------------------------------------------------
vet(NUM.1.POS, 5)
vet(NUM.1.POS, -3)
vet(NUM.1.POS, runif(5))
vet(CHR, letters)
vet(CHR, factor(letters))

## ---------------------------------------------------------
library(microbenchmark)
microbenchmark(
  vet(laps.template, laps.4), # vet version
  vet_stopifnot(laps.4)       # validate version
)

## ---------------------------------------------------------
microbenchmark(data.frame(a=numeric()))

## ---------------------------------------------------------
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

## ---------------------------------------------------------
f.tpl <- `attributes<-`(function(x, y) NULL, NULL)
secant_vetr2 <- function(f, x, dx) {
  vetr(f.tpl, numeric(), numeric())
  secant(f, x, dx)
}
secant_vetr2(log, 1, .1)
secant_vetr2(sin, 1, .1)

