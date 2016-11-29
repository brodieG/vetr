## ----global_options, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(error=TRUE)
options(width=80)

## -----------------------------------------------------------------------------
fun <- function(a, b) {
  validate_args(matrix(numeric(), ncol=3), logical(1))
  TRUE
}

## -----------------------------------------------------------------------------
fun_traditional <- function(a, b) {
  stopifnot(is.numeric(a), is.matrix(a), ncol(a) == 3, is.logical(b), length(b) == 1)
  TRUE
}

## -----------------------------------------------------------------------------
fun(matrix(1:6, ncol=2), TRUE)                # Fail
fun_traditional(matrix(1:6, ncol=2), TRUE)    # Fail

## -----------------------------------------------------------------------------
fun(matrix(1:6, ncol=3), "hello")              # Fail
fun(matrix(1:6, ncol=3), TRUE)                 # Success

## -----------------------------------------------------------------------------
my.obj <- 1.5
validate(matrix(numeric(), ncol=3), my.obj)

## -----------------------------------------------------------------------------
validate(matrix(numeric(), ncol=3), my.obj, return.mode="text")

## -----------------------------------------------------------------------------
validate(numeric(), runif(10))
validate(numeric(), matrix(runif(9), 3))
validate(numeric(), letters)

## -----------------------------------------------------------------------------
validate(numeric(3), runif(10))
validate(numeric(3), runif(3))

## -----------------------------------------------------------------------------
laps.template <- structure(
  list(
    car=character(1),
    data=data.frame(lap=numeric(), time=Sys.time()[0])
  ),
  class="laps"
)

## -----------------------------------------------------------------------------
analyze <- function(x) {
  validate_args(laps.template)
  TRUE # stand-in for really meaningful code
}

## -----------------------------------------------------------------------------
lap.times <- data.frame(lap=1:10, time=cumsum(rnorm(10, 120, 3)))
laps.1 <- structure(lap.times, class="laps")
laps.2 <- structure(list("corvette z06", lap.times), class="laps")
laps.3 <- laps.4 <- setNames(laps.2, c("car", "data"))
laps.4$data <- transform(laps.4$data, time=Sys.time() + time)

## -----------------------------------------------------------------------------
analyze(laps.1)   # Forgot to include car
analyze(laps.2)   # Missing names
analyze(laps.3)   # Lap times should be in POSIXct
analyze(laps.4)   # Valid object

## -----------------------------------------------------------------------------
validate(integer(1) || character(1), 2)
validate(integer(1) || character(1), "hello")
validate(integer(1) || character(1), TRUE)

## -----------------------------------------------------------------------------
validate(numeric(3) && all(. > 0), c(1, 2, 3))
validate(numeric(3) && all(. > 0), c(1, -2, 3))

## ---- eval=FALSE--------------------------------------------------------------
#  logical(1) || (numeric(1) && . %in% 0:1)

## -----------------------------------------------------------------------------
validate(logical(1) || (numeric(1) && . %in% 0:1), TRUE)
validate(logical(1) || (numeric(1) && . %in% 0:1), 0)
validate(logical(1) || (numeric(1) && . %in% 0:1), "1")

## ---- eval=FALSE--------------------------------------------------------------
#  identity(length(a) == length(b) && . %in% 0:1)

## ---- eval=FALSE--------------------------------------------------------------
#  identity(logical(1L) && my_special_fun(.))

## -----------------------------------------------------------------------------
TF <- quote(logical(1) && !is.na(.))  # note `quote`
validate(TF, TRUE)
validate(TF, NA)
validate(TF, 1)

## -----------------------------------------------------------------------------
ZERO_OR_ONE <- quote(numeric(1) && !is.na(.) && . %in% 0:1)
TF_ish <- quote(TF || ZERO_OR_ONE)
validate(TF_ish, 1)
validate(TF_ish, "0")

## -----------------------------------------------------------------------------
NONA <- mk_val_token(!is.na(.), "not contain NAs")
TF <- quote(logical(1L) && NOT.NA)
validate(TF, NA)

## -----------------------------------------------------------------------------
validate(NUM.1.POS, 5)
validate(NUM.1.POS, -3)
validate(NUM.1.POS, runif(5))
validate(CHR, letters)
validate(CHR, factor(letters))

## -----------------------------------------------------------------------------
analyze2 <- function(x, ...) {
  stopifnot(
    is.list(x), inherits(x, "laps"), identical(names(x), names(laps.template)),
    is.character(x$car), length(x$car) == 1, is.data.frame(x$data),
    identical(names(x$data), names(laps.template$data)),
    identical(attributes(x$data$lap), attributes(laps.template$data$lap)),
    identical(attributes(x$data$time), attributes(laps.template$data$time))
  )
  TRUE
}
analyze2(laps.1)   # Forgot to include car

## -----------------------------------------------------------------------------
analyze(laps.1)    # Forgot to include car

## -----------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  analyze2(laps.4), # stopifnot version
  analyze(laps.4)   # validate version
)

## -----------------------------------------------------------------------------
microbenchmark(data.frame(a=numeric()))

