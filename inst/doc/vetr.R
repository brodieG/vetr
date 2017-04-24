## ----global_options, echo=FALSE---------------------------
knitr::opts_chunk$set(error=TRUE)
options(width=60)

## ---------------------------------------------------------
getOption("width")
cat(rep('x', 60), "\n", sep="")
x <- 1:3
stopifnot(is.numeric(x), length(x) == 1L)

## ---------------------------------------------------------
my_fun <- function() stop("Testing what happens with\nnewlines in knitr")
my_fun()
library(vetr)
vet(numeric(1L), x)

## ---------------------------------------------------------
laps.template <- structure(
  class="laps",
  list(
    car=character(1),
    data=data.frame(lap=numeric(), time=Sys.time()[0])
) )

## ---------------------------------------------------------
lap.times <- data.frame(lap=1:10, time=cumsum(rnorm(10, 120, 3)))
laps.1 <- structure(lap.times, class="laps")
laps.2 <- structure(list("corvette z06", lap.times), class="laps")
laps.3 <- laps.4 <- setNames(laps.2, c("car", "data"))
laps.4$data <- transform(laps.4$data, time=Sys.time() + time)

## ---------------------------------------------------------
vet(laps.template, laps.1)   # Forgot to include car
vet(laps.template, laps.2)   # Missing names
vet(laps.template, laps.3)   # Lap times should be in POSIXct
vet(laps.template, laps.4)   # Correct!

## ---------------------------------------------------------
stopifnot(
  is.list(laps.1),
  inherits(laps.1, "laps"),
  length(laps.1) == 2L,
  identical(names(laps.1), c("car", "data")),
  is.character(laps.1$car),
  length(laps.1$car) == 1L,
  is.data.frame(laps.1$data),
  identical(names(laps.1$data), c("lap", "time")),
  is.numeric(laps.1$lap),
  is.POSIXct(laps.1$time)
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
NONA <- mk_val_token(!is.na(.), "not contain NAs")
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
  analyze2(laps.4), # stopifnot version
  analyze(laps.4)   # validate version
)

## ---------------------------------------------------------
microbenchmark(data.frame(a=numeric()))

