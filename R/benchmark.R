# Lightweight benchmarking function

bench <- function(..., times=1000L) {
  stopifnot(is.integer(times), length(times) == 1, times > 0)
  dots <- as.list(match.call(expand.dots=FALSE)[["..."]])
  p.f <- parent.frame()

  gc()
  timings <- vapply(
    dots, function(x) {
      call.q <- bquote(for(i in 1:.(times)) .(x))
      start <- Sys.time()
      eval(call.q, p.f)
      stop <- Sys.time()
      stop - start
    },
    Sys.time() - Sys.time()
  ) / times
  exps <- vapply(dots, function(x) deparse(x)[[1]], character(1L))
  timings.clean <- timings[timings != 0]

  unit <- "seconds"
  mult <- 1

  if(length(timings.clean)) {
    min.time <- min(log(timings.clean, base=10))
    if(min.time <= -6) {
      unit <- "nanoseconds"
      mult <- 9
    } else if(min.time <= -3) {
      unit <- "microseconds"
      mult <- 6
    } else if(min.time <= 0) {
      unit <- "milliseconds"
      mult <- 3
    }
  }
  timings <- timings * 10 ^ mult

  cat(sprintf("Mean eval time from %d evals, in %s\n", times, unit))
  cat(
    paste0(
      "  ",
      format(exps), "  ",  format(timings, justify='right'), "\n"
    ),
    sep=""
  )
  invisible(NULL)
}
