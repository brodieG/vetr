# Copyright (C) 2022 Brodie Gaslam
#
# This file is part of "vetr - Trust, but Verify"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Lightweight Benchmarking Function
#'
#' Evaluates provided expression in a loop and reports mean evaluation time.
#' This is inferior to `microbenchmark` and other benchmarking tools in many
#' ways except that it has zero dependencies or suggests which helps with
#' package build and test times.  Used in vignettes.
#'
#' Runs [gc()] before each expression is evaluated.  Expressions are evaluated
#' in the order provided.  Attempts to estimate the overhead of the loop by
#' running a loop that evaluates `NULL` the `times` times.
#'
#' Unfortunately because this computes the average of all iterations it is very
#' susceptible to outliers in small sample runs, particularly with fast running
#' code.  For that reason the default number of iterations is one thousand.
#'
#' @importFrom stats median
#' @export
#' @param ... expressions to benchmark, are captured unevaluated
#' @param times how many times to loop, defaults to 1000
#' @param deparse.width how many characters to deparse for labels
#' @return NULL, invisibly, reports timings as a side effect as screen output
#' @examples
#' bench_mark(runif(1000), Sys.sleep(0.001), times=10)

bench_mark <- function(..., times=1000L, deparse.width=40) {
  stopifnot(
    is.integer(times) || is.numeric(times), length(times) == 1, times > 0
  )
  times <- as.integer(times)
  dots <- as.list(match.call(expand.dots=FALSE)[["..."]])
  p.f <- parent.frame()

  timings <- vapply(
    dots, function(x) {
      call.q <- bquote({
        gc()
        start <- proc.time()
        for(i in 1:.(times)) .(x)
        stop <- proc.time()
        stop[['elapsed']] - start[['elapsed']]
      })
      eval(call.q, p.f)
    },
    numeric(1L)
  )
  # try to compute overhead

  o.h.times <- 10
  gc()
  overhead <- vapply(
    seq.int(o.h.times), function(x) {
      call.q.baseline <- bquote({
        start <- proc.time()
        for(j in 1:.(times)) NULL
        stop <- proc.time()
        stop[['elapsed']] - start[['elapsed']]
      })
      eval(call.q.baseline, p.f)
    },
    numeric(1L)
  )
  overhead.act <- median(overhead)
  timings.fin <- (timings - overhead.act) / times
  exps <- vapply(
    dots,
    function(x) dep_oneline(x, max.chars=deparse.width),
    character(1L)
  )
  timings.clean <- timings.fin[timings.fin >= 0]

  unit <- "seconds"
  mult <- 0

  if(length(timings.clean)) {
    min.time <- min(log(timings.clean, base=10))
    if(min.time <= -3) {
      unit <- "microseconds"
      mult <- 6
    } else if(min.time <= 0) {
      unit <- "milliseconds"
      mult <- 3
    }
  }
  timings.disp <- timings.fin * 10 ^ mult

  cat(
    sprintf(
      "Mean eval time from %d iteration%s, in %s:\n", times,
      if(times > 1) "s" else "", unit
  ) )
  cat(
    paste0(
      "  ",
      format(exps), "  ~  ",
      format(signif(timings.disp, 4), justify='right'), "\n"
    ),
    sep=""
  )
  invisible(data.frame(call=exps, mean.time=timings.fin))
}
