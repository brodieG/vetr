# Copyright (C) 2023 Brodie Gaslam
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

#' Verify Objects Meet Structural Requirements
#'
#' Use vetting expressions to enforce structural requirements for objects.
#' `tev` is a version of `vet` compatible with `magrittr` pipes.
#'
#' `tev` just reverses the `target` and `current` arguments for better
#' integration with `magrittr`.  There are two major caveats:
#'
#' * error messages will be less useful since you will get `.` instead
#'   of the deparsed call
#' * `x \\%>\\% tev(y)` is much slower than `vet(y, x)` (or even `tev(x, y)`)
#'
#' @section Vetting Expressions:
#'
#' Vetting expressions can be template tokens, standard tokens, or any
#' combination of template and standard tokens combined with \code{&&} and/or
#' \code{||}.  Template tokens are R objects that define the required structure,
#' much like the `FUN.VALUE` argument to [vapply()].  Standard tokens are tokens
#' that contain the `.` symbol and are used to vet values.
#'
#' If you do use the `.` symbol in your vetting expressions in your
#' packages, you will need to include `utils::globalVariables(".")` as a
#' top-level call to avoid the "no visible binding for global variable '.'"'
#' R CMD check NOTE.
#'
#' See `vignette('vetr', package='vetr')` and examples for details on how
#' to craft vetting expressions.
#'
#' @useDynLib vetr, .registration=TRUE, .fixes="VALC_"
#' @aliases tev
#' @export
#' @seealso [vetr()] for a version optimized to vet function arguments,
#'   [alike()] for how templates are used, [vet_token()] for how to specify
#'   custom error messages and also for predefined validation tokens for common
#'   use cases, [all_bw()] for fast bounds checks.
#' @param target a template, a vetting expression, or a compound expression
#' @param current an object to vet
#' @param env the environment to match calls and evaluate vetting expressions
#'   in; will be ignored if an environment is also specified via
#'   [vetr_settings()].  Defaults to calling frame.
#' @param format character(1L), controls the format of the return value for
#'   `vet`, in case of failure.  One of:\itemize{
#'     \item "text": (default) character(1L) message for use elsewhere in code
#'     \item "full": character(1L) the full error message used in "stop" mode,
#'       but actually returned instead of thrown as an error
#'     \item "raw": character(N) least processed version of the error message
#'       with none of the formatting or surrounding verbiage
#' }
#' @param stop TRUE or FALSE whether to call [stop()] on failure
#'   or not (default)
#' @param settings a settings list as produced by [vetr_settings()], or NULL to
#'   use the default settings
#' @return TRUE if validation succeeds, otherwise varies according to value
#'   chosen with parameter `stop`
#' @examples
#' ## template vetting
#' vet(numeric(2L), runif(2))
#' vet(numeric(2L), runif(3))
#' vet(numeric(2L), letters)
#' try(vet(numeric(2L), letters, stop=TRUE))
#'
#' ## `tev` just reverses target and current for use with maggrittr
#' \dontrun{
#' if(require(magrittr)) {
#'   runif(2) %>% tev(numeric(2L))
#'   runif(3) %>% tev(numeric(2L))
#' }
#' }
#' ## Zero length templates are wild cards
#' vet(numeric(), runif(2))
#' vet(numeric(), runif(100))
#' vet(numeric(), letters)
#'
#' ## This extends to data.frames
#' iris.tpl <- iris[0,]   # zero row matches any # of rows
#' iris.1 <- iris[1:10,]
#' iris.2 <- iris[1:10, c(1,2,3,5,4)]  # change col order
#' vet(iris.tpl, iris.1)
#' vet(iris.tpl, iris.2)
#'
#' ## Short (<100 length) integer-like numerics will
#' ## pass for integer
#' vet(integer(), c(1, 2, 3))
#' vet(integer(), c(1, 2, 3) + 0.1)
#'
#' ## Nested templates; note, in packages you should consider
#' ## defining templates outside of `vet` or `vetr` so that
#' ## they are computed on load rather that at runtime
#' tpl <- list(numeric(1L), matrix(integer(), 3))
#' val.1 <- list(runif(1), rbind(1:10, 1:10, 1:10))
#' val.2 <- list(runif(1), cbind(1:10, 1:10, 1:10))
#' vet(tpl, val.1)
#' vet(tpl, val.2)
#'
#' ## See `example(alike)` for more template examples
#'
#' ## Standard tokens allow you to check values
#' vet(. > 0, runif(10))
#' vet(. > 0, -runif(10))
#'
#' ## Zero length token results are considered TRUE,
#' ## as is the case with `all(logical(0))`
#' vet(. > 0, numeric())
#'
#' ## `all_bw` is like `isTRUE(all(. >= x & . <= y))`, but
#' ## ~10x faster for long vectors:
#' vet(all_bw(., 0, 1), runif(1e6) + .1)
#'
#' ## You can combine templates and standard tokens with
#' ## `&&` and/or `||`
#' vet(numeric(2L) && . > 0, runif(2))
#' vet(numeric(2L) && . > 0, runif(10))
#' vet(numeric(2L) && . > 0, -runif(2))
#'
#' ## Using pre-defined tokens (see `?vet_token`)
#' vet(INT.1, 1)
#' vet(INT.1, 1:2)
#' vet(INT.1 && . %in% 0:1 || LGL.1, TRUE)
#' vet(INT.1 && . %in% 0:1 || LGL.1, 1)
#' vet(INT.1 && . %in% 0:1 || LGL.1, NA)
#'
#' ## Vetting expressions can be assembled from previously
#' ## defined tokens
#' scalar.num.pos <- quote(numeric(1L) && . > 0)
#' foo.or.bar <- quote(character(1L) && . %in% c('foo', 'bar'))
#' vet.exp <- quote(scalar.num.pos || foo.or.bar)
#'
#' vet(vet.exp, 42)
#' vet(scalar.num.pos || foo.or.bar, 42)  # equivalently
#' vet(vet.exp, "foo")
#' vet(vet.exp, "baz")

vet <- function(
  target, current, env=parent.frame(), format="text", stop=FALSE, settings=NULL
)
  # note sys.call not matched, which is why we need substitute(current)
  .Call(
    VALC_validate, substitute(target), current, substitute(current),
    sys.call(), env, format, stop, settings
  )
#' @rdname vet
#' @export

tev <- function(
  current, target, env=parent.frame(), format="text", stop=FALSE, settings=NULL
)
  # note sys.call not matched, which is why we need substitute(current)
  .Call(
    VALC_validate, substitute(target), current, substitute(current),
    sys.call(), env, format, stop, settings
  )

#' Verify Function Arguments Meet Structural Requirements
#'
#' Use vetting expressions to enforce structural requirements for function
#' arguments.  Works just like [vet()], except that the formals of the
#' enclosing function automatically matched to the vetting expressions provided
#' in `...`.
#'
#' Only named arguments may be vetted; in other words it is not possible to vet
#' arguments passed via `...`.
#'
#' @inheritSection vet Vetting Expressions
#'
#' @note `vetr` will force evaluation of any arguments that are being
#'   checked (you may omit arguments that should not be evaluate from
#'   `vetr`)
#' @seealso [vet()], in particular `example(vet)`.
#' @param ... vetting expressions, each will be matched to the enclosing
#'   function formals as with [match.call()] and will be used to validate the
#'   value of the matching formal.
#' @param .VETR_SETTINGS a settings list as produced by [vetr_settings()], or
#'   NULL to use the default settings.  Note that this means you cannot use
#'   `vetr` with a function that takes a `.VETR_SETTINGS` argument
#' @return TRUE if validation succeeds, otherwise `stop` with error message
#'   detailing nature of failure.
#' @export
#' @examples
#' fun1 <- function(x, y) {
#'   vetr(integer(), LGL.1)
#'   TRUE   # do some work
#' }
#' fun1(1:10, TRUE)
#' try(fun1(1:10, 1:10))
#'
#' ## only vet the second argument
#' fun2 <- function(x, y) {
#'   vetr(y=LGL.1)
#'   TRUE   # do some work
#' }
#' try(fun2(letters, 1:10))
#'
#' ## Nested templates; note, in packages you should consider
#' ## defining templates outside of `vet` or `vetr` so that
#' ## they are computed on load rather that at runtime
#' tpl <- list(numeric(1L), matrix(integer(), 3))
#' val.1 <- list(runif(1), rbind(1:10, 1:10, 1:10))
#' val.2 <- list(runif(1), cbind(1:10, 1:10, 1:10))
#' fun3 <- function(x, y) {
#'   vetr(x=tpl, y=tpl && ncol(.[[2]]) == ncol(x[[2]]))
#'   TRUE   # do some work
#' }
#' fun3(val.1, val.1)
#' try(fun3(val.1, val.2))
#' val.1.a <- val.1
#' val.1.a[[2]] <- val.1.a[[2]][, 1:8]
#' try(fun3(val.1, val.1.a))

vetr <- function(..., .VETR_SETTINGS=NULL)
  .Call(
    VALC_validate_args,
    fun.match <- sys.function(sys.parent(1)),
    match.call(
      definition=fun.match,
      call=sys.call(sys.parent(1)),
      envir=parent.frame(2L),
      expand.dots=FALSE
    ),
    match.call(
      definition=fun.match,
      envir=(par.frame <- parent.frame()),
      expand.dots=FALSE
    ),
    par.frame,
    .VETR_SETTINGS
  )
