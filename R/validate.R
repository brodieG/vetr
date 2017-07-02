# Copyright (C) 2017  Brodie Gaslam
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

#' Vet Objects and Function Arguments
#'
#' Use templates and expressions to vet objects or function arguments.
#' \code{vet} vets objects, and \code{vetr} vets the formals of the
#' function that encloses it.
#'
#' The \code{target} argument for \code{vet} and the \code{...} arguments are
#' recursively substituted.  If you wish to programmatically specify a vetting
#' expression you can provide it as quoted language.
#'
#' See \code{vignette('vetr', package='vetr')} and examples for details on how
#' to use these functions.
#'
#' @useDynLib vetr, .registration=TRUE, .fixes="VALC_"
#' @note \code{vetr} will force evaluation of any arguments that are being
#'   checked (you may omit arguments that should not be evaluate from
#'   \code{vetr})
#' @name vet
#' @rdname vet
#' @aliases vetr tev
#' @export
#' @seealso \code{\link{alike}} for how templates are used,
#'   \code{\link{vet_token}} for how to specify custom error messages and also
#'   for predefined validation tokens for common use cases.
#' @param ... arguments to vetr; they will be matched to the enclosing
#'   function formals as with \code{\link{match.call}}
#' @param target a template, a vetting expression, or a compound expression
#' @param current an object to vet
#' @param env the environment to match calls and evaluate vetting expressions
#'   in; will be ignored if an environment is also specified via
#'   [vetr_settings()].  Defaults to calling frame.
#' @param format character(1L), controls the format of the return value for
#'   \code{vet}, in case of failure.  One of:\itemize{
#'     \item "text": (default) character(1L) message for use elsewhere in code
#'     \item "full": character(1L) the full error message used in "stop" mode,
#'       but actually returned instead of thrown as an error
#'     \item "raw": character(N) least processed version of the error message
#'       with none of the formatting or surrounding verbiage
#' }
#' @param stop TRUE or FALSE whether to call \code{\link{stop}} on failure
#'   (default) or not
#' @param settings a settings list as produced by [vetr_settings()], or NULL to
#'   use the default settings
#' @param .VETR_SETTINGS same as `settings`, but for `vetr`.  Note that this
#'   means you cannot use `vetr` with a function that takes a `.VETR_SETTINGS`
#'   argument
#' @return TRUE if validation succeeds, otherwise \code{stop} for
#'   \code{vetr} and varies for \code{vet} according to value chosen with
#'   parameter \code{stop}
#' @examples
#' ## template vetting
#' vet(numeric(2L), runif(2))
#' vet(numeric(2L), runif(3))
#' vet(numeric(2L), letters)
#' try(vet(numeric(2L), letters, stop=TRUE))
#'
#' ## `tev` just reverses target and current for use with maggrittr
#'
#' if(require(magrittr)) {
#'   runif(2) %>% tev(numeric(2L))
#'   runif(3) %>% tev(numeric(2L))
#' }
#' ## Zero length templates are wild cards
#' vet(numeric(), runif(2))
#' vet(numeric(), runif(100))
#' vet(numeric(), letters)
#'
#' ## This extends to data.frames
#' iris.tpl <- iris[0,]   # zero row
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
#' ## Vetting expression
#' vet(. > 0, runif(10))
#' vet(. > 0, -runif(10))
#'
#' ## Compound expressions (template(s) + expression(s)):
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
#' ## Function parameter vetting
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
#' ## more complex vetting (`tpl`, `val.1`, and `val.2` defined in
#' ## earlier examples)
#' fun3 <- function(x, y) {
#'   vetr(x=tpl, y=tpl && ncol(.[[2]]) == ncol(x[[2]]))
#'   TRUE   # do some work
#' }
#' fun3(val.1, val.1)
#' try(fun3(val.1, val.2))
#' val.1.a <- val.1
#' val.1.a[[2]] <- val.1.a[[2]][, 1:8]
#' try(fun3(val.1, val.1.a))

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

#' @rdname vet
#' @export

vetr <- function(..., .VETR_SETTINGS=NULL)
  .Call(
    VALC_validate_args,
    fun.match <- sys.function(sys.parent(1)),
    match.call(
      definition=fun.match,
      call=sys.call(sys.parent(1)),
      envir=parent.frame(2L)
    ),
    match.call(definition=fun.match, envir=(par.frame <- parent.frame())),
    par.frame,
    .VETR_SETTINGS
  )
