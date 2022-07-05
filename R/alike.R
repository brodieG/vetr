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

#' Compare Object Structure
#'
#' Similar to \code{\link{all.equal}}, but compares object structure rather than
#' value.  The \code{target} argument defines a template that the \code{current}
#' argument must match.
#'
#' @section alikeness:
#'
#' Generally speaking two objects are alike if they are of the same type (as
#' determined by \code{\link{type_alike}}) and length.  Attributes on the
#' objects are required to be recursively \code{alike}, though the following
#' attributes are treated specially: \code{class}, \code{dim}, \code{dimnames},
#' \code{names}, \code{row.names}, \code{levels}, \code{tsp}, and \code{srcref}.
#'
#' Exactly what makes two objects \code{alike} is complex, but should be
#' intuitive.  The best way to understand "alikeness" is to review the examples.
#' For a thorough exposition see \href{../doc/alike.html}{the vignette}.
#'
#' Note that the semantics of alikeness for language objects, formulas, and
#' functions may change in the future.
#'
#' @export
#' @seealso \code{\link{type_alike}}, \code{\link{type_of}},
#'   \code{\link{abstract}}, \code{\link{vetr_settings}} for more control of
#'   settings
#' @param target the template to compare the object to
#' @param current the object to determine alikeness of to the template
#' @param settings a list of settings generated using \code{vetr_settings}, NULL
#'   for default
#' @param env environment used internally when evaluating expressions; currently
#'   used only when looking up functions to \code{\link{match.call}} when
#'   testing language objects, note that this will be overridden by the
#'   environment specified in \code{settings} if any, defaults to the parent
#'   frame.
#' @return TRUE if target and current are alike, character(1L) describing why
#'   they are not if they are not
#' @examples
#' ## Type comparison
#' alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
#' alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
#' alike(1.1, 1L)         # TRUE, by default, integers are always considered real
#'
#' alike(1:100, 1:100 + 0.0)  # TRUE
#'
#' ## We do not check numerics for integerness if longer than 100
#' alike(1:101, 1:101 + 0.0)
#'
#' ## Scalarness can now be checked at same time as type
#' alike(integer(1L), 1)            # integer-like and length 1?
#' alike(logical(1L), TRUE)         # logical and length 1?
#' alike(integer(1L), 1:3)
#' alike(logical(1L), c(TRUE, TRUE))
#'
#' ## Zero length match any length of same type
#' alike(integer(), 1:10)
#' alike(1:10, integer())   # but not the other way around
#'
#' ## Recursive objects compared recursively
#' alike(
#'   list(integer(), list(character(), logical(1L))),
#'   list(1:10, list(letters, TRUE))
#' )
#' alike(
#'   list(integer(), list(character(), logical(1L))),
#'   list(1:10, list(letters, c(TRUE, FALSE)))
#' )
#'
#' ## `NULL` is a wild card when nested within recursive objects
#' alike(list(NULL, NULL), list(iris, mtcars))
#' alike(NULL, mtcars)    # but not at top level
#'
#' ## Since `data.frame` are lists, we can compare them recursively:
#' iris.fake <- transform(iris, Species=as.character(Species))
#' alike(iris, iris.fake)
#'
#' ## we even check attributes (factor levels must match)!
#' iris.fake2 <- iris
#' levels(iris.fake2$Species) <- c("setosa", "versicolor", "africana")
#' alike(iris, iris.fake2)
#'
#' ## We can use partially specified objects as templates
#' iris.tpl <- abstract(iris)
#' str(iris.tpl)
#' alike(iris.tpl, iris)
#' ## any row sample of iris matches our iris template
#' alike(iris.tpl, iris[sample(1:nrow(iris), 10), ])
#' ## but column order matters
#' alike(iris.tpl, iris[c(2, 1, 3, 4, 5)])
#'
#' ## 3 x 3 integer
#' alike(matrix(integer(), 3, 3), matrix(1:9, nrow=3))
#' ## 3 x 3, but not integer!
#' alike(matrix(integer(), 3, 3), matrix(runif(9), nrow=3))
#' ## partial spec, any 3 row integer matrix
#' alike(matrix(integer(), 3), matrix(1:12, nrow=3))
#' alike(matrix(integer(), 3), matrix(1:12, nrow=4))
#' ## Any logical matrix (but not arrays)
#' alike(matrix(logical()), array(rep(TRUE, 8), rep(2, 3)))
#'
#' ## In order for objects to be alike, they must share a family
#' ## tree, not just a common class
#' obj.tpl <- structure(TRUE, class=letters[1:3])
#' obj.cur.1 <-  structure(TRUE, class=c("x", letters[1:3]))
#' obj.cur.2 <-  structure(TRUE, class=c(letters[1:3], "x"))
#'
#' alike(obj.tpl, obj.cur.1)
#' alike(obj.tpl, obj.cur.2)
#'
#' ## You can compare language objects; these are alike if they are self
#' ## consistent; we don't care what the symbols are, so long as they are used
#' ## consistently across target and current:
#'
#' ## TRUE, symbols are consistent (adding two different symbols)
#' alike(quote(x + y), quote(a + b))
#' ## FALSE, different function
#' alike(quote(x + y), quote(a - b))
#' ## FALSE, inconsistent symbols
#' alike(quote(x + y), quote(a + a))

alike <- function(target, current, env=parent.frame(), settings=NULL)
  .Call(VALC_alike_ext, target, current, substitute(current), env, settings)

