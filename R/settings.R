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

#' Generate Control Settings For vetr and alike
#'
#' Utility function to generate setting values.  We strongly recommend
#' that you generate the settings outside of function calls so that setting
#' generation does not become part of the \code{vet/vetr/alike} evaluation as
#' that could add noticeable overhead to the function evaluation.
#'
#' Settings after `fuzzy.int.max.len` are fairly low level and exposed mostly
#' for testing purposes.  You should generally not need to use them.
#'
#' Note that a successful evaluation of this function does not guarantee a
#' correct settings list.  Those checks are carried out internally by
#' \code{vet/vetr/alike}.
#'
#' @seealso \code{\link{type_alike}}, \code{\link{alike}}, \code{\link{vetr}}
#' @export
#' @param type.mode integer(1L) in 0:2, defaults to 0, determines how object
#'   types (as in `typeof`) are compared: \itemize{
#'     \item 0: integer like numerics (e.g. \code{1.0}) can match against
#'       integer templates, and integers always match real templates; all
#'       function types are considered of the same type
#'     \item 1: integers always match against numeric templates, but not vice
#'       versa, and integer-like numerics are treated only as numerics;
#'       functions only match same function type (i.e. closures only match
#'       closures, builtins builtins, and specials specials)
#'     \item 2: types must be equal for all objects types (for functions, this
#'       is unchanged from 1)
#'   }
#' @param attr.mode integer(1L) in 0:2, defaults to 0,  determines strictness of
#'   attribute comparison: \itemize{
#'     \item \code{0} only checks attributes that are present in target, and
#'       uses special comparisons for the special attributes (\code{class},
#'       \code{dim}, \code{dimnames}, \code{names}, \code{row.names},
#'       \code{levels}, \code{srcref}, and \code{tsp}) while requiring other
#'       attributes to be \code{alike}
#'     \item \code{1} is like \code{0}, except all atributes must be
#'       \code{alike}
#'     \item \code{2} requires all attributes to be present in \code{target} and
#'       \code{current} and to be alike
#'   }
#' @param lang.mode integer(1L) in 0:1, defaults to 0, controls language
#'   matching, set to `1` to turn off use of [match.call()]
#' @param fun.mode NOT IMPLEMENTED, controls how functions are compared
#' @param rec.mode integer(1L) `0` currently unused, intended to control how
#'   recursive structures (other than language objects) are compared
#' @param fuzzy.int.max.len max length of numeric vectors to consider for
#'   integer likeness (e.g. `c(1, 2)` can be considered "integer", even
#'   though it is numeric); currently we limit this check to vectors
#'   shorter than 100 to avoid a potentially expensive computation on large
#'   vectors, set to -1 to apply to all vectors irrespective of length
#' @param suppress.warnings logical(1L) suppress warnings if TRUE
#' @param width to use when deparsing expressions; default `-1`
#'   equivalent to \code{getOption("width")}
#' @param env.depth.max integer(1L) maximum number of nested environments to
#'   recurse through, defaults to 65535L; these are tracked to make sure we do
#'   not get into an infinite recursion loop, but because they are tracked we
#'   keep a limit on how many we will go through, set to -1 to allow unlimited
#'   recursion depth.  You should not need to change this unless you are running
#'   into the recursion limit.
#' @param symb.sub.depth.max integer(1L) maximum recursion depth when
#'   recursively substituting symbols in vetting expression, defaults to 65535L
#' @param symb.size.max integer(1L) maximum number of characters that a symbol
#'   is allowed to have in vetting expressions, defaults to 15000L.
#' @param track.hash.content.size integer(1L) (advanced) used to set the initial
#'   size of the symbol tracking vector used with the hash table that detects
#'   recursive symbol substitution.  If the tracking vector fills up it will be
#'   grown by 2x.  This parameter is exposed mostly for developer use.
#' @param nchar.max integer(1L) defaults to 65535L, threshold after which
#'   strings encountered in C code are truncated.  This is the read limit.  In
#'   theory `vetr` can produce strings longer than that by combining multiple
#'   shorter pieces.
#' @param env what environment to use to match calls and evaluate vetting
#'   expressions, although typically you would specify this with the `env`
#'   argument to `vet`; if NULL will use the calling frame to
#'   \code{vet/vetr/alike}.
#' @param result.list.size.init initial value for token tracking.   This will be
#'   grown by a factor of two each time it fills up until we reach
#'   `result.list.size.max`.
#' @param result.list.size.max maximum number of tokens we keep track of,
#'   intended mostly as a safeguard in case a logic error causes us to keep
#'   allocating memory.  Set to 1024 as a default value since it should be
#'   exceedingly rare to have vetting expressions with such a large number of
#'   tokens, enough so that if we reach that number it is more likely something
#'   went wrong.
#' @return list with all the setting values
#' @examples
#' type_alike(1L, 1.0, settings=vetr_settings(type.mode=2))
#' ## better if you are going to re-use settings to reduce overhead
#' set <- vetr_settings(type.mode=2)
#' type_alike(1L, 1.0, settings=set)

vetr_settings <- function(
  type.mode=0L, attr.mode=0L, lang.mode=0L, fun.mode=0L, rec.mode=0L,
  suppress.warnings=FALSE, fuzzy.int.max.len=100L,
  width=-1L, env.depth.max=65535L, symb.sub.depth.max=65535L,
  symb.size.max=15000L, nchar.max=65535L, track.hash.content.size=63L,
  env=NULL, result.list.size.init=64L, result.list.size.max=1024L
) {
  # we just use the function to match parameters
  as.list(environment())
}
