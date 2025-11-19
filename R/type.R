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

#' @export
#' @rdname type_alike

type_of <- function(object)
  .Call(VALC_typeof, object)

#' Fuzzily Compare Types of Objects
#'
#' Type evaluation and comparison is carried out with special treatment for
#' numerics, integers, and function types.  Whole number NA-free numeric vectors
#' of sufficiently short length (<100 by default) representable in the integer
#' type are considered to be type integer.  Closures, built-ins, and specials
#' are all treated as type closure.
#'
#' Specific behavior can be tuned with the `type.mode` parameter to the
#' [vetr_settings()] object passed as the `settings` parameter to this function.
#'
#' @seealso [alike()], [vetr_settings()], in particular the section about
#'   the `type.mode` parameter which affects how this function behaves.
#' @param target the object to test type alikeness against
#' @param current the object to test the type alikeness of
#' @param settings NULL, or a list as produced by [vetr_settings()]
#' @param object the object to check the type of
#' @return For `type_of` character(1L) the type of the object, for `type_alike`
#'   either TRUE, or a string describing why the types are not alike.
#' @export
#' @examples
#' type_of(1.0001)          # numeric
#' type_of(1.0)             # integer (`typeof` returns numeric)
#' type_of(1)               # integer (`typeof` returns numeric)
#' type_of(sum)             # closure (`typeof` returns builtin)
#' type_of(`$`)             # closure (`typeof` returns special)
#'
#' type_alike(1L, 1)
#' type_alike(1L, 1.1)
#' type_alike(integer(), numeric(100))
#' type_alike(integer(), numeric(101))  # too long

type_alike <- function(target, current, settings=NULL)
  .Call(VALC_type_alike, target, current, substitute(current), settings)
