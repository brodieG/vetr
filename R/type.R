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

#' A Fuzzier Version of [typeof()]
#'
#' Numerics that are equivalent to integers (e.g `x == floor(x)`) are
#' classified as integers, and builtin and special functions are reported as
#' closures.
#'
#' @param object the object to check the type of
#' @return character(1L) the type of the object
#' @export
#' @examples
#'
#' type_of(1.0001)          # numeric
#' type_of(1.0)             # integer (`typeof` returns numeric)
#' type_of(1)               # integer (`typeof` returns numeric)
#' type_of(sum)             # closure (`typeof` returns builtin)
#' type_of(`$`)             # closure (`typeof` returns special)

type_of <- function(object)
  .Call(VALC_typeof, object)

#' Compare Types of Objects
#'
#' By default, checks [type_of()] objects and two objects are
#' considered `type_alike` if they have the same type.  There is special
#' handling for integers, numerics, and functions.
#'
#' For integers and numerics, if `current` is integer or integer-like
#' (e.g. 1.0) it will match real or integer `target` values.  Closures,
#' built-ins, and specials are all treated as type function.
#'
#' Specific behavior can be tuned with the `type.mode` parameter to the
#' [vetr_settings()] object passed as the `settings` parameter to this function.
#'
#' @seealso type_of, alike, [vetr_settings()], in particular the section about
#'   the `type.mode` parameter which affects how this function behaves.
#' @param target the object to test type alikeness against
#' @param current the object to test the type alikeness of
#' @param settings NULL, or a list as produced by [vetr_settings()]
#' @export

type_alike <- function(target, current, settings=NULL)
  .Call(VALC_type_alike, target, current, substitute(current), settings)
