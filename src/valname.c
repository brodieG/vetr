/*
Copyright (C) 2023 Brodie Gaslam

This file is part of "vetr - Trust, but Verify"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#include "alike.h"

/*
 * External version for testing.  See src/r-copied.c for implementation.
 */
SEXP ALIKEC_is_valid_name_ext(SEXP name) {
  if(TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
    error("Argument `name` must be character(1L)");
  return ScalarLogical(ALIKEC_is_valid_name(CHAR(asChar(name))));
}
