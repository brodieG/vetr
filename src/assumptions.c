/*
Copyright (C) 2017  Brodie Gaslam

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

#include <Rinternals.h>

/*
 * Check all the assumptions we're making
 *
 * Intended to be run onload to make sure there isn't some weird system where
 * our baseline assumptions are not met
 */
void VALC_check_assumptions() {
  if(sizeof(R_len_t) < sizeof(int))
    error("Failed system assumption: R_len_t is not gte to int");
  if(sizeof(char) != 8)
    error("Failed system assumption: sizeof(char) is not 8");
  if(sizeof(int) < 32)
    error("Failed system assumption: sizeof(int) is less than 32");

  if(INT_MIN != NA_INTEGER) {
    error(
      "%s%s",
      "Failed system assumption: INT_MIN != NA_INTEGER but the code in this ",
      "package assumes that they are equal; please contact maintainer."
    );
  }
  if(R_XLEN_T_MAX >= DOUBLE_MAX) {
    error("Failed system assumption: R_XLEN_T_MAX is not less than DOUBLE_MAX");
  }
}
