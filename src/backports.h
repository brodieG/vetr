/*
Copyright (C) Brodie Gaslam

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

This file contains code copied from the R's Writing R Extensions manual and from
the R sources.  Original copyright notices follow.
*/
/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2025  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* 
 * Include after R header files 
 *
 * Code necessitated by the API restrictions implemented in R4.6+
 */

#include <Rversion.h>
#if R_VERSION < R_Version(4, 4, 1)
#define allocLang Rf_allocLang

static SEXP Rf_allocLang(int n)
{
    if (n > 0)
          return LCONS(R_NilValue, Rf_allocList(n - 1));
    else
          return R_NilValue;
}
#endif

#if R_VERSION < R_Version(4, 5, 0)
#include <stdbool.h>
# define R_ClosureFormals(x) FORMALS(x)
# define R_ParentEnv(x) ENCLOS(x)
// Annoyingly Rboolean is being "phased out" but in 4.5 true/false is used so we
// need to include them for < 4.5.
SEXP R_getVarEx(SEXP sym, SEXP rho, Rboolean inherits, SEXP ifnotfound);
SEXP R_getVar(SEXP sym, SEXP rho, Rboolean inherits);
#endif

#if R_VERSION < R_Version(4, 6, 0)

SEXP R_getAttributes(SEXP x);
#endif
