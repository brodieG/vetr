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
 * C level equivalent of `attributes()`, returns NULL or the attributes as a
 * VECSXP.
 *
 * R_getAttributes was introduced in R 4.6.0. This backport uses ATTRIB which is
 * still available in earlier R versions.
 *
 * The return value MUST BE PROTECTED.
 *
 * This code is from ~r89313
 */

#include <Rinternals.h>
#include <Rversion.h>

#if R_VERSION < R_Version(4, 6, 0)
SEXP R_getAttributes(SEXP x)
{
    if (TYPEOF(x) == ENVSXP)
	R_CheckStack(); /* in case attributes might lead to a cycle */

    SEXP attrs = ATTRIB(x), namesattr;
    int nvalues = length(attrs);
    if (isList(x)) {
	namesattr = getAttrib(x, R_NamesSymbol);
	if (namesattr != R_NilValue)
	    nvalues++;
    } else
	namesattr = R_NilValue;
    /* FIXME */
    if (nvalues <= 0)
	return R_NilValue;
    /* FIXME */
    SEXP value, names;
    PROTECT(namesattr);
    PROTECT(value = allocVector(VECSXP, nvalues));
    PROTECT(names = allocVector(STRSXP, nvalues));
    nvalues = 0;
    if (namesattr != R_NilValue) {
	SET_VECTOR_ELT(value, nvalues, namesattr);
	SET_STRING_ELT(names, nvalues, PRINTNAME(R_NamesSymbol));
	nvalues++;
    }
    while (attrs != R_NilValue) {
	SEXP tag = TAG(attrs);
	if (TYPEOF(tag) == SYMSXP) {
	    SET_VECTOR_ELT(value, nvalues, getAttrib(x, tag));
	    SET_STRING_ELT(names, nvalues, PRINTNAME(tag));
	}
	else { // empty tag, hence name = ""
            // Not reachable AFAIK
            // nocov start
	    MARK_NOT_MUTABLE(CAR(attrs));
	    SET_VECTOR_ELT(value, nvalues, CAR(attrs));
	    SET_STRING_ELT(names, nvalues, R_BlankString);
            // nocov end
	}
	attrs = CDR(attrs);
	nvalues++;
    }
    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(3);
    return value;
}
#endif
