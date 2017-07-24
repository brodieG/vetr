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

/*
 * Truncates strings to specified lenght.
 *
 * Will re-encode any encoded strings into UTF-8.
 *
 * string character vector of strings to truncate
 * len scalar integer of length to truncate to
 * mark_trunc scalar logical whether to append a ".." to indicate that
 *   a string was truncated
 *
 *
 */

SEXP CSR_strsub(SEXP string, SEXP chars, SEXP mark_trunc) {
  if(TYPEOF(string) != STRSXP)
    stop("Argument `string` must be a string.");
  if(
    TYPEOF(chars) != INTSXP || xlength(chars) != 1 || INTEGER(chars)[0] < 1
  )
    stop(
      "Argument `chars` must be scalar integer, strictly positive, and not NA."
    );

  R_xlen_t i, len = xlength(string);

  if(CHAR_BIT != 8)
    error("Internal Error: can only work with 8 bit characters");

  for(i = 0; i < len; ++i) {
    SEXP str_elt = STRING_ELT(string, i);
    cetype_t chr_enc = getCharCE(str_elt);
    const char * char_val;

    switch(chr_enc) {
      case CE_NATIVE:
      case CE_UTF8:
        char_val = CHAR(STRING_ELT(string, i));
        break;
      case CE_LATIN1:
        char_val = translateCharUTF8(STRING_ELT(string, i));
        break;
      default:
        // nocov start
        error(
          "%s%s",
          "Internal Error: unexpected character encoding; ",
          "contact maintainer."
        );
        // nocov end
    }
    int char_count = 0;
    while(*char_val) {
      if(*char_val < 128) { // ASCII
        ++char_count;
      } else {
        // Should be UTF8, so check first byte for number of chars, valid values
        // are 1111, 1110, 1100, and 1000

        const char * char_head = *char_val >> 4;
        





      }

    }


  }


}
