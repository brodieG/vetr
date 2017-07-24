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
#include "cstringr.h"

/*
 * Truncates strings to specified length.
 *
 * Will re-encode any encoded strings into UTF-8.
 *
 * string character vector of strings to truncate
 * len scalar integer of length to truncate to
 * mark_trunc scalar logical whether to append a ".." to indicate that
 *   a string was truncated
 *
 * Note that we always allocate a STRSXP of the same length as `string`, even if
 * we end up not trimming any of the elements.  We only create new CHARSXP for
 * the elements that are changed (assuming it's okay to re-use the same CHARSXP
 * in different strings).
 */

SEXP CSR_strsub(SEXP string, SEXP chars, SEXP mark_trunc) {
  if(CHAR_BIT != 8)
    error("Internal Error: can only work with 8 bit characters");

  if(TYPEOF(string) != STRSXP)
    stop("Argument `string` must be a string.");
  if(TYPEOF(mark_trunc) != LGLSXP && xlength(mark_trunc) != 1)
    stop("Argument `mark_trunc` must be a TRUE or FALSE.");

  if(
    TYPEOF(chars) != INTSXP || xlength(chars) != 1 || INTEGER(chars)[0] < 1
  )
    stop(
      "Argument `chars` must be scalar integer, strictly positive, and not NA."
    );

  R_xlen_t i, len = xlength(string);
  int mark = asInteger(mark_trunc) > 0;
  int chars_int = asInteger(chars);

  const char * pad = ".."; // padding for truncated strings
  const int pad_len = 2;   // make sure this aligns with `pad`

  if(chars_int - mark * pad_len < 1)
    stop(
      "Argument `chars` must be greater than 2 when `mark_trunc` is TRUE."
    );

  res_string = PROTECT(allocVector(STRSXP, len));

  for(i = 0; i < len; ++i) {
    SEXP str_elt = STRING_ELT(string, i);
    cetype_t chr_enc = getCharCE(str_elt);
    const char * char_point;

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
    R_xlen_t char_count = 0;
    int char_val;

    // Limiting to 8 less than SIZE_T_MAX to make room for a last 4 byte UTF8
    // character, '..', and the NULL terminator

    size_t byte_count = 0, byte_count_prev, byte_count_prev_prev,
      size_t_lim = SIZE_T_MAX - 4 - byte_pad - 1;

    // if you change this, you must adapt position tracking below

    size_t byte_pad = pad_len;
    int is_utf8 = 0, invalid_utf8 = 0;

    // Loop while no NULL character

    while(
      char_val = *(char_point + byte_count) &&
      char_count < chars_int
    ) {
      if(byte_count >= size_t_lim)
        error("Internal Error: size_t overflow.") // nocov, should never happen

      // Keep track of the byte position two characters ago

      if(char_count > 1) byte_count_prev_prev = byte_count_prev;
      if(char_count) byte_count_prev = byte_count;

      ++char_count;
      ++byte_count;  // increment once for ASCII
      if(char_head > 127) {
        // Should be UTF8, so check 4 most significant bits of first  byte for
        // number of chars, valid values are 1111, 1110, 1100, and 1000,
        // non-UTF8 byte are counted as one character by the ++byte_count above

        is_utf8 = 1;
        int char_head = char_val >> 4;
        int bytes;

        switch(char_head) {
          case 15: // 1111
            byte_count +=3;
            break;
          case 14: // 1110
            byte_count +=2;
            break;
          case 12: // 1100
            byte_count +=1;
            break;
          default:  // probably shouldn't be allowed to happen
            invalid_utf8 = 1;  // don't do anything with this for now
    } } }
    if(byte_count >= INT_MAX - byte_pad)
      // nocov start
      error(
        "%s - %s %s at index %.0f",
        "Internal Error: Encountered string longer than INT_MAX",
        CSR_num_as_chr((double) byte_pad, 1), (double) i
      );
      // nocov end

    // Check whether we got to end of string, and if we did truncate

    SEXP char_sxp;

    if(char_count >= chars_int && char_val) {
      char * char_res;
      char * char_trunc = CSR_strmcpy(
        char_point, mark ? byte_count_prev_prev : byte_count
      );
      if(mark) {
        // add an ellipsis at the end.  This is inefficient since we copy the
        // string again, but probably not worth the work to do it in one step.
        // Also probably don't need the CSR fun.

        char_res = R_alloc(byte_count, sizeof(char));
        int snp_try = snprintf(char_res, byte_count, "%s..", char_trunc);
        if(snp_try < 0)
          error(
            "Internal Error: failed generating truncated string at index %.0f",
            (double) i
          );
      } else char_res = char_trunc;

      char_sxp = PROTECT(
        is_utf8 ?  mkCharCE(char_res, CE_UTF8) : mkChar(char_res)
      )
    } else {
      char_sxp = PROTECT(STRING_ELT(string, i));
    }
    // Deal with incorrectly encoded strings? At this point we just let them
    // through since presumably `translateToUTF8` should have dealt with them

    SET_STRING_ELT(res_string, char_sxp);
  }
  UNPROTECT(2);
  return res_string;
}
