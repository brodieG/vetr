/*
Copyright (C) 2020 Brodie Gaslam

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
#include "pfhash.h"
#include "settings.h"

#ifndef _TRACK_HASH_H
#define _TRACK_HASH_H

  /*
   * Note: last value written to `contents` is at ->idx - 1, if ->idx is zero,
   * then the list is empty
   */

  struct track_hash {
    pfHashTable * hash;
    char ** contents;          // an array of characters
    size_t idx;                // location after last value in contents
    size_t idx_max;            // how big the contents are
  };
  struct track_hash * VALC_create_track_hash(size_t size_init);
  int VALC_add_to_track_hash(
    struct track_hash * track_hash, const char * key, const char * value,
    size_t max_nchar
  );
  void VALC_reset_track_hash(
    struct track_hash * track_hash, size_t idx
  );
  SEXP VALC_track_hash_test(SEXP keys, SEXP size);

#endif
