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

#include "pfhash.h"
#include "cstringr.h"
#include "trackinghash.h"

/*
 * Implement a hash table with ancillary data that intended to allow checking
 * that a value exists in a hash table, and also quickly erase the last n values
 * that were written to the hash table
 *
 * size_init is the initial size of the content tracking character array, and
 * has no effect on the actual hash table.
 */
struct track_hash * VALC_create_track_hash(size_t size_init) {
  pfHashTable * hash = pfHashCreate(NULL);
  char ** contents = (char **) R_alloc(size_init, sizeof(char *));
  struct track_hash * track_hash =
    (struct track_hash *) R_alloc(1, sizeof(struct track_hash));

  track_hash->hash = hash;
  track_hash->contents = contents;
  track_hash->idx = 0;
  track_hash->idx_max = size_init;

  return track_hash;
}

/*
 * Restores hash to original state at index idx by removing all entries in
 * contents that were defined up to and including that point.
 *
 * reset_track_hash(x, 0) will remove all entries.
 *
 * Modifies the hash table by reference.
 */

void VALC_reset_track_hash(
  struct track_hash * track_hash, size_t idx
) {
  for(size_t i = track_hash->idx; i > idx; --i) {

    int del_res = pfHashDel(track_hash->hash, track_hash->contents[i - 1]);
    if(del_res)
      // nocov start
      error(
        "Internal Error: unable to delete key %s; contact maintainer.",
        track_hash->contents[i - 1]
      );
      // nocov end
  }
  track_hash->idx = idx;
}
/* Add an item to the hash table
 *
 * If it already exists return 0, else if it doesn't exist 1, unless an
 * allocation is required in which case return the size of the allocation.
 *
 * Modifies track_hash by reference
 */

int VALC_add_to_track_hash(
  struct track_hash * track_hash, const char * key, const char * value,
  size_t max_nchar
) {
  int res = 1;
  int res_set = pfHashSet(track_hash->hash, key, value);

  if(res_set < 0) {
    // nocov start
    error(
      "Internal Error: failed setting value in hash table, contact maintainer."
    );
    // nocov end
  } else if(res_set) {
    // Already existed, so no need to add
    res = 0;
  } else {
    // Need to add a value to the hash, first make sure that there is enough
    // room in the content tracking to hold it, and if not double the size of
    // the tracking list

    if(track_hash->idx == track_hash->idx_max) {

      // first, make sure no issues with size_t -> long

      size_t new_size = CSR_add_szt(track_hash->idx_max, track_hash->idx_max);
      size_t max_long = 1;
      max_long = (max_long << sizeof(long)) / 2;

      if(new_size > max_long) {
        // nocov start
        error(
          "Internal Error: attempted to allocate hash content vector bigger ",
          "than int size."
        );
        // nocov end
      }
      // re-allocate, note that we are re-allocating an array of pointers to
      // strings, but `S_realloc` is looking for a (char *) hence the coersion

      track_hash->contents = (char **) S_realloc(
        (char *) track_hash->contents, (long) new_size,
        (long) track_hash->idx_max,
        sizeof(char *)
      );
      res = (int) new_size;
      track_hash->idx_max = new_size;
    } else if (track_hash->idx > track_hash->idx_max) {
      // nocov start
      error("Internal Error: hash index corrupted; contact maintainer.");
      // nocov end
    }
    // We incur some cost in duplicating string here which may not be strictly
    // necessary since it's most likely every string we use here should be
    // present for the duration of execution, but cost is probably reasonably
    // low.  Should revisit if this turns out to be wrong.

    char * key_cpy = CSR_strmcpy(key, max_nchar);
    track_hash->contents[track_hash->idx] = key_cpy;
    track_hash->idx++;  // shouldn't be overflowable
  }
  return res;
}
/*
 * External function for testing
 *
 * Any NA values in `keys` are taken to mean to take the `as.numeric` value
 * of the next element as the reset index.
 *

   hash tracking, uses a hash to detect potential collisions, 1 means a value
   is added, >1 means a value was added and tracking array had to be resized
   to that size, 0 means it existed already, NA is a reset instruction, value
   following a reset instruction is what the reset was to
 */

SEXP VALC_track_hash_test(SEXP keys, SEXP size) {
  if(TYPEOF(keys) != STRSXP) error("Arg keys must be character");
  if(TYPEOF(size) != INTSXP) error("Arg size must be integer");

  R_xlen_t i;
  R_xlen_t key_size = xlength(keys);
  SEXP res = PROTECT(allocVector(INTSXP, key_size));

  struct track_hash * track_hash = VALC_create_track_hash(asInteger(size));
  struct VALC_settings set = VALC_settings_init();

  for(i = 0; i < key_size; ++i) {
    if(STRING_ELT(keys, i) == NA_STRING) {
      INTEGER(res)[i] = NA_INTEGER;
      if(++i < key_size) {
        int reset_int = atoi(CHAR(STRING_ELT(keys, i)));
        if(reset_int < 0) error("Internal Error: negative reset key.");
        VALC_reset_track_hash(track_hash, (size_t) reset_int);
        INTEGER(res)[i] = reset_int;
      }
    } else {
      int add_res = VALC_add_to_track_hash(
        track_hash, CHAR(STRING_ELT(keys, i)), "42", set.nchar_max
      );
      INTEGER(res)[i] = add_res;
    }
  }
  UNPROTECT(1);
  return(res);
}
