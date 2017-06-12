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

void VALC_reset_track_hash(struct track_hash * track_hash, size_t idx) {
  for(size_t i = idx; i <= track_hash->idx; i--) {
    int del_res = pfHashDel(track_hash->hash, track_hash->contents[i]);
    if(del_res)
      // nocov start
      error(
        "Internal Error: unable to delete key %s; contact maintainer.",
        track_hash->contents[i]
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

size_t VALC_add_to_track_hash(
  struct track_hash * track_hash, const char * key, const char * value
) {
  size_t res = 1;
  int res_set = pfHashSet(track_hash->hash, key, value);

  if(res_set < 0) {
    // nocov start
    error(
      "Internal Error: failed setting value in hash table, contact maintainer."
    );
    // nocov end
  } else if(res_set) {
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
          "than long limit"
        );
        // nocov end
      }
      // re-allocate, note that we are re-allocating an array of pointers to
      // strings, but `S_realloc` is looking for a (char *) hence the coersion

      S_realloc(
        (char *) track_hash->contents, new_size, track_hash->idx_max,
        sizeof(char *)
      );
      track_hash->idx_max = res = new_size;
    } else if (track_hash->idx > track_hash->idx_max) {
      // nocov start
      error("Internal Error: hash index corrupted; contact maintainer.");
      // nocov end
    }
    // We incur some cost in duplicating string here which may not be strictly
    // necessary since it's most likely every string we use here should be
    // present for the duration of execution, but cost is probably reasonably
    // low.  Should revisit if this turns out to be wrong.

    char * key_cpy = CSR_strmcpy(key, CSR_MAX_CHAR);
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
 */

SEXP VALC_track_hash_test(SEXP keys, SEXP size) {
  if(TYPEOF(keys) != STRSXP) error("Arg keys must be character");
  if(TYPEOF(size) != INTSXP) error("Arg size must be integer");

  R_xlen_t i, key_size = XLENGTH(keys);
  SEXP res = PROTECT(allocVector(INTSXP, key_size));

  struct track_hash * track_hash = VALC_create_track_hash(asInteger(size));

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
      INTEGER(res)[i] =
        VALC_add_to_track_hash(track_hash, CHAR(STRING_ELT(keys, i)), "42");
    }
  }
  UNPROTECT(1);
  return(res);
}
