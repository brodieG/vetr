#include "cstringr.h"
#include "pfhash.h"

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
  size_t VALC_add_to_track_hash(
    struct track_hash * track_hash, const char * key, const char * value
  );
  void VALC_reset_track_hash(struct track_hash * track_hash, size_t idx);
  SEXP VALC_track_hash_test(SEXP keys, SEXP size);

#endif
