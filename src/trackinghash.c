#include "pfhash.h"
#include "cstringr.h"
/*
 * Note: last value written to `contents` is at ->idx - 1, if ->idx is zero,
 * then the list is empty
 */

struct track_hash {
  pfHashTable * hash;
  const char ** contents;    // an array of characters
  size_t idx;                // location after last value in contents
  size_t idx_max;            // how big the contents are
};


struct track_hash * create_track_hash(size_t size_init) {

  pfHashTable * hash = pfHashCreate(NULL);
  const char ** contents = R_alloc(size_init, sizeof(char *));
  struct track_hash * track_hash = R_alloc(1, sizeof(struct track_hash));

  track_hash->hash = hash;
  track_hash->contents = contents;
  track_hash->idx = 0;
  track_hash->idx_max = size_init;

  return track_hash;
}
/*
 * Restores hash to original state at index idx by removing all entries in
 * contents that were defined after that point
 *
 * Modifies the hash table by reference.
 */

void reset_track_hash(struct * track_hash, size_t idx) {
  for(size_t i = idx; i < track_hash->idx; i--) {
    int del_res = pfHashDel(track_hash->hash, track_hash->contents[idx]);
    if(int del_res)
      // nocov start
      error(
        "Internal Error: unable to delete key %s; contact maintainer.",
        track_hash->contents[idx]
      );
      // nocov end
  }
  track_hash->idx = idx;
}

/* Add an item to the hash table
 *
 * If it already exists return 1, else 0
 *
 * Modifies track_hash by reference
 */

int add_to_track_hash(
  struct * track_hash, const char * key, const char * value
) {
  int res_set = pfHashSet(track_hash, key, value);
  int res = 0;

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
      // re-allocate
      S_realloc(
        track_hash->contents, new_size, track_hash->idx_max, sizeof(char *)
      );
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

    char * key_cpy = CSR_strmcpy(key, VALC_MAX_CHAR);
    trach_hash->contents[track_hash->idx] = key_cpy;
    track_hash->idx++;  // shouldn't be overflowable
    res = 1;
  }
  return res;
}
