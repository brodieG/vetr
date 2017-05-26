#include "alike.h"
/*
Functions used to manage tracking recursion into list like objects

To track recursion you must:
- initalize the rec track object first,
- increment each time you recurse,
- decrement each time you recurse,
- mark lvl_max when you hit an error
*/
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Allocate the storage for the indices; should be done at first error, then
as we unwind recursion we record the values of the indices for each level
prior to the error

By design the rec.lvl should be 0 if there is no recursion.
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_init(struct ALIKEC_rec_track rec) {
  if(rec.lvl) {
    rec.indices = (struct ALIKEC_index *)
      R_alloc(rec.lvl, sizeof(struct ALIKEC_index));
  }
  return rec;
}
/*
After error has been found, populate the last value in our index tracking
structure, AND decrement the index level.

See ALIKEC_rec_ind_init for details on index structure
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_set(
  struct ALIKEC_rec_track rec, struct ALIKEC_index ind
) {
  // Initialize indices if not initialized

  if(!rec.indices) {
    rec = ALIKEC_rec_ind_init(rec);
    rec.lvl_max = rec.lvl;
  }
  // Find correct spot in previously allocated indices spaces, clearly relies on
  // lvl being exactly correct...

  struct ALIKEC_index * cur_ind = rec.indices + rec.lvl - 1;
  *cur_ind = ind;
  return rec;
}
/*
Record character or numeric index values
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_chr(
  struct ALIKEC_rec_track res, const char * ind
) {
  union ALIKEC_index_raw ind_u = {.chr = ind};
  return ALIKEC_rec_ind_set(res, (struct ALIKEC_index) {ind_u, 1});
}
struct ALIKEC_rec_track ALIKEC_rec_ind_num(
  struct ALIKEC_rec_track res, R_xlen_t ind
) {
  union ALIKEC_index_raw ind_u = {.num = ind};
  return ALIKEC_rec_ind_set(res, (struct ALIKEC_index) {ind_u, 0});
}
struct ALIKEC_rec_track ALIKEC_rec_def() {
  return (struct ALIKEC_rec_track) {
    .lvl = 0,
    .lvl_max = 0,
    .indices = 0,  // NULL pointer
    .envs = 0,     // NULL pointer
    .gp = 0
  };
}
/*
increment recursion

decrementing happens via ALIKEC_rec_ind_set
*/
struct ALIKEC_rec_track ALIKEC_rec_inc(struct ALIKEC_rec_track rec) {
  size_t lvl_old = rec.lvl;
  rec.lvl++;
  if(rec.lvl < lvl_old) {
    // nocov start
    error(
      "Internal Error: %s; contact maintainer.",
      "max recursion depth exceeded, this really shouldn't happen"
    );
    // nocov end
  }
  return rec;
}
struct ALIKEC_rec_track ALIKEC_rec_dec(struct ALIKEC_rec_track rec) {
  if(!rec.lvl) {
    // nocov start
    error(
      "Internal Error: %s; contact maintainer.",
      "tried to decrement rec counter below zero"
    );
    // nocov end
  }
  rec.lvl--;
  return rec;
}
/*
Closely related to ALIKEC_rec_ind_as_chr except that it return a list (vector)
with the language call with all the indices subset, and the pointer to the
location in the language call that needs to be substituted.
*/
SEXP ALIKEC_rec_ind_as_lang(struct ALIKEC_rec_track rec) {
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  setAttrib(res, ALIKEC_SYM_syntacticnames, ScalarLogical(1));
  SEXP lang = PROTECT(list1(R_NilValue));
  SEXP lang_cpy = lang;

  if(rec.lvl_max) {  // Recursion occurred
    // Make call to `[[` or `$`.  CAR is the `[[` or `$`, CADDR is the index
    // value, and CADR is the spot that will be filled in with what is being
    // subsetted: CADR$CADDR or CADR[[CADDR]]

    for(size_t i = rec.lvl_max; i > 0; i--) {
      size_t j = i - 1;
      SEXP index_call = PROTECT(lang3(R_NilValue, R_NilValue, R_NilValue));
      switch(rec.indices[j].type) {
        case 0:
          SETCAR(index_call, R_Bracket2Symbol);
          SETCADDR(index_call, ScalarReal(rec.indices[j].ind.num));
          break;
        case 1:
          SETCAR(index_call, R_DollarSymbol);
          SETCADDR(index_call, install(rec.indices[j].ind.chr));
          if(!ALIKEC_is_valid_name(rec.indices[j].ind.chr))
            setAttrib(res, ALIKEC_SYM_syntacticnames, ScalarLogical(0));
          break;
        default: {
          // nocov start
          error(
            "Internal Error: unexpected index type %d; contact maintainer.",
            rec.indices[j].type
          );
          // nocov end
      } }
      SETCAR(lang, index_call);
      UNPROTECT(1);
      lang = CDR(index_call);
    }
    SET_VECTOR_ELT(res, 0, CAR(lang_cpy));
    SET_VECTOR_ELT(res, 1, lang);
  }
  UNPROTECT(2);
  return res;
}
