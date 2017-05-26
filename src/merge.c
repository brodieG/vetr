#include "alike.h"

/*
 * Structure to capture the data to sort as well as the index so that we can
 * recover the sort index after the fact
 */

struct ALIKEC_sort_dat {
  const char * string;
  R_xlen_t index;
};

/*
 * Compare two character vectors a and b to determine if a is less than b or
 * equal.  The trick is that we're actually comparing the first, third, and
 * fourth values of the character vectors
 *
 * We assume we wont get non terminated strings from SEXPs...
 */

int ALIKEC_merge_comp(const void *p, const void *q) {
  struct ALIKEC_sort_dat a = *(struct ALIKEC_sort_dat *) p;
  struct ALIKEC_sort_dat b = *(struct ALIKEC_sort_dat *) q;

  return(strcmp(a.string, b.string));
}
/*
 * Sort a list of 5 length character vectors by the 1st, 2nd, 4th, and 5th
 * elements.  Use the 3rd element for tie breaks to ensure reproducible
 * outcomes.
 *
 * Mixed in one length character vectors are also sorted but obviously by their
 * entire value.  No other lenghts are allowed.
 *
 * Example: c("`names(letters)`", "be", "character", "is", "integer")
 */

SEXP ALIKEC_sort_msg(SEXP msgs) {
  if(TYPEOF(msgs) != VECSXP) {
    error("Expected list argument, got %s", type2char(TYPEOF(msgs)));
  }
  R_xlen_t vec_len = xlength(msgs), i;

  struct ALIKEC_sort_dat * sort_dat =
    (struct ALIKEC_sort_dat *) R_alloc(vec_len, sizeof(struct ALIKEC_sort_dat));

  for(i = 0; i < vec_len; i++) {
    SEXP str_elt = VECTOR_ELT(msgs, i);
    if(
      TYPEOF(str_elt) != STRSXP ||
      (XLENGTH(str_elt) != 5 && XLENGTH(str_elt) != 1)
    ) {
      // nocov start
      error(
        "Internal Error: unexpected string format to merge; contact maintainer"
      );
      // nocov end
    }
    const char * sort_string = "";
    if(XLENGTH(str_elt) == 1) {
      sort_string = CHAR(asChar(str_elt));
    } else {
      // delimiters to minimize susceptibility to frame shift, but obviously not
      // a guarantee

      sort_string = CSR_smprintf6(
        ALIKEC_MAX_CHAR, "%s <:> %s <:> %s <:> %s <:> %s%s",
        CHAR(STRING_ELT(str_elt, 0)), CHAR(STRING_ELT(str_elt, 1)),
        CHAR(STRING_ELT(str_elt, 3)), CHAR(STRING_ELT(str_elt, 4)),
        CHAR(STRING_ELT(str_elt, 2)), ""
      );
    }
    sort_dat[i] = (struct ALIKEC_sort_dat) {
      sort_string,
      i
    };
  }
  qsort(sort_dat, vec_len, sizeof(struct ALIKEC_sort_dat), ALIKEC_merge_comp);

  SEXP msg_sort = PROTECT(allocVector(VECSXP, vec_len));

  for(i = 0; i < vec_len; i++) {
    SET_VECTOR_ELT(msg_sort, i, VECTOR_ELT(msgs, sort_dat[i].index));
  }
  UNPROTECT(1);
  return(msg_sort);
}

/*
 * Combine length five length character vectors where the first, second,
 * fourth and fifth fourth elements are identical.
 *
 * msgs a list of character vectors of the same length.
 *
 * One length vectors are treated as unmergeable.
 */

SEXP ALIKEC_merge_msg(SEXP msgs) {

  R_xlen_t len = XLENGTH(msgs);
  SEXP res;


  if(len > 1) {
    // 1. Sort the strings (really only need to do this if longer than 3, but oh
    // well

    SEXP msg_sort = PROTECT(ALIKEC_sort_msg(msgs));
    R_xlen_t groups = 1;

    // Determine how many groups of similar things there are in our list

    for(R_xlen_t i=1; i < len; i++) {
      SEXP v_elt = VECTOR_ELT(msg_sort, i);
      SEXP v_elt_prv = VECTOR_ELT(msg_sort, i - 1);
      if(
        XLENGTH(v_elt) == 1 ||
        strcmp(CHAR(STRING_ELT(v_elt, 0)), CHAR(STRING_ELT(v_elt_prv, 0))) ||
        strcmp(CHAR(STRING_ELT(v_elt, 1)), CHAR(STRING_ELT(v_elt_prv, 1))) ||
        strcmp(CHAR(STRING_ELT(v_elt, 3)), CHAR(STRING_ELT(v_elt_prv, 3))) ||
        strcmp(CHAR(STRING_ELT(v_elt, 4)), CHAR(STRING_ELT(v_elt_prv, 4)))
      ) {
        ++groups;
      }
    }
    // If we need to condense the list, then allocate it, otherwise just return
    // the original list

    if(groups < len) {
      res = PROTECT(allocVector(VECSXP, groups));
      R_xlen_t k = 0;      // count the index in our result vector
      R_xlen_t j = 0;      // count how many elements in group
      // this will be the concatented second value in our vectors

      const char * target;

      for(R_xlen_t i=0; i < len; i++) {

        SEXP v_elt_nxt = R_NilValue, v_elt = VECTOR_ELT(msg_sort, i);

        if(i < len - 1) {
          v_elt_nxt = VECTOR_ELT(msg_sort, i + 1);
        }
        // Note, we'll only ever acces v_elt_nxt if we're not at the last value
        // in the loop so it is okay for it to be R_NilValue in that iteration

        int next_diff = (i == len - 1) ||
          XLENGTH(v_elt) == 1 ||
          strcmp(CHAR(STRING_ELT(v_elt, 0)), CHAR(STRING_ELT(v_elt_nxt, 0))) ||
          strcmp(CHAR(STRING_ELT(v_elt, 1)), CHAR(STRING_ELT(v_elt_nxt, 1))) ||
          strcmp(CHAR(STRING_ELT(v_elt, 3)), CHAR(STRING_ELT(v_elt_nxt, 3))) ||
          strcmp(CHAR(STRING_ELT(v_elt, 4)), CHAR(STRING_ELT(v_elt_nxt, 4)));

        if(next_diff) {
          SEXP v_elt_d = duplicate(v_elt);
          SET_VECTOR_ELT(res, k, v_elt_d);

          // append with, "or" if necessary and write

          if(j) {
            target = CSR_smprintf4(
              ALIKEC_MAX_CHAR, "%s, or %s",
              target, CHAR(STRING_ELT(v_elt_d, 2)), "", ""
            );
            SET_STRING_ELT(v_elt_d, 2, mkChar(target));
          }
          j = 0;
          ++k;
        } else {
          // more than one value, but not done yet

          if(j) {
            target = CSR_smprintf4(
              ALIKEC_MAX_CHAR, "%s, %s",
              target, CHAR(STRING_ELT(v_elt, 2)), "", ""
            );
          } else target = CHAR(STRING_ELT(v_elt, 2));
          ++j;
        }
      }
    } else {
      res = PROTECT(msgs); // stack balance
    }
  } else res = PROTECT(PROTECT(msgs)); // stack balance

  UNPROTECT(2);
  return res;
}
/*
 * additional layer just collapses the 5 lenght char vectors into one
 */
SEXP ALIKEC_merge_msg_ext(SEXP msgs) {
  SEXP msg_c = PROTECT(duplicate(ALIKEC_merge_msg(msgs)));

  R_xlen_t i;

  for(i = 0; i < XLENGTH(msg_c); i++) {
    SEXP v_elt = VECTOR_ELT(msg_c, i);
    if(XLENGTH(v_elt) == 5) {
      SET_VECTOR_ELT(
        msg_c, i,
        PROTECT(
          mkString(
            CSR_smprintf6(
              ALIKEC_MAX_CHAR, "%sshould %s %s (%s %s)",
              CHAR(STRING_ELT(v_elt, 0)), CHAR(STRING_ELT(v_elt, 1)),
              CHAR(STRING_ELT(v_elt, 2)), CHAR(STRING_ELT(v_elt, 3)),
              CHAR(STRING_ELT(v_elt, 4)), ""
      ) ) ) );
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return msg_c;
}
