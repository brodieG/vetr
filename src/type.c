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

#include "alike.h"

/*
compare types, accounting for "integer like" numerics; empty string means
success, otherwise outputs an a character string explaining why the types are
not alike

call is substituted current, only used when this is called by type_alike directly otherwise doesn't do much
*/
struct ALIKEC_res ALIKEC_type_alike_internal(
  SEXP target, SEXP current, struct VALC_settings set
) {
  SEXPTYPE tar_type, cur_type, tar_type_raw, cur_type_raw;
  int int_like = 0;
  tar_type_raw = TYPEOF(target);
  cur_type_raw = TYPEOF(current);

  struct ALIKEC_res res = ALIKEC_res_init();

  if(tar_type_raw == cur_type_raw) return res;

  tar_type = tar_type_raw;
  cur_type = cur_type_raw;

  if(set.type_mode == 0) {
    if(
      tar_type_raw == INTSXP && (
        set.fuzzy_int_max_len < 0 ||
        (
          xlength(target) <= set.fuzzy_int_max_len &&
          xlength(current) <= set.fuzzy_int_max_len
      ) )
    ) {
      int_like = 1;
    }
    if(int_like || (
        tar_type_raw == CLOSXP || tar_type_raw == SPECIALSXP ||
        tar_type_raw == BUILTINSXP
      )
    ) {
      tar_type = ALIKEC_typeof_internal(target);
      cur_type = ALIKEC_typeof_internal(current);
    }
  }
  if(tar_type == cur_type) return res;
  if(
    cur_type == INTSXP && set.type_mode < 2 &&
    (tar_type == INTSXP || tar_type == REALSXP)
  ) {
    return res;
  }
  const char * what;

  if(set.type_mode == 0 && int_like) {
    what = "integer-like";
  } else if (set.type_mode < 2 && tar_type == REALSXP) {
    what = "numeric";
  } else if (set.type_mode == 0 && tar_type == CLOSXP) {
    what = "function";
  } else {
    what = type2char(tar_type);
  }
  struct ALIKEC_res res_fin = res;

  res_fin.success = 0;
  res_fin.dat.strings.target[0]= "type \"%s\"";
  res_fin.dat.strings.target[1]= what;
  res_fin.dat.strings.current[0] = "\"%s\"";
  res_fin.dat.strings.current[1] = type2char(cur_type);
  res_fin.wrap = allocVector(VECSXP, 2); // note not PROTECTing b/c return
  return res_fin;
}
SEXP ALIKEC_type_alike(
  SEXP target, SEXP current, SEXP call, SEXP settings
) {
  struct ALIKEC_res res;
  struct VALC_settings set = VALC_settings_vet(settings, R_BaseEnv);

  res = ALIKEC_type_alike_internal(target, current, set);
  PROTECT(res.wrap);
  SEXP res_sexp;
  if(!res.success) {
    res_sexp = PROTECT(ALIKEC_res_as_string(res, call, set));
  } else {
    res_sexp = PROTECT(ScalarLogical(1));
  }
  UNPROTECT(2);
  return(res_sexp);
}

/* - typeof ----------------------------------------------------------------- */

SEXPTYPE ALIKEC_typeof_internal(SEXP object) {
  double * obj_real;
  SEXPTYPE obj_type = TYPEOF(object);

  switch(obj_type) {
    case REALSXP:
      {
        R_xlen_t obj_len = XLENGTH(object), i;
        obj_real = REAL(object);
        /*
        could optimize this more by using the magic number tricks or bit
        fiddling, but at end of day this still wouldn't be fast enough to
        realistically use on a very large vector, so it doesn't really matter
        */
        for(i = 0; i < obj_len; i++) {
          if(
            (isnan(obj_real[i]) || !isfinite(obj_real[i])) ||
            obj_real[i] != (int)obj_real[i]
          )
            return REALSXP;
        }
        return INTSXP;
      }
      break;
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
      return CLOSXP;
  }
  return(obj_type);
}
/*
External interface for typeof, here mostly so we don't have to deal with the
SEXP return in the internal use case
*/

SEXP ALIKEC_typeof(SEXP object) {
  return mkString(type2char(ALIKEC_typeof_internal(object)));
}
