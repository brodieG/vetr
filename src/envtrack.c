/*
Copyright (C) 2023 Brodie Gaslam

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
We need environment stack tracking that will not persist across .Call calls
*/

/*
Allocated and re-allocate our env stack tracking object

Return 0 for failure, 1 for normal success, 2 for success requiring
re-allocation, 3 for success requiring re-allocation and copying

We need stack size to be strictly greater than stack_ind as next time we write
we will write to stack_ind
*/

int ALIKEC_env_stack_alloc(
  struct ALIKEC_env_track * envs, int env_limit
) {
  int success = 1;
  int stack_size = envs->stack_size;
  if(stack_size <= envs->stack_ind) {
    int stack_size_old = stack_size;
    if(stack_size > INT_MAX - stack_size) {
      // nocov start
      error(
        "%s%s",
        "Internal Error: cannot increase env stack size past INT_MAX; ",
        "contact maintainer"
      );
      // nocov end
    }
    stack_size += stack_size ? stack_size : envs->stack_size_init;

    if(stack_size <= envs->stack_ind) {
      // nocov start
      error(
        "%s%s",
        "Internal Error: env stack size increase is insufficient; ",
        "contact maintainer"
      );
      // nocov end
    }
    if(stack_size > env_limit) return 0;

    SEXP * env_stack_tmp = (SEXP *) R_alloc(stack_size, sizeof(SEXP));
    envs->stack_size = stack_size;

    success = 2;
    if(envs->env_stack == 0) {
      envs->env_stack = env_stack_tmp;
    } else if(envs->stack_size > stack_size_old) {
      // Prev allocation happened, need to copy pointers
      for(int i = 0; i < stack_size_old; i++)
        env_stack_tmp[i] = envs->env_stack[i];
      // ideally would free env_stack before repointing...
      envs->env_stack = env_stack_tmp;
      success = 3;
    }
  }
  return success;
}
/*
Initialize our stack tracking object
*/
struct ALIKEC_env_track * ALIKEC_env_set_create(
  int stack_size_init, int env_limit
) {
  if(stack_size_init < 1) {
    // nocov start
    error(
      "Internal Error: `alike` env stack size init should be greater than zero"
    );
    // nocov end
  }
  struct ALIKEC_env_track * envs =
    (struct ALIKEC_env_track *) R_alloc(1, sizeof(struct ALIKEC_env_track));
  envs->stack_size = envs->stack_ind = 0;
  envs->env_stack = 0;
  envs->no_rec = 0;
  envs->stack_size_init = stack_size_init;
  int res = ALIKEC_env_stack_alloc(envs, env_limit);
  if(!res) error("Unable to allocate `alike` environment stack");
  return envs;
}

/*
Track what environments we've checked already

Not super efficient allocation here; we should really free previous allocation
instead of just leaving it hanging until .Call ends.

Also, this should really be a linked list so we don't have to re-copy all the
pointers every time we expand the list.

Really taking solace in the point that this part of the code should be rarely
activated.

Returns
  * > 1 if the environment has not been seen before (and adds it to stack),
    really it is the result of the allocation attempt
  * 0 if the environment is found
  * -1 if we are out of space in the env stack
*/

int ALIKEC_env_track(
  SEXP env, struct ALIKEC_env_track * envs, int env_limit
) {
  int alloc_res;
  if(!(alloc_res = ALIKEC_env_stack_alloc(envs, env_limit))) return -1;
  int env_found = 0;
  for(int i = 0; i < envs->stack_ind; i++) {
    if(env == envs->env_stack[i]) {
      env_found = 1;
      break;
    }
  }
  if(env_found) return 0;
  envs->env_stack[envs->stack_ind] = env;
  envs->stack_ind++;
  return alloc_res;
}
/*
External interface purely for testing whether our environment hashing
is working
*/

SEXP ALIKEC_env_track_test(SEXP env_list, SEXP stack_size_init, SEXP env_limit) {
  int stack_init_int = asInteger(stack_size_init);
  if(stack_init_int == NA_INTEGER || stack_init_int < 0) {
    // nocov start
    error("Internal Error: stack_size_init must be positive");
    // nocov end
  }
  if(TYPEOF(env_list) != VECSXP) {
    // nocov start
    error("Internal Error: expected a list for argument `env_list`");
    // nocov end
  }
  if(TYPEOF(env_limit) != INTSXP) {
    // nocov start
    error("Internal Error: expected an integer for argument `env_limit`");
    // nocov end
  }
  int env_limit_int = asInteger(env_limit);
  struct ALIKEC_env_track * envs =
    ALIKEC_env_set_create(stack_init_int, env_limit_int);

  R_xlen_t len = XLENGTH(env_list);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int * res_int = INTEGER(res);
  R_xlen_t i;

  for(i = 0; i < len; i++) {
    SEXP env = VECTOR_ELT(env_list, i);
    if(TYPEOF(env) != ENVSXP)
      error(
        "All contents of `env` %s at item %d\n",
        "should be environments; error ", i + 1
      );
    res_int[i] = ALIKEC_env_track(env, envs, env_limit_int);
  }
  UNPROTECT(1);
  return res;
}
