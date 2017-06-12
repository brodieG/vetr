## Internal Funs
##
## R interface for an internal C functions used by \code{alike}.  Provided
## primarily for unit testing purposes
##
## @aliases name_compare class_compare dimname_compare dim_compare ts_compare
##   lang_alike fun_alike dep_alike match_call_alike env_track
## @keywords internal
## @param int_mode

attr_compare <- function(target, current, attr.mode=0L)
  .Call(VALC_compare_attributes, target, current, attr.mode)

name_compare <- function(target, current)
  .Call(VALC_compare_names, target, current)

class_compare <- function(target, current, rev=0)  # `rev` is unused; here for legacy
  .Call(VALC_compare_class, target, current)

dimname_compare <- function(target, current)
  .Call(VALC_compare_dimnames, target, current)

dim_compare <- function(
  target, current, tar_obj=integer(), cur_obj=integer(), rev=0L
)
  .Call(VALC_compare_dims, target, current, tar_obj, cur_obj, rev);

ts_compare <- function(target, current)
  .Call(VALC_compare_ts, target, current)

lang_alike <- function(target, current, match.call.env=parent.frame())
  .Call(VALC_lang_alike, target, current, match.call.env)

lang_alike_chr <- function(target, current, match.call.env=parent.frame())
  .Call(VALC_lang_alike_chr, target, current, match.call.env)

fun_alike <- function(target, current)
  .Call(VALC_fun_alike, target, current)

dep_alike <- function(obj, width.cutoff=60L)
  .Call(VALC_deparse, obj, width.cutoff)

dep_oneline <- function(obj, max.chars=20L, keep.at.end=0L)
  .Call(VALC_deparse_oneline, obj, max.chars, keep.at.end)

pad <- function(obj, lines=-1, pad=-1)
  .Call(VALC_pad, obj, lines, pad)

pad_or_quote <- function(obj, width=-1L, syntactic=-1L)
  .Call(VALC_pad_or_quote, obj, width, syntactic)

match_call_alike <- function(call, env)
  .Call(VALC_match_call, call, quote(match.call(NULL, quote(NULL))), env)

env_track <- function(env, size_init = 32, env_limit=65536L)
  .Call(VALC_env_track, env, size_init, env_limit)

is_valid_name <- function(name)
  .Call(VALC_is_valid_name_ext, name)

is_dfish <- function(obj)
  .Call(VALC_is_dfish, obj)

alike_mode <- function(obj)
  .Call(VALC_mode, obj)

#' Used for testing C code
#'
#' @keywords internal
#' @export

alike_test <- function(obj) .Call(VALC_test, substitute(obj)) # nocov

# alike_test2 <- function(target, current)
#   .Call(VALC_test, target, current, sys.frame(sys.nframe()))

syntactic_names <- function(lang) .Call(VALC_syntactic_names, lang)

msg_sort <- function(messages)
  .Call(VALC_msg_sort, messages)

msg_merge <- function(messages)
  .Call(VALC_msg_merge, messages)

msg_merge_ext <- function(messages)
  .Call(VALC_msg_merge_ext, messages)

find_fun <- function(fun.name, env)
  .Call(VALC_find_fun, fun.name, env)

hash_test <- function(values, keys) .Call(VALC_hash_test, values, keys);

track_hash <- function(keys, size) .Call(VALC_track_hash, keys, size);

#' Pre-calculated Precision Level
#'
#' Used to limit overhead of calls the require use of
#' \code{.Machine$double.eps ^ 0.5}

MachDblEpsSqrt <- .Machine$double.eps ^ 0.5
