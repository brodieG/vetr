#' Verify Values in Vector are Between Two Others
#'
#' Default mode includes the bounds, but that can be modified with the
#' `include.bounds` parameter.  In addition to changing bounds treatment,
#' `include.bounds` allows you to specify whether you want the values inside or
#' outside the range defined by `lo`-`hi`.  Keep in mind that betweeness of
#' character strings will be affected by system collation, etc.
#'
#' If `x` and `lo`/`hi` are different types, `lo`/`hi` will be coerced to the
#' type of `x`.  Coercions resulting in NA will trigger errors.
#'
#' @param x vector numeric, integer, or character
#' @param lo scalar vector of type coercible to the type of `x`, cannot be NA
#' @param hi scalar vector of type coercible to the type of `x`, cannot be NA
#' @param na.rm TRUE or FALSE (default), whether NAs are allowed, NAs are
#'   normally taken to never meet the `lo`-`hi` value requirements.
#' @param include.ends `character(1L)` in:
#'   * "[]" include `lo` and `hi`
#'   * "()" exclude `lo` and `hi`
#'   * "[)" include `lo`, exclude `hi`
#'   * "(]" exclude `lo`, include `hi`
#'   * "][" values outside of `lo`-`hi`, include `lo` and `hi`
#'   * ")(" values outside of `lo`-`hi`, exclude `lo` and `hi`
#'   * "](" values outside of `lo`-`hi`, include `lo`, exclude `hi`
#'   * ")[" values outside of `lo`-`hi`, exclude `lo`, include `hi`

all_bw <- function(x, lo, hi, na.rm=FALSE, include.ends="[]")
  .Call(VALC_all_bw, x, lo, hi, na.rm, include.ends)


