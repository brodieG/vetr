#' Verify Values in Vector are Between Two Others
#'
#' Similar to \code{isTRUE(all(x >= lo & x <= hi))} with default settings,
#' except that it is substantially faster and returns a string describing the
#' first encountered violation rather than FALSE on failure.
#'
#' You can modify the comparison to be strictly greater/less than via the
#' `bounds` parameter, and the treatment of NAs with `na.rm`.  Note that NAs are
#' considered to be out of bounds by default.  While technically incorrect
#' since we cannot know whether an NA value is in or out of bounds, this
#' assumption is both conservative and convenient.  Zero length `x` will always
#' succeed.
#'
#' If `x` and `lo`/`hi` are different types, `lo`/`hi` will be coerced to the
#' type of `x`.  When `lo`/`hi` are numeric and `x` is integer, if `lo`/`hi`
#' values are outside of the integer range then that side will be treated as if
#' you had used `-Inf`/`Inf`.  `-Inf` and `Inf` mean `lo` and `hi` will be
#' unbounded for all data types.
#'
#' @export
#' @param x vector logical (treated as integer), integer, numeric, or character.
#'   Factors are treated as their underlying integer vectors.
#' @param lo scalar vector of type coercible to the type of `x`, cannot be NA,
#'   use `-Inf` to indicate unbounded (default).
#' @param hi scalar vector of type coercible to the type of `x`, cannot be NA,
#'   use `Inf` to indicate unbounded (default), must be greater than or equal to
#'   `lo`.
#' @param na.rm TRUE, or FALSE (default), whether NAs are considered to be
#'   in bounds.  Unlike with [all()], for `all_bw` `na.rm=FALSE` returns an
#'   error string if there are NAs instead of NA.  Arguably NA, but not NaN,
#'   should be considered to be in `[-Inf,Inf]`, but since `NA < Inf` is NA we
#'   treat them as always being out of bounds.
#' @param bounds `character(1L)` for values between `lo` and `hi`:
#'   * \dQuote{[]} include `lo` and `hi`
#'   * \dQuote{()} exclude `lo` and `hi`
#'   * \dQuote{(]} exclude `lo`, include `hi`
#'   * \dQuote{[)} include `lo`, exclude `hi`
#'
#' @return TRUE if all values in `x` conform to the specified bounds, a string
#'   describing the first position that fails otherwise
#' @examples
#' all_bw(runif(100), 0, 1)
#' all_bw(runif(100) * 2, 0, 1)
#' all_bw(NA, 0, 1)              # This is does not return NA
#' all_bw(NA, 0, 1, na.rm=TRUE)
#'
#' vec <- c(runif(100, 0, 1e12), Inf, 0)
#' all_bw(vec, 0)      # All +ve numbers
#' all_bw(vec, hi=0)   # All -ve numbers
#' all_bw(vec, 0, bounds="(]") # All strictly +ve nums
#' all_bw(vec, 0, bounds="[)") # All finite +ve nums

all_bw <- function(x, lo=-Inf, hi=Inf, na.rm=FALSE, bounds="[]")
  .Call(VALC_all_bw, x, lo, hi, na.rm, bounds)


