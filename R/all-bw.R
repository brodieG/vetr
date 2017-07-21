#' Verify Values in Vector are Between Two Others
#'
#' Equivalent to `all(x >= lo & x <= hi)`, but substantially faster for larger
#' vectors because it does not create `length(x)` intermediate logical vectors
#' and it stops at the first failure.  You can modify the comparison to be
#' strictly greater/less than via the `bounds` parameter.
#'
#' If `x` and `lo`/`hi` are different types, `lo`/`hi` will be coerced to the
#' type of `x`.  When `lo`/`hi` are numeric and `x` is integer, if `lo`/`hi`
#' values are outside of the integer range then that side will be treated as if
#' you had used `-Inf`/`Inf`.  `-Inf` and `Inf` mean `lo` and `hi` will be
#' unbounded for all data types.
#'
#' @export
#' @param x vector logical (treated as integer), integer, numeric, or character.
#' @param lo scalar vector of type coercible to the type of `x`, cannot be NA,
#'   use `-Inf` to indicate unbounded (default).
#' @param hi scalar vector of type coercible to the type of `x`, cannot be NA,
#'   use `Inf` to indicate unbounded (default), must be greater than or equal to
#'   `lo`.
#' @param na.rm TRUE or FALSE (default), whether NAs are considered to be in
#'   bounds, NAs are treated as being out of bounds unless you set this
#'   parameter to TRUE, or in the degenerate case where `lo` is `-Inf`, `hi` is
#'   `Inf`, and `bounds` is \dQuote{[]}.
#' @param bounds `character(1L)` for values between `lo` and `hi`:
#'   * \dQuote{[]} include `lo` and `hi`
#'   * \dQuote{()} exclude `lo` and `hi`
#'   * \dQuote{[)} include `lo`, exclude `hi`
#'   * \dQuote{(]} exclude `lo`, include `hi`
#' @return TRUE if all values in `x` conform to the specified bounds, a string
#'   describing the first position that fails otherwise
#' @examples
#' all_bw(runif(100), 0, 1)
#' all_bw(runif(100) * 2, 0, 1)
#' all_bw(NA, 0, 1)
#' all_bw(NA, 0, 1, na.rm=TRUE)
#'
#' vec <- c(runif(100, 0, 1e12), Inf, 0)
#' all_bw(vec, 0)      # All +ve numbers
#' all_bw(vec, hi=0)   # All -ve numbers
#' all_bw(vec, 0, bounds="(]") # All strictly +ve nums
#' all_bw(vec, 0, bounds="[)") # All finite +ve nums

all_bw <- function(x, lo=-Inf, hi=Inf, na.rm=FALSE, bounds="[]")
  .Call(VALC_all_bw, x, lo, hi, na.rm, bounds)


