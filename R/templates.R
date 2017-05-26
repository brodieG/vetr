
#' Data Frame Constructor
#'
#' A lighter weight constructor for data frames to use in
#' \code{\link{validate}}.  Arguments are not checked or modified at all;
#' constructing something that is a reasonable template for a real data frame is
#' solely the responsibility of the user.
#'
#' @note characters are not coerced to factors, etc.
#'
#' @param ... the objects to use as columns for the data frame template
#' @return a list classed as `data.frame`
#' @export
#' @examples
#' mk_df(a=character(), b=factor(levels=letters[1:6]))

mk_df <- function(...) structure(list(...), class="data.frame")
