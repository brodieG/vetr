# Initial list of settings to track
#
# alike settings
# VALC_max_chars
# CSR_max_chars (reconcile with above)
# Recursion depth for environments
# Recursion depth for vetting expression substitution
# track_hash contents initial size (64)
# VALC_name_sub max symbol 15K
#
# Get rid of the frame, deprecate `.alike`



#' Generate Control Settings For Vetr and Alike
#'
#' Utility function to generate setting values.  We strongly recommend
#' that you generate the settings outside of function calls so that setting
#' generation does not become part of the \code{vet/vetr/alike} evaluation as
#' that could add noticeable overhead to the function evaluation.
#'
#' Settings after `fuzzy.int.max.len` are fairly low level and exposed mostly
#' for testing purposes.  You should generally not need to use them.
#'
#' Note that a successful evaluation of this function does not guarantee a
#' correct settings list.  Those checks are carried out internally by
#' \code{vet/vetr/alike}.
#'
#' @seealso \code{\link{type_alike}}, \code{\link{alike}}, \code{\link{vetr}}
#' @export
#' @param type.mode integer(1L) in 0:2 how strict to be about type comparison,
#'   see \code{\link{type_alike}}, with 0, the default, the most flexible.
#' @param type.mode integer(1L) in 0:2, see \code{mode} parameter to
#'   \code{\link{type_alike}}
#' @param attr.mode integer(1L) in 0:2 determines strictness of attribute
#'   comparison: \itemize{
#'     \item \code{0} only checks attributes that are present in target, and
#'       uses special comparisons for the special attributes (\code{class},
#'       \code{dim}, \code{dimnames}, \code{names}, \code{row.names},
#'       \code{levels}, \code{srcref}, and \code{tsp}) while requiring other
#'       attributes to be \code{alike}
#'     \item \code{1} is like \code{0}, except all atributes must be
#'       \code{alike}
#'     \item \code{2} requires all attributes to be present in \code{target} and
#'       \code{current} and to be alike
#'   }
#' @param lang.mode integer(1L) in 0:1 controls language matching, set to `1` to
#'   turn off use of \code{\link{match.call}}
#' @param fun.mode NOT IMPLEMENTED, controls how functions are compared
#' @param rec.mode integer(1L) `0` currently unused, intended to control how
#'   recursive structures (other than language objects) are compared
#' @param fuzzy.int.max.len max length of numeric vectors to consider for
#'   integer likeness (e.g. \code{c(1, 2)} can be considered "integer", even
#'   though it is numeric); currently we limit this check to vectors
#'   shorter than 100 to avoid a potentially expensive computation on large
#'   vectors, set to -1 to apply to all vectors
#' @param suppress.warnings logical(1L) suppress warnings if TRUE
#' @param width to use when deparsing expressions; default `-1`
#'   equivalent to \code{getOption("width")}
#' @param env.depth.max integer(1L) maximum number of nested environments to
#'   recurse through; these are tracked to make sure we do not get into an
#'   infinite recursion loop, but because they are tracked we keep a limit on
#'   how many we will go through, set to -1 to allow unlimited recursion depth.
#' @param symb.sub.depth.max integer(1L) maximum recursion depth when
#'   recursively substituting symbols in vetting expression
#' @param symb.size.max integer(1L) maximum number of characters that a symbol
#'   is allowed to have in vetting expressions.
#' @param track.hash.content.size integer(1L) used to set the initial size of
#'   the symbol tracking vector used with the hash table that detects recursive
#'   symbol substitution.  If the tracking vector fills up it will be grown by
#'   2x.
#' @param env what environment to use to match calls and evaluate vetting
#'   expressions, although typically you would specify this with the `env`
#'   argument to `vet`; if NULL will use the calling frame to
#'   \code{vet/vetr/alike}.
#' @return list with all the setting values

vetr_settings <- function(
  type.mode=0L, attr.mode=0L, lang.mode=0L, fun.mode=0L, rec.mode=0L,
  suppress.warnings=FALSE, fuzzy.int.max.len=100L,
  width=-1L, env.depth.max=65535L, symb.sub.depth.max=65535L,
  symb.size.max=15000L, nchar.max=65535L, track.hash.content.size=63L, 
  env=NULL
) {
  # we just use the function to match parameters
  as.list(environment())
}
