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
#' @param type.mode
#' @param attr.mode
#' @param lang.mode
#' @param fun.mode NOT IMPLEMENTED
#' @param rec.mode
#' j

vetr_settings <- function(
  type.mode=0L, attr.mode=0L, lang.mode=0L, fun.mode=0L ,rec.mode=0L, 
  env=parent.frame(), fuzzy.int.max.len=100L, suppress.warnings=FALSE,
  width=-1L, env.limit=65536L, nchar.limit=20000L, 
) {

}
