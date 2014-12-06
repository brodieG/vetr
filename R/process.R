#' Processes Check Expressions for \code{`\link{check_args}`}
#' 
#' These functions are not exported.  They operate by recursively evaluating
#' the expressions inside \code{`\link{c_and}`} and \code{`\link{c_or}`} objects
#' and storing the results in lists which are eventually returned to the top level.
#' 
#' A value of TRUE in a list means that particular check was met successfully.
#' A character value means that test was failed.  Depending on whether we're
#' dealing with a \code{`\link{c_and}`} or \code{`\link{c_or}`} check we
#' might end up with more than one value in the list, although if we do they
#' should all be character strings detailing all the things the object could have
#' been to pass the check.
#' 
#' @seealso \code{`\link{check_args}`}, \code{`\link{c_and}`}
#' @import alike
#' @aliases process.default process.c_and process.c_or
#' @param check the check object, typically a c_group object, a template object, or a value (e.g. TRUE)
#' @param frm the formal we are trying to match the check object against
#' @param check.calls the call that produced \code{`check`}
#' @param frm.name 1 length character, the name of the formal that is being checked
#' @return a list containing either TRUE, a 1 length character vector, or a mix of the
#'   two (including only TRUEs and only CHARACTER vectors).  If TRUE or only TRUEs,
#'   then the checks passed successfully.  If there is one character vector, then
#'   one of the tests that had to be passed failed.  If there are multiple character
#'   vectors and no TRUEs, then check failed all the tests that would have allowed
#'   validation to succeed.  If there are multiple 

process <- function(...) {
  UseMethod("process")
}
#' @method process default
#' @S3method process default

process.default <- function(check, frm, check.calls, frm.name) {
  if(frm.name %in% all.vars(check.calls, unique=TRUE)) {
    if(!identical(check, TRUE)) {  # Type 2 check
      return(list(paste0(deparse(check.calls), " != TRUE")))
    }
  } else {               # Type 1 check
    struct.check <- alike(check, frm)
    if(!isTRUE(struct.check)) return(list(struct.check))
  } 
  list(TRUE)
}
#' @method process c_and
#' @S3method process c_and

process.c_and <- function(check, frm, check.calls, frm.name) {  
  res <- list()
  for(i in seq_along(check)) {
    res <- c(res, process(check[[i]], frm , attr(check, "check.calls")[[i]], frm.name))
    if(res[[length(res)]] != TRUE) return(res)  # this will either be a single TRUE, or one character string, or many character strings
  }
  return(list(TRUE))
}
#' @method process c_or
#' @S3method process c_or

process.c_or <- function(check, frm, check.calls, frm.name) {
  
  res <- list()
  for(i in seq_along(check)) {
    res <- c(res, process(check[[i]], frm , attr(check, "check.calls")[[i]], frm.name))
    if(res[[length(res)]] == TRUE) return(list(TRUE))
  }
  return(res)
}