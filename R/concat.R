# #' Aggregates Check Expressions for \code{`\link{check_args}`}
# #'
# #' @aliases c_and c_or
# #' @export
# #' @param `...` check expressions
# #' @seealso \code{`\link{check_args}`}:

# c_and <- function(...){
#   structure(
#     list(...),
#     check.calls=as.list(match_call(dots="expand"))[-1],
#     class=c("c_and", "c_group")
#   )
# }
# #' @export

# c_or <- function(...){
#   structure(
#     list(...),
#     check.calls=as.list(match_call(dots="expand"))[-1],
#     class=c("c_or", "c_group")
#   )
# }
