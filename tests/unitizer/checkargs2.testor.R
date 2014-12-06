fun3 <- function(a, b, c=5L) {
  check_args(
    a=numeric(), 
    b=c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)), 
    c=data.frame(a=character(), b=numeric(), c=factor(levels=c("moms", "pops")), stringsAsFactors=FALSE)
  )
}
try(fun3(3, matrix(sample(1:20, 9), nrow=3), 1:3))