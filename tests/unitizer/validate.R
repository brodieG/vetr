library(validate)
fun <- function(x, y, z) {
  validate(x=matrix(ncol=3), y=integer(2L), z=logical(1L))
}
fun(1, 2, 3)
