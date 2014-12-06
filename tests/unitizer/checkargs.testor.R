library(functools)

fun <- function(a, b, c=5L) {
  check_args(numeric(), is.integer(b), numeric(3L))
}
fun(3, 1:9, 1:3)                          # pass
fun(3, matrix(1:9, nrow=3), 1:3)          # pass
fun(3, matrix(1:10, nrow=2), 1:3)         # pass 
fun(3, matrix(runif(9), nrow=3))          # fail
fun(3, matrix(runif(9), nrow=3), 1:3)     # fail

fun2 <- function(a, b, c=5L) {
  check_args(numeric(), c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)) , numeric(3L))
}
fun2(3, matrix(runif(9), nrow=3), 1:3)             # fail
fun2(3, matrix(sample(1:9), nrow=3), 1:3)          # pass
fun2(3, matrix(letters[sample(1:9)], nrow=3), 1:3) # pass
fun2(3, letters[sample(1:9)], 1:3)                 # fail


fun3 <- function(a, b, c=5L) {
  check_args(
    a=numeric(), 
    b=c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)), 
    c=data.frame(a=character(), b=numeric(), c=factor(levels=c("moms", "pops")), stringsAsFactors=FALSE)
  )
}
fun3(3, matrix(sample(1:20, 9), nrow=3), 1:3)                                 # fail 
fun3(3, matrix(sample(1:20, 9), nrow=3), data.frame())                        # fail
fun3(3, matrix(sample(1:20, 9), nrow=3), data.frame(x=1:3, y=1:3, z=1:3))     # fail
fun3(3, matrix(sample(1:20, 9), nrow=3), data.frame(a=1:3, b=1:3, c=1:3))     # fail
fun3(3, matrix(1L), data.frame(a="a", b=1, c=factor("moms", levels=c("moms", "pops")), stringsAsFactors=FALSE))     # pass

fun4 <- function(a, b, c=1:10) {
  check_args(
    a=numeric(), 
    b=c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)), 
    c=c_and(matrix(integer(), nrow=3), integer(3L))
  )
  if(length(c) != 3L) stop("Lenght Error")
}
fun4(3, matrix(sample(1:20, 9), nrow=3), 1:11)          # fail
var <- runif(10)
fun4(var, matrix(sample(1:20, 9), nrow=3), c(5, 12, 3)) # pass
fun4(3, matrix(sample(1:20, 9), nrow=3))                # pass (? outcome not clear)



# # expr <- quote({
# #   a + 3
# #   sin(pi) * 8 + var / 3
# # })
# # str(flatten_expr(expr))


# # Dealing with a couple of problems.  Main one right now is that check_args() is
# # evaluating the arguments (so instead of getting is.numeric(b), we get TRUE,
# # but really the root issue is that the method that
# # match_call uses to pull arguments (using substitute in the function frame)
# # substitutes too much.  Need to narrow the substitute so that it only works on 
# # the ... argument.

