
```
fun <- function(argument1, argument2, argument3, argument.opt=FALSE) {
  validate(
    argument1=integer(1L) && .(!is.na(.)),
    argument2=scl.pos.int(5L),
    argument3=matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))) ||
      (logical(2L) && .(all(is.na(.))),
    argument.opt=logical(1L)
  )
}
```

Rules to break down calls:

* if `(`, dive in
* if `&&` or `||` and `length() == 2` then recurse
* if `.(`
    * mark for evaluation, but replace all `.` with original variable name, and remove dot from anything else that is more than one dot
* else evaluate as `alike(arg, expr)`
    * but how do we treat something like `matrix(integer(), nrow=.(.))`
    * main issue is with rule to add dots in order to escape dots

So we need to recurse through all elements hunting for any `.` and `.(` or dot only expressions, and:

* if a single dot
  * if a call, replace with identical
  * if not, replace with arg name
* if more than one dot, trim a dot

Two pass process then

1. Descend, capture items to `alike`, vs items to plain evaluate
2. Re-descend and at marked items, either plain evaluate or `alike`

So what data structure allows us to mark?

