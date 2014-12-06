
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

So what data structure allows us to mark during recursive descent?  We really should study the standard libraries...

In the meantime, let's consider:
```
((a && b) || c || (d && e && f)) && g
```
Maybe the right thing to do is one round of recursive descent / replacement, and then a second round of evaluation under each format.  This mostly works except that we still need to know which tests get evaluated normally vs as templates.

Alternate: do the same recursive descent, returning a C "linked list"?  Or maybe easier just use an R linked list, and for each CAR use a two element "list" vector, containing:

1. A pointer to the call element (the original CAR)
2. How the element should be evaluated:
    * plain evaluated
    * as `alike`
    * not at all because it is a child element to one of the two above



