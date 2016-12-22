## NSE?

Is it possible to allow the functions to accept arbitrary language objects and
expressions?

For example, currently something like `validate(INT.1, x)` works fine because
`INT.1` is a language object, but `validate(INT.1 || LGL.1)` fails because R
will try to evaluate the expression which obviously doesn't work.

Proposal:

1. Substitute first
2. Parse
3. Evaluate language components
4. Continue parsing?

Seems like this should work just fine so long as we substitute target first.

Need to watch out for infinite loops with stuff that keeps evaluating to
language? (e.g. `y <- quote(get("y")); validate(y, x);`)

## Parse Rules

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

UPDATE: now any expression that contains a dot is evaluted as is

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

## C Notes

### Tracebacks

Tracebacks not set in unitizer when eval(stop()) from C, even though they are set outside of unitizer, and when calling `error` in C within unitizer.

### PROTECT stack and `error`

It seems like there are no stack imbalance problems when the script finishes with errors; is this okay?

### Promise Evaluation

`eval` in C will cause a promise to be evaluated, even though eventhough `findVar` will keep returning a promise and `PRSEEN` will still return 0.  We tested this by accessing a slow evaluating argument more than once.

## Optimization

### `.Call` vs. `.External`:

```
> microbenchmark(valtest1(1, 2, 3), valtest2(1, 2, 3))
Unit: nanoseconds
              expr  min     lq median     uq    max neval
 valtest1(1, 2, 3)  856 1092.5 1353.5 1523.5  18416   100
 valtest2(1, 2, 3) 4316 4601.5 4906.0 5249.5 106648   100

```
but this was using `.External("funname", ...)` as the variable version does not seem to work:
```
Error in .External(VALC_test2, ...) : NULL value passed as symbol address
```
Actually, if we change both character fun name:
```
> microbenchmark(valtest1(1, 2, 3), valtest2(1, 2, 3))
Unit: microseconds
              expr   min     lq median     uq    max neval
 valtest1(1, 2, 3) 4.655 4.7890  4.957 5.1860 57.944   100
 valtest2(1, 2, 3) 4.296 4.5355  4.660 4.8635 19.669   100
```
so moral is we need to figure out how to get `.External` to work with the object?

### Recursion vs Loop

Doesn't really seem to make a difference:
```
SEXP VALC_test1(SEXP a) {
  if(TYPEOF(a) == VECSXP) {
    return(VALC_test1(VECTOR_ELT(a, 0)));
  }
  return(a);
}
SEXP VALC_test2(SEXP a) {
  while(TYPEOF(a) == VECSXP) {
    a = VECTOR_ELT(a, 0);
  }
  return(a);
}
> microbenchmark(valtest1(a), valtest2(a))
Unit: microseconds
        expr   min    lq median     uq   max neval
 valtest1(a) 1.001 1.022 1.0595 1.1535 9.619   100
 valtest2(a) 1.004 1.022 1.0800 1.1575 2.853   100
```
This is on an about 20-30 deep list

### Complex Return Values

We're using SEXPs for complex return values, which often means we will create a SEXP from a C object, and then convert back to C on the other side.  Don't know how much overhead stuff like `ScalarInteger()` has, but probably much better to just make structs and use those minimizing the amount of SEXPs used.

### install

Costs about 40ns per, based on simple test.

### `match_call` use from within C code

With two `match_call` and extracting fun:
```
Unit: microseconds
         expr   min     lq median    uq    max neval
 fun(1, 2, 3) 6.591 7.1275   7.35 7.638 59.792   500
```
With one `match_call` and extracting the fun name:
```
> microbenchmark(fun(1, 2, 3), times=500)
Unit: microseconds
         expr  min     lq median    uq    max neval
 fun(1, 2, 3) 4.72 5.0715   5.23 5.418 55.835   500
```
With just the one `match.call`:
```
> microbenchmark(fun(1, 2, 3), times=500)
Unit: microseconds
         expr   min    lq median     uq    max neval
 fun(1, 2, 3) 4.722 5.027  5.176 5.3605 38.562   500
```
So about 1.9us for using `match.call` to do the matching.  If we tried to match
on tag name only could be faster; something to think about.

### Initial benchmark

Using version 0.0.1.9000 and the following functions:
```
fun1 <- function(x, y, z)
  validate(
    x=matrix(integer(), ncol=3) || integer(3L),
    y=integer(2L) || NULL || logical(1L),
    z=logical(1L)
  )
fun1b <- function(x, y, z) {
  stopifnot(
    (
      is.matrix(x) && ncol(x) == 3 && (
        is.integer(x) || (is.numeric(x) && all(floor(x) == x))
      )
    ) ||
    ((is.integer(x) || (is.numeric(x) && all(floor(x) == x))) && length(x) == 3)
  )
  stopifnot(
    ((is.integer(y) || (is.numeric(y) && all(floor(y) == y))) && length(y) == 2) ||
    is.null(y) || (is.logical(y) && length(y) == 1L)
  )
  stopifnot(is.logical(z) && length(z) == 1L)
}
```
we get comparable performance:
```
> microbenchmark(fun1(1:3, FALSE, FALSE), fun1b(1:3, FALSE, FALSE))
Unit: microseconds
                     expr    min     lq median      uq    max neval
  fun1(1:3, FALSE, FALSE) 23.165 24.257 25.050 26.0010 60.102   100
 fun1b(1:3, FALSE, FALSE) 19.199 20.684 22.361 23.4795 38.313   100
```

### LISTSXP vs VECSXP

Linked lists seem to be slightly faster, at least for small lists:

```
SEXP VALC_test(SEXP a, SEXP b) {
  SEXP c;
  // c = PROTECT(allocVector(VECSXP, 3));
  // SET_VECTOR_ELT(c, 0, R_NilValue);
  // SET_VECTOR_ELT(c, 1, R_NilValue);
  // SET_VECTOR_ELT(c, 2, R_NilValue);
  // VECTOR_ELT(c, 0);
  // VECTOR_ELT(c, 1);
  // VECTOR_ELT(c, 2);
  // UNPROTECT(1);
  c = PROTECT(list3(R_NilValue, R_NilValue, R_NilValue));
  CAR(c);
  CADR(c);
  CADDR(c);
  UNPROTECT(1);
  return(R_NilValue);
}
```

VECSXP

```
> microbenchmark(valtest(1, 2))
Unit: nanoseconds
          expr min    lq median   uq   max neval
 valtest(1, 2) 703 734.5    858 1068 35383   100
 ```

LISTSXP

```
Unit: nanoseconds
          expr min  lq median     uq   max neval
 valtest(1, 2) 681 728    837 1024.5 10264   100
```

## Usability

### `validate` return values

Unlike `validate_args`, we should have the option not to stop.  Options:

* Return message as is, but that is not super helpful due to the "Argument `current` stuff
* Return a variation on above, but more anonymous (start with "should..")
* Return a character vector with the values it could be
    * prepend "should .." or not?
* throw an error

Default should be reasonably useful error message as character value, perhaps starting with "should", options are stop, just the message stop uses, default, just the raw values.

One issue with the default mode is that we end up with stuff like:
```
should meet at least one of the following:
  - be length 1 (is 2)
  - have `current > 4` evaluate to all TRUE values (contains non-TRUE values)
```
The problems is the `current` in the scond line.  What would be better?  Just use the `.` syntax?  Not sure.

### Random output

    Error in validate(INT.1 || NULL, 1:3) :
      Argument `current` must meet at least one of the following, but does not:
      - Expected length 1, but got 3
      - Expected type "NULL", but got "integer"


    Error in validate(INT.1 || NULL, 1:3) :
      At least one of the following should be true:
      - `length(1:3)` should be 1, but is 3
      - `1:3` should be type NULL, but is integer

    Error in fun2(x = 1:3, y = TRUE) :
      Argument `x` fails validation because none of the following are true:
      - `length(1:3)` should be 1, but is 3
      - `1:3` should be type NULL, but is integer

    Error in fun2(x = 1:3, y = TRUE) :

    Input error in fun2(x = 1:3, y = TRUE) :

      For argument `x` at least one of the following must be TRUE:
      For argument `x` at least one of these  must be TRUE:

      At least one of the following should be TRUE for argument `x`, but none
      are:

      Bad argument `x`; does not pass any of the following:

      - `length(1:3)` should be 1, not 3
      - `1:3` should be type NULL, not integer
      - `all(-1:1 > 0)` should evaluate to TRUE (is FALSE)

      - Expression:
          all(-1:1 > 0)
        should evaluate to TRUE (is FALSE)

      

    Error in fun2(x = matrix(c(1:9), nrow = 3), y = -1:1, z = NULL) :

      Argument have `all(-1:1 > 0)` evaluate to TRUE (is FALSE)

      For argument `y`, `all(-1:1 > 0)` should evaluate to TRUE (is FALSE)


   Error in fun2(x = 1:3, y = TRUE) :
      Argument `y` wrong because `TRUE` should be type integer, not logical

    Error in validate(INT.1 || NULL, 1:3) :
      Argument `current` must:
      - have length 1 (is 3), OR
      - have type "NULL" (is "integer"), OR
      - have class "matrix" (is "integer"), OR
      - have `!is.na(current)` all TRUE (is FALSE, contains FALSE, contains NA)
      - inherit from class "foo"

    Error in validate(INT.1 || NULL, 1:3) :
      Argument `current` must have:
      - length 1 (is 3), OR
      - type "NULL" (is "integer"), OR
      - class "matrix" (is "integer"), OR
      - `!is.na(current)` all TRUE (is FALSE, contains FALSE, contains NA)
      - inheritance from class "foo"

      Argument `current` should:
      - be length 1 (is 3), OR
      - be type "NULL" (is "integer"), OR
      - be class "matrix" (is "integer"), OR
      - have `!is.na(current)` all TRUE (is FALSE, contains FALSE, contains NA)
      - inherit from class "foo"

      Argument `current` should:
      - be length 1 (is 3), OR
      - be type "NULL" (is "integer"), OR
      - be class "matrix" (is "integer"), OR
      - have `!is.na(current)` all TRUE (is FALSE, contains FALSE, contains NA)
      - inherit from class "foo"

    "Expected at index [[2]][[2]][[2]][[2]]: length 3 (is 1)"


    be length 3 (is 1) at index [[2]][[2]][[2]][[2]]

    alike(integer(1L), 1:3)
    should be length 1 (is 3)

    should be length 1 (is 3)
    should be length 1 to be alike (is 3)
    should be length 1 (is 3) to be alike

    Expected length 1, but got 3
    length 1 (is 3)

    Expected length 1 (is 3)

    must have length 1 (is 3)


    # Nope, row.names won't match
    > alike(df.tpl, df.cur2)
    Test Failed Because:
    Value mismatch: 1 string mismatch

    +++ .new:
    [1] "should be \"one\" at index [[1]] for row.names (is \"uno\")"
    --- .ref
    [1] "Expected \"one\" at index [[1]] for row.names, but got \"uno\""

    unitizer>

    # Nope, row.names won't match
    > alike(df.tpl, df.cur2)
    Test Failed Because:
    Value mismatch: 1 string mismatch

    +++ .new: [1] "should be \"one\" at index [[1]] for row.names (is \"uno\")"
    --- .ref: [1] "Expected \"one\" at index [[1]] for row.names, but got \"uno\""

    unitizer>

    # Nope, row.names won't match
    > alike(df.tpl, df.cur2)
    Test Failed Because:
    Value mismatch: 1 string mismatch
    + [1] "should be \"one\" at index [[1]] for row.names (is \"uno\")"
    - [1] "Expected \"one\" at index [[1]] for row.names, but got \"uno\""
    unitizer>

Argument `x` in `x > 0` should evaluate to all TRUE
Argument `x` should
  lead `x > 0` to contain only TRUE

Argument `x` should
  cause `x > 0` to evaluate to all TRUE
  have length 3 (is 2)


Argument `x` in `x > 0` should evaluate to all TRUE

