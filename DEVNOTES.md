## Notes from 8/21

So main thing to do is cleanly separate the "message" SEXP into the message
character values as part of the `ALIKEC_res_interim` object or some such, and
just the wrap object.

So we need to restructure the `ALIKEC_res` and `ALIKEC_res_sub` objects to
contain all the string structure.  Additionally, it seems like the interim
object really doesn't need the call object?  I guess it can be there explicitly
as a call object to distinguish it from the `wrap` object?  The only real use
case is that after we've wrapped the call and are ready to return it to `vetr`
we need to put it someplace.

So we need objects that:

* Track the strings
* Track the strings + recursion index + wrap lang
* Track the strings + wrap lang + attr type
* Track the strings + final call

Also need to track success status, df status, attr level

* `ALIKEC_res_strings`: just the strings
* `ALIKEC_res_min`: strings + success
* `ALIKEC_res_wrap`: strings + success + wrap + [rec index] + [df] + [attr]
* `ALIKEC_res`: strings + success + call

One thing left to do: figure out what `ALIKEC_wrap` should be returning.

## Notes from 8/20

Currently working through trying to delay the construction of error messages to
the very last minute.

Seems like the main issue is that that in VALC_evaluate, we need to change from
a simple LISTSXP to some homegrown object containing all the errors.  The major
issue we're going to have here is that we need to make sure that the protection
stack works out since as we've heading right now the VALC_res needs to be
protected explicitly since it contains non-SEXP crap in it.  Most likely we'll
need to change that to be a SEXP that contains the vector messages in component
pieces plus the ancillary stuff.

All this is to delay having to call `pad_or_quote` or `smprintf` business.  Have
to wonder if we truly optimized those, maybe we could just skip worrying about
these?  Seems like no matter what we'll have to create some CHARSXPS...

We'll need to figure out how to update `VALC_process_error` to deal with the new
format.

Looks like creating a 5 long character vector with new strings (i.e. requiring
new CHARSXPs) takes about 4 microseconds, which seems way too long.  So maybe an
alternative solution is to pass around a results object that contains the result
data as C objects, and one `LISTSXP` with all the SEXP objects.  This means that
every function that returns an `ALIKEC_res_fin` or `VALC_res` object needs to be
modified to accept the result object as an input.

Actually, not quite, we just need the `VALC_res` ones.  We can just harvest and
copy over the `ALIKEC_res_fin` separately.

There are two types of SEXPs we need to handle:

1. The call info from `alike` that has all the `names(x)[1]` stuff
2. The result of evaluating standard tokens

For the non-SEXP stuff, which is most likely to be the alike strings, do we want
to implement a pair list type structure, or a resizing array?  Probably pair
list.  For standard tokens seems like recording the value of the `eval_c`
variable might be sufficient.

## ALIKE structs

### `ALIKEC_res_interim`

#### Inputs

* `ALIKEC_res_strings_to_SEXP` doesnt use object slot
* `ALIKEC_res_interim_as_strings` doesn't use object slot

For the next two, while used, call could just as easily be passed separately

* `ALIKEC_strsxp_or_true` deparses object into call
* `ALIKEC_string_or_true` deparses object into call

#### Output

* `ALIKEC_alike_wrap` generates the final call object that can then be deparsed,
  so can be saved as a simple call
* `ALIKEC_type_alike_internal` doesn't even use the object
* `ALIKEC_fun_alike_internal` doesn't even use the object

### `ALIKEC_res`

#### Inputs

* `ALIKEC_wrap`

#### Output

* `ALIKEC_alike_obj`
* `ALIKEC_alike_rec`
* `ALIKEC_alike_internal`

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

^^ indeed.  In order to protect against this, we will use a hash table to track
re-appearance of a symbol, the problem is we only care about reappearance of a
symbol if it is a child to itself.  This means we have to rest the hash tables
after we dive into each branch of the parsed language, or duplicate them each
time we dive into a branch.

So we need a structure that points to:

* Our hash table
* A growable character vector containing all encountered names
* The current index of the character vector
* The current size of the character vector

We also want to extend this to environment tracking as that currently may not be
done correctly?  Though with environments maybe less of an issue as there isn't
any and/or logic to contend with, so if an environment matched in one branch it
must be definition match in the next.

One potential challenge on relying on environment memory locations is if
somehow R copies and moves an environment in the middle of our operation.  That
seems somewhat unlikely, but should probably be documented someplace.

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

`eval` in C will cause a promise to be evaluated, even though `findVar` will keep returning a promise and `PRSEEN` will still return 0.  We tested this by accessing a slow evaluating argument more than once.

## Optimization

### `all_in`

Rather than try to do a full on optimization right now, try a simple
implementation that is faster than the simple `all(x %in% y)`:

```
source('tests/unitizer/_pre/lorem.R')
lorem.words <- unlist(strsplit(lorem, "\\W"))
lorem.unique <- unique(lorem.words)
x <- sample(lorem.unique, 1e4, rep=TRUE)

library(microbenchmark)
microbenchmark(times=10,
  all(unique(x) %in% lorem.unique),
  all(x %in% lorem.unique),
  !anyNA(match(unique(x), lorem.unique))
)
## Unit: microseconds
##                                    expr     min      lq     mean   median
##        all(unique(x) %in% lorem.unique) 173.327 175.111 221.3290 216.5445
##                all(x %in% lorem.unique) 223.698 225.259 228.5640 225.8155
##  !anyNA(match(unique(x), lorem.unique)) 188.009 189.564 231.6546 221.5680
##       uq     max neval
##  264.897 277.004    10
##  232.412 240.201    10
##  258.627 329.226    10
```

Short of it is that it doesn't seem worth it.

### Vs. `checkmate`

```
xx <- runif(1e4)
xx[1] <- 1
xx[2] <- 0
microbenchmark(
  assertNumeric(xx, any.missing = FALSE, lower = 0, upper = 1),
  vet(numeric() && all_bw(., 0, 1), xx),
  assertNumeric(xx, any.missing = FALSE, lower = 0),
  vet(numeric() && all_bw(., 0, Inf), xx),
  vet(numeric() && all(. > 0), xx)
)
microbenchmark(
  all_bw(xx, 0, 1),
  all(xx >= 0 & xx <= 1)
)
## Unit: microseconds
##                                                          expr    min       lq
##  assertNumeric(xx, any.missing = FALSE, lower = 0, upper = 1) 26.627  29.9290
##                         vet(numeric() && all_bw(., 0, 1), xx) 20.198  22.6825
##             assertNumeric(xx, any.missing = FALSE, lower = 0) 19.055  20.7130
##                       vet(numeric() && all_bw(., 0, Inf), xx) 14.885  16.8890
##                              vet(numeric() && all(. > 0), xx) 73.076 119.4560
##       mean  median       uq     max neval
##   42.24239  36.187  55.2480  88.824   100
##   30.46707  25.055  40.4240  86.059   100
##   27.72199  23.029  36.6965  54.854   100
##   23.69168  18.597  30.5225  54.603   100
##  149.69694 133.635 169.2880 301.521   100

ss <- do.call(paste0, expand.grid(letters, letters, letters))
microbenchmark(
  all_bw(ss, "a", "zzz"),
  all(xx >= "a" & xx <= "zzz")
)
## Unit: microseconds
##                          expr       min        lq       mean    median
##        all_bw(ss, "a", "zzz")   336.112   410.057   463.3804   458.985
##  all(xx >= "a" & xx <= "zzz") 23731.092 24235.388 26356.3079 24951.518
##          uq       max neval
##    502.3695   904.064   100
##  27598.0535 34451.201   100
```
Not entirely sure what's going on here, shouldn't be that much faster.

### Loop unswitching

Trusting that gcc does comparable stuff?

```
xx <- runif(1e4)
microbenchmark(
  all_bw(xx, 0, 1),
  all_bw2(xx, 0, 1),
  all(xx >= 0 & xx <= 1)
)
## Unit: microseconds
##                    expr    min      lq      mean   median       uq      max
##        all_bw(xx, 0, 1) 23.117 23.3210  24.65532  23.6515  24.1765   98.217
##       all_bw2(xx, 0, 1) 13.665 13.8910  14.62394  14.2865  14.5635   37.207
##  all(xx >= 0 & xx <= 1) 82.325 92.8285 139.04823 141.6830 149.6355 1164.560
```
Switching to 03
```
> microbenchmark(
+   all_bw(xx, 0, 1),
+   all_bw2(xx, 0, 1),
+   all(xx >= 0 & xx <= 1)
+ )
Unit: microseconds
                   expr    min     lq      mean  median       uq      max neval
       all_bw(xx, 0, 1) 13.659 14.002  15.74092  14.434  14.9120   47.561   100
      all_bw2(xx, 0, 1) 18.759 19.114  21.21446  19.823  20.5100   55.189   100
 all(xx >= 0 & xx <= 1) 82.436 91.505 145.77960 116.293 146.3375 2128.326   100
```
Tried seeing if there were some ways to force more loop unswitching as compiler
really should be able to do it, but didn't find obvious setting which suggests
higher probability that different compilers may work differently as well.

Compare to integer:
```
xx <- runif(1e4)
yy <- sample(seq.int(1e4))
microbenchmark(
  all_bw(xx, 0, 1),
  all_bw(yy, 0, 1e4),
  all_bw(yy, 0, 9e3),  # fail
  all(xx >= 0 & xx <= 1)
)
## Unit: microseconds
##                    expr    min      lq      mean   median       uq     max
##        all_bw(xx, 0, 1) 12.609 13.2120  13.54127  13.3295  13.6290  23.172
##    all_bw(yy, 0, 10000)  8.209  8.6555   8.94603   8.7505   9.1845  11.299
##     all_bw(yy, 0, 9000)  4.067  4.3385   5.44750   4.6980   5.2350  55.264
##  all(xx >= 0 & xx <= 1) 82.532 89.7550 127.70684 126.4315 148.0600 905.974
```

### New Notes (7/14/17)

It seems overall `.Call` has gotten a bit faster, although perhaps this is the
new machine being faster:
```
> microbenchmark(test1(1), test2(1,2), test3(1,2,3))
Unit: nanoseconds
           expr min    lq   mean median  uq   max neval
       test1(1) 488 506.0 953.77  517.5 585 40254   100
    test2(1, 2) 592 632.0 724.22  664.5 745  3332   100
 test3(1, 2, 3) 699 748.5 853.18  809.0 881  2741   100
```
These all do trivial work, trying to measure overhead from parameters.  Looks
like 150ns per parameter.  Probably not enough to worry about too much.

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
  validate_args(
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

### vs alternatives

```
library(validate)
library(ensurer)
mean2 <- function(x, ...) {validate_args(numeric()); mean(x, ...)}
mean3 <- function(x, ...) {ensure_that(x, is.numeric(.)); mean(x, ...)}r
mean4 <- function(x, ...) {stopifnot(is.numeric(x)); mean(x, ...)}

mean2a <- function(x, ...) {validate_args(INT.1); mean(x, ...)}
mean4a <- function(x, ...) {
  stopifnot(
  is.integer(x) || (is.numeric(x) && all(x == round(x))),
  length(x) == 1L, !is.na(x));
  mean(x, ...)
}
library(microbenchmark)
microbenchmark(
  mean(1:1e4), mean2(1:1e4), mean3(1:1e4), mean4(1:1e4), mean2a(1L),
  mean4a(1L)
)
# Unit: microseconds
#            expr     min       lq      mean   median       uq     max neval
#   mean(1:10000)  18.081  19.0440  21.45990  20.3400  21.2840  48.570   100
#  mean2(1:10000)  31.804  34.9475  40.90450  36.6980  39.2780 121.493   100
#  mean3(1:10000) 129.214 134.4045 172.52273 139.9375 158.0205 938.026   100
#  mean4(1:10000)  26.739  29.2570  35.75861  30.4975  32.7875 158.021   100`
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

### Providing Access to Templates

It would be super helpful to actually be able to see the template, and possibly
even get a diff against it.  E.g., in:

```
mdlc <- lm(Sepal.Width ~ Petal.Length + Sepal.Length, iris)
mdlc.a <- abstract(mdlc)
mdld <- lm(Sepal.Width ~ Petal.Length -1, iris)
validate(mdlc.a, mdld)

# Error in validate(mdlc.a, mdlc) :
#   `(mdld$terms)[[3]][[1]]` should be a call to `+` (is a call to `-`)

(mdld$terms)[[3]]

# Petal.Length - 1

```

It would be great to have easy access to `mdlc.a$terms[[3]]`

## OR error messages

One of the more challenging ones are OR error messages, where our current naive
display of the multiple possibilities is repetitive an unclear.  Here is an
improvement:

```
Argument `x` does not meet any of the allowable forms:
- `names(letters[1:3])` is "NULL", but should be "character"
- `letters[1:3]` is "character", but should be "matrix" or "list"
```

This one is pretty good, but boy is it going to be a pain to figure out
all the elements that could be collapsed into it...  Or is it?  can we
just sort and then remove the common pieces, and append with ", or"?
More importantly, is it worth the trouble for a fairly rare use case?
Need to make sure we match up to the `is`, otherwise we'll get
nonsensical errors.  Blergh

Additional complication: what about multi-line expressions? Or
expressions that are not template based (e.g. "is not TRUE (FALSE)")

Part of the problem is that we have errors that are generated by `alike`, and
errors that are generated by validate for the non-alike methods, and we're
trying to use string output from those these processes in a unified logical way.
It may make more sense to split up the errors into lists with the following
components:

* A deparsed expression of the problem element, as a `character(1L)`
* A `character(1L)` of what it should be, without the 'should be', or more
  specifically without the `should`, as we need to contend with 'should be' and
  'should contain', etc.
* A `character(1L)` of what it is

Then when we get a list of these, we identify which elements are equal in the
first and last elements, and collapse the "should bes" into one comma/or
delimited string.

Implementation may be more straightforward than anticipated.  The only thing we
seem to need to do for `alike` is to break up the `res.message` business into
`res.target` and `res.actual`.  Some special treatment needed for the language
results since those have the annoying chr->SEXP->chr translation business going
on via ALIKEC_lang_alike_internal() (actually, that's probably not going away
any time soon as there are additional complexities therein).

### Random output

`all_bw(zzz, 0, 1)` is not TRUE (is "character" instead of a "logical")

`all_bw(zzz, 0, 1)` is not TRUE (string: "contains values outside of  only values in range `[0,1]` (2.783366 at index 3)")

    Error in fun2a(x = letters[1:3]) :
      For argument `x` at least one of these should pass:
      - `names(letters[1:3])` should be type "character" (is "NULL")
      - `letters[1:3]` should be matrix (is character)
      - `letters[1:3]` should be type "list" (is "character")

    Error in fun2a(x = letters[1:3]) :
      Argument `x` does not validate; at least one of the following should be
      true:

      In order to pass validation, argument `x` should meet one or more of the
      following requirements, but it meets none:

      - `names(letters[1:3])` should be type "character" (is "NULL")
      - `letters[1:3]` should be matrix (is character)
      - `letters[1:3]` should be type "list" (is "character")

      Argument `x` fails validation because it should meet one or more of the
      following requirements, but it meets none:

      Argument `x` is invalid as no allowable conditions are met:

      Argument `x` does not meet any of the allowable forms:

      Argument `x` does not meet any of the allowable validators:

      Argument `x` does not meet any of the permitted validators:

      Argument `x` fails all permitted validators:

      Argument `x` fails every permitted vetting expression:

      - `names(letters[1:3])` could be type "character" (is "NULL")
      - `letters[1:3]` could be matrix (is character)
      - `letters[1:3]` could be type "list" (is "character")


      Argument `x` fails because none of the following are true:

      For argument `x` at least one of these should pass:

      - `names(letters[1:3])` should be type "character" (is "NULL")
      - `letters[1:3]` should be matrix (is character)
      - `letters[1:3]` should be type "list" (is "character")

    # Problem with this one is it doesn't tell you what it should be:

    Error in fun2a(x = letters[1:3]) :
      For argument `x` at least one of the following should be true but none are:
      - `typeof(names(letters[1:3])) == "character"`
      - `class(letters[1:3]) == "matrix"`
      - `typeof(letters[1:3]) == "list"`

    #

    Error in fun2a(x = letters[1:3]) :
      Argument `x` must meet at least one of the following to validate:
      - `typeof(names(letters[1:3]))` is NULL (expected "character")
      - `class(letters[1:3])` is character (expected "matrix")
      - `typeof(letters[1:3])` is character (expected "list")

    # Below is best, but doesn't work when we're subsetting / using accesor funs

    Error in fun2a(x = letters[1:3]) :
      Argument `x` validation failure: `letters[1:3]` is character, but
      should be matrix, or type list

    Validation error in fun2a(x = letters[1:3]) :
      Argument `x`: `letters[1:3]` is character, but should be matrix or
      list

      Argument `x`:
        letters[1:3]
      is character, but should be matrix or type list



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

