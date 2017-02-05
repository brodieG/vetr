# validate

## Overview

`validate` prevents "Garbage In" by ensuring your function arguments are exactly what you expect.  It was originally designed to add some rigor to functions using S3 objects, but it also simplifies otherwise tedious checking.

Checking is done by comparing user input to "template" objects:
```
my_fun <- function(x, y) {
  validate(
    x=matrix(character(), ncol=3),   # Argument x must be a 3 column character matrix
    y=logical(1L)                    # Argument y must be a 1 length logical
  )
  # ... do stuff
}
```
You can also mix standard checks with template checks by using `.(`:
```
my_fun <- function(x, y) {
  validate(
    x=matrix(character(), ncol=3),   # Argument x must be a 3 column character matrix
    y=logical(1L) && .(!is.na(.))    # Argument y must be a 1 length logical and not NA
  )
  # ... do stuff
}
If the contents of `.(` evaluate to anything other than `TRUE` the test fails.  Note that any `.` inside `.(` are substituted for the corresponding argument name prior to evaluation.

## Template Comparisons

Template comparisions are carried out with `alike` (see install_github("brodieg/alike") for more details).

## Roadmap

This package is very much in early alpha stages and will likely see interface changes in the near future.  In particular, `validate` uses non standard evaluation and is only currently tested with validation expressions typed directly into the function.  We will work on providing standard evaluation alternatives.

Additionally, the error messages are somewhat cryptic, though technically correct.  We will look to improve that too.

Finally (at least near term), we will look to implement more complex templates along the lines of `intNoNA()` or some such.

## A Note On Overhead

Validation adds overhead, but some overhead is almost always preferable to allowing garbage to seep into your program and produce garbage out.  That said, we have sought to minimize overhead by implementing `validate` in C.  For example, testing three arguments with modest templates takes about 20 microseconds on our system.

## Installation

`validate` is currently only available on github:

```
library(devtools)
install_github("brodieg/validate")
```

## Similar Packages

* [ensurer](https://github.com/smbache/ensurer)
* [types](https://github.com/jimhester/types)
* [argufy](https://github.com/gaborcsardi/argufy)
