# redefine funs to give us flexibility if we change packages without having
# to export the internal functions

library(vetr)

unitizer_sect("Name like attributes", {
  vetr:::name_compare(c("", "hello"), c("abc", "hello"))
  vetr:::name_compare(c("ab", "hello"), c("abc", "hello"))
  vetr:::name_compare(c(NA_character_, "hello"), c("abc", "hello"))
  vetr:::name_compare(c("ab", "hello"), c(NA_character_, "hello"))
  vetr:::name_compare(c(NA_character_, "hello"), c(NA_character_, "hello"))

  # mess with values

  vetr:::name_compare(1:3, 3:1)
} )
unitizer_sect("S3 Classes", {
  class1 <- letters[1:5]
  class2 <- letters[3:5]
  class3 <- letters[c(4, 3, 5)]
  class4 <- character()
  class5 <- NULL
  class6 <- list("a", "b", "c")

  # Note third argument is left in for legacy reasons but doesn't actually do
  # anything

  vetr:::class_compare(class2, class1, 0);
  vetr:::class_compare(class1, class2, 0);
  vetr:::class_compare(class1, class1[1:3], 0);
  vetr:::class_compare(class3, class2, 0);
  vetr:::class_compare(class3, class1, 0);
  vetr:::class_compare(class5, class2, 0);  # this should never happen in reality, but use alike check since not char char comparison
  vetr:::class_compare(class2, class5, 0);  # idem
  vetr:::class_compare(class5, class5, 0);
  vetr:::class_compare(class6, class2, 0);
  vetr:::class_compare(class2, class6, 0);

  class7 <- c("a", "data.frame")

  vetr:::class_compare(class7, class1, 0)
  vetr:::class_compare(class1, class7, 0)
})
unitizer_sect("Dimnames", {
  dimn1 <- list(NULL, NULL, NULL)
  dimn2 <- list(a=letters[1:3], b=letters[4:6], c=letters[7:9])
  dimn3 <- list(letters[1:3], b=letters[4:6], c=letters[7:9])
  dimn4 <- list(letters[1:3], B=letters[4:6], C=letters[7:9])
  dimn5 <- list(a=LETTERS[1:3], b=letters[4:6], c=letters[7:9])
  dimn6 <- list(a="", b=letters[4:6], c=letters[7:9])
  dimn7 <- list()
  dimn8 <- list(a=LETTERS[1:3], b=letters[4:6], c=letters[7:9], d=letters[24:26])
  dimn9 <- list(a=1:3, b=letters[4:6], c=letters[7:9])
  dimn10 <- list(a=list("a", "b", "c"), b=letters[4:6], c=letters[7:9])
  dimn11 <- NULL
  dimn12 <- matrix(letters[1:9], nrow=3)
  dimn13 <- `attr<-`(dimn2, "bar", "yowza")
  dimn14 <- `attr<-`(dimn2, "bar", "yowz")
  dimn15 <- list(a=letters[1:3], b=letters[1:3])
  dimn16 <- list(a=letters[1:3], b=letters[1:3])
  attr(dimn15, "a") <- 1:2
  attr(dimn16, "a") <- 1:3
  dimn17 <- list(a=letters[1:3])
  dimn18 <- list(a=letters[1:2], b=letters[1:3])

  # baseline cases

  vetr:::dimname_compare(dimn3, dimn2)
  vetr:::dimname_compare(dimn2, dimn3)
  vetr:::dimname_compare(dimn3, dimn4)
  vetr:::dimname_compare(dimn2, dimn5)
  vetr:::dimname_compare(dimn6, dimn5)
  vetr:::dimname_compare(dimn5, dimn6)

  # "empty-ish" cases

  vetr:::dimname_compare(dimn2, dimn1)
  vetr:::dimname_compare(dimn1, dimn2)
  vetr:::dimname_compare(dimn11, dimn2)
  vetr:::dimname_compare(dimn11, dimn11)
  vetr:::dimname_compare(dimn7, dimn2)
  vetr:::dimname_compare(dimn2, dimn7)
  vetr:::dimname_compare(dimn7, dimn7)

  # Breaking cases

  vetr:::dimname_compare(dimn5, dimn8)
  vetr:::dimname_compare(dimn8, dimn5)
  vetr:::dimname_compare(dimn9, dimn2)
  vetr:::dimname_compare(dimn2, dimn9)
  vetr:::dimname_compare(dimn2, dimn12)
  vetr:::dimname_compare(dimn12, dimn12)

  # Attr on attr

  vetr:::dimname_compare(dimn2, dimn13)
  vetr:::dimname_compare(dimn13, dimn2)
  vetr:::dimname_compare(dimn13, dimn14)
  vetr:::dimname_compare(dimn14, dimn13)

  # dimanames with attributes other than names that are not alike

  vetr:::dimname_compare(dimn15, dimn16)

  # Mismatched lengths

  vetr:::dimname_compare(dimn17, dimn18)
})
unitizer_sect("Dims", {
  dim1 <- rep(2L, 2)
  dim2 <- rep(2L, 3)
  dim3 <- rep(2L, 4)
  dim4 <- c(1L, 1L)
  dim5 <- 2L
  dim6 <- c(1L, 2L, 3L)
  dim7 <- rep(0L, 2)
  dim8 <- c(0L, 0L, 2L)
  dim9 <- NULL
  dim10 <- letters[1:2]
  dim11 <- list(2L, 2L)

  vetr:::dim_compare(dim1, dim2)  # fail
  vetr:::dim_compare(dim2, dim3)  # fail
  vetr:::dim_compare(dim1, dim4)  # fail
  vetr:::dim_compare(dim2, dim6)  # fail
  vetr:::dim_compare(dim7, dim1)  # works
  vetr:::dim_compare(dim7, dim4)  # works
  vetr:::dim_compare(dim1, dim7)  # fail
  vetr:::dim_compare(dim7, dim2)  # works
  vetr:::dim_compare(dim8, dim2)  # works
  vetr:::dim_compare(dim8, dim6)  # fail
  vetr:::dim_compare(dim6, dim9)  # works

  # really a corner case that shouldn't come about

  vetr:::dim_compare(9L, NULL)

  # With non atomic objects

  vetr:::dim_compare(dim1, dim2, list())          # fail
  vetr:::dim_compare(dim1, dim2, cur_obj=list())  # fail
  vetr:::dim_compare(dim1, dim2, list(), list())  # fail

  vetr:::dim_compare(dim1, dim2, integer(), list())  # fail

  # Errors

  vetr:::dim_compare(dim9, dim6)  # fail
  vetr:::dim_compare(dim10, dim1) # fail
})
unitizer_sect("Time Series", {
  ts.1 <- attr(ts(runif(24), 1970, frequency=12), "ts")
  ts.2 <- attr(ts(runif(24), 1970, frequency=4), "ts")
  ts.3 <- ts.4 <- ts.1
  ts.3[[2L]] <- 0
  ts.4[[3L]] <- 0

  vetr:::ts_compare(ts.1, ts.2)
  vetr:::ts_compare(ts.3, ts.2)  # zero is wildcard
  vetr:::ts_compare(ts.4, ts.2)
  vetr:::ts_compare(ts.4, ts.1)  # zero is wildcard
  vetr:::ts_compare(ts.1, ts.4)  # but not in reverse

  # non-ts comparisons

  vetr:::ts_compare(ts.4, "hello")
  vetr:::ts_compare("hello", 1:3)
  vetr:::ts_compare(ts.1, 1:3)   # TRUE, because second param is not REAL, kicks off to standard alike comparison
  vetr:::ts_compare(ts.4, 1:4)
})
unitizer_sect("All Attributes, default", {
  vetr:::attr_compare(1, 1)                                           # TRUE
  vetr:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3))  # TRUE
  vetr:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), "hello")  # Error
  vetr:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1.1)      # Error
  vetr:::attr_compare(matrix(integer(), 4), matrix(integer(), 3, 3))           # Dim 1 error
  vetr:::attr_compare(matrix(integer(), ncol=4), matrix(integer(), 3, 3))      # Dim 2 error
  vetr:::attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  vetr:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[2:4])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  vetr:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(letters[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  vetr:::attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  vetr:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  vetr:::attr_compare(                                                         # TRUE
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10])
  )
  vetr:::attr_compare(                                                         # TRUE
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  vetr:::attr_compare(                                                         # TRUE
    structure(unname(data.frame(integer(), character())), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  vetr:::attr_compare(                                                         # TRUE, zero length attr
    structure(list(), welp=list()),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  vetr:::attr_compare(                                                         # Attr length mismatch
    structure(list(), welp=list(NULL)),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  vetr:::attr_compare(                                                         # Missing attr
    structure(list(), welp=list(), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3))
  )
  vetr:::attr_compare(                                                         # TRUE
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:3])
  )
  vetr:::attr_compare(                                                         # class mismatch
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:4])
  )
  vetr:::attr_compare(                                                         # TRUE
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4])
  )
} )
unitizer_sect("All attributes, strict", {
  # dim mismatch, but passes because comparison is `alike`
  vetr:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1)
  # TRUE
  vetr:::attr_compare(matrix(integer(), 3, 3), matrix(integer(), 3, 3), 1)
  # dimnames mismatch, but alike so passes
  vetr:::attr_compare(
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    attr.mode=1
  )
  # dimnames mismatch, but passes because target has NULL names
  vetr:::attr_compare(
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  # dimnames mismatch, but here fails because target has them but current doesnt
  vetr:::attr_compare(
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    attr.mode=1
  )
  # actually passes because both have 2 length character name attrs, which are
  # alike
  vetr:::attr_compare(
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  # TRUE
  vetr:::attr_compare(
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  # TRUE
  vetr:::attr_compare(
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  # Class mismatch
  vetr:::attr_compare(
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4]),
    attr.mode=1
  )
  # Too many attrs
  vetr:::attr_compare(
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  # Too many attrs
  vetr:::attr_compare(
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  # Missing attr
  vetr:::attr_compare(
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3)),
    attr.mode=2
  )
  # Missing attr, but attr count same
  vetr:::attr_compare(
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3), kelp=20),
    attr.mode=2
  )
} )
unitizer_sect("Closures", {
  # TRUE, methods should always match generics
  vetr:::fun_alike(print, print.data.frame)
  # FALSE, but generics won't match methods with more arguments
  vetr:::fun_alike(print.data.frame, print)
  vetr:::fun_alike(summary, summary.lm)
  vetr:::fun_alike(summary.lm, summary)

  fn0 <- function(x, y) NULL
  fn1 <- function(x, y, z) NULL
  fn2 <- function(y, x) NULL
  fn3 <- function(x=1, y=2) NULL
  fn4 <- function(x, ...) NULL
  fn5 <- function(x) NULL
  fn6 <- function(x, y, z, ...) NULL
  fn7 <- function(x, ..., y) NULL
  fn8 <- function(x, a, ..., g, y) NULL
  fn9 <- function(x, a, ..., g, y, w) NULL

  vetr:::fun_alike(fn0, fn1)  # FALSE
  vetr:::fun_alike(fn1, fn0)  # FALSE
  vetr:::fun_alike(fn4, fn1)  # FALSE, dots must be matched
  vetr:::fun_alike(fn0, fn2)  # FALSE
  vetr:::fun_alike(fn0, fn3)  # TRUE
  # FALSE - defaults in target must be specified in current as well
  vetr:::fun_alike(fn3, fn0)
  vetr:::fun_alike(fn4, fn5)  # FALSE dots in target must exit in current
  vetr:::fun_alike(fn4, fn6)  # TRUE
  vetr:::fun_alike(fn4, fn7)  # TRUE
  # FALSE - all arguments in target must be in current, even with dots
  vetr:::fun_alike(fn7, fn4)
  vetr:::fun_alike(fn7, fn8)  # TRUE
  # FALSE - extra arguments in current must be adjacent to dots
  vetr:::fun_alike(fn7, fn9)

  # Try some builtins / specials

  vetr:::fun_alike(`+`, `-`)  # TRUE, builtins
  vetr:::fun_alike(substitute, function(expr, env) NULL)  # TRUE, special
  vetr:::fun_alike(function(expr, env) NULL, substitute)  # TRUE, special
  vetr:::fun_alike(substitute, on.exit)  # FALSE, specials
  vetr:::fun_alike(on.exit, substitute)  # FALSE, specials
  vetr:::fun_alike(`[`, substitute)      # FALSE, argless specials
  vetr:::fun_alike(`[`, `&&`)          # TRUE, argless specials

  # Errors

  vetr:::fun_alike(identity, 10)
  vetr:::fun_alike(10, identity)
})
unitizer_sect("Env Track", {
  el.1 <- replicate(5, new.env())
  el.2 <- el.1[c(1, 1, 2, 3, 4, 1, 2, 3, 5, 1)]
  vetr:::env_track(el.1, 1L)  # first env a freebie, so should be 1
  vetr:::env_track(el.2, 1L)

  # Overwhelm env stack, though not that satifying a test

  vetr:::env_track(el.1, 1L, 3L)

  # Error

  vetr:::env_track(list(1, 2, 3), 1L, 3L)
} )
unitizer_sect("valid names", {
  vetr:::is_valid_name("hello")
  vetr:::is_valid_name(".hello")
  vetr:::is_valid_name("123")
  vetr:::is_valid_name("hello there")
  vetr:::is_valid_name("h1ello")
  vetr:::is_valid_name("_hello")
  vetr:::is_valid_name(".1fail")
  vetr:::is_valid_name("NULL")
  vetr:::is_valid_name("FALSE")

  vetr:::is_valid_name(letters)
} )
unitizer_sect("Is dfish", {
  df1 <- list(a=1:10, b=letters[1:10])
  df2 <- list(a=1:10, b=letters[1:9])
  vetr:::is_dfish(df1)
  vetr:::is_dfish(df2)
  vetr:::is_dfish(1:10)
} )
unitizer_sect("syntactic", {
  vetr:::syntactic_names(quote(hello))
  vetr:::syntactic_names(quote(`hello there`))
  vetr:::syntactic_names(quote(1 + 1))
  vetr:::syntactic_names(quote(1 %hello there% 1))
  vetr:::syntactic_names(quote(1 + `hello there`))
  vetr:::syntactic_names(quote(-(1:3)))
  vetr:::syntactic_names(quote(c(-1:1, NA_integer_)))
  vetr:::syntactic_names(quote(a == 25))
  vetr:::syntactic_names(quote(all(-1:1 > 0)))
})
unitizer_sect("Pad or Quote", {
  vetr:::pad_or_quote(quote(1 + 1))
  vetr:::pad_or_quote(quote(!anyNA(1 + 1)))

  vetr:::pad_or_quote(quote(1 + 1), syntactic=0L)
  vetr:::pad_or_quote(quote(1 + 1), syntactic=1L)
})
unitizer_sect("Merge messages", {
  vetr:::msg_sort(list(letters[5:1], letters[1:5]))
  # third element plays no role in sort
  vetr:::msg_sort(list(c("a", "a", "a", "z", "b"), c("a", "a", "z", "b", "b")))

  # corner cases

  vetr:::msg_sort(list(letters[5:1]))
  vetr:::msg_sort(as.list(letters[5:1]))
  vetr:::msg_sort(list(letters[1:5], NULL))
  vetr:::msg_sort(letters)

  msgs <- list(
    c("`my_var`", "be", "integer", "is", "character"),
    c("`my_var`", "have", "3 columns", "has", "1"),
    c("`length(names(my_var))`", "be", "2", "is", "4"),
    c("`my_var`", "be", "\"NULL\"", "is", "character"),
    c("`attr(my_var)`", "be", "\"NULL\"", "is", "list"),
    c("`my_var`", "be", "matrix", "is", "character"),
    c("`length(names(my_var))`", "be", "3", "is", "4")
  )
  vetr:::msg_merge(msgs)
  vetr:::msg_merge(msgs[1:3])  # no merging required here
  vetr:::msg_merge(msgs[1])    # no merging required here

  vetr:::msg_merge_2(msgs)
})
unitizer_sect("Hash", {
  keys <- vapply(
    1:26, function(x) paste0(letters[seq(x)], collapse=""), character(1L)
  )
  values <- vapply(
    1:26, function(x) paste0(LETTERS[seq(x)], collapse=""), character(1L)
  )
  vetr:::hash_test(keys, values)

  ## hash tracking, uses a hash to detect potential collisions, 1 means a value
  ## is added, >1 means a value was added and tracking array had to be resized
  ## to that size, 0 means it existed already, NA is a reset instruction, value
  ## following a reset instruction is what the reset was to

  vetr:::track_hash(letters[1:3], 2L)  # one resize
  vetr:::track_hash(letters[1:5], 2L)  # two resize

  vetr:::track_hash(c("a", "b", "b"), 2L)          # one repeat
  vetr:::track_hash(c("a", "b", NA, 1,  "b"), 2L)  # reset prior to repeat
  vetr:::track_hash(c("a", "b", NA, 1,  "a"), 2L)

  keys <- c(
    "a", "b", NA, 1,  "b", "hello", "goodbye", "a", NA, 3, "hello",
    "goodbye", "b"
  )
  vetr:::track_hash(keys, 8L)

  # all of these resolve to the same hash value, so should all be stored under
  # the same spot in the hash table.  We use this to test that additions and
  # removals from pairlists work

  collisions <- c("f b", "n b", "n d", "t m", "b r", "n w", "q w", "o x")
  keys.1 <- c(collisions, NA, 0, collisions)

  vetr:::track_hash(keys.1, 64L)

  keys.2 <- c(collisions, NA, 4, collisions)
  vetr:::track_hash(keys.2, 64L)

  # test additions and deletions under collisions

  vetr:::hash_test2(
    c(collisions[1:3], collisions[1:3], "hello"),
    c(T, T, T, F, F, F, F)
  )
})
unitizer_sect("Mode", {
  vetr:::alike_mode(NULL)
  vetr:::alike_mode(quote(a))
  vetr:::alike_mode(mean)
  vetr:::alike_mode(`+`)
  vetr:::alike_mode(log)
  vetr:::alike_mode(quote(1 + 1))
})
unitizer_sect("Find funs", {
  fun <- function(x, y) NULL
  vetr:::find_fun(quote(fun), environment())
  vetr:::find_fun(quote(asdhfqwerasdfasdf), environment())

  fun2 <- function(x) vetr:::find_fun(quote(x), environment())
  # corner case
  fun2()
})
