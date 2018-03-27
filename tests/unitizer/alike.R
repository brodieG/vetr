library(vetr)

unitizer_sect("Atomic", {
  alike(integer(), 1:3)    # TRUE
  alike(integer(5L), 1:3)  # FALSE
  alike(integer(3L), 1:3)  # TRUE
  alike(numeric(), c(1, 2, 3))         # TRUE
  alike(numeric(), 1L)                 # TRUE
  alike(numeric(), c(1.1,.053,41.8))   # TRUE
  alike(integer(3L), 1:3 + .01)
  alike(integer(6L), seq(1/6, 1, 1/6) * 6)     # FALSE, not true integers
  alike(integer(4L), letters[1:4])
  alike(letters[1:4], c("hello", "goodbye", "ba", "da"))  # TRUE

  alike(integer(), NULL)    # FALSE, corner case test

  alike(c(a=1, b=2), 3)         # Length mismatch
  alike(c(a=1, b=2), c(1, 2))   # Names
} )
unitizer_sect("lists", {
  lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

  alike(lst, lst.2)     # length mismatch

  lst.3 <- lst.2
  lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

  alike(lst, lst.3)     # object type mismatch
  alike(1:10, "hello")  # object type mismatch, no dive

  alike(lst, lst)       # obvious match

  lst.4 <- lst
  lst.4[[2]][[2]] <- list()

  alike(lst.4, lst)     # should match
  alike(lst, lst.4)     # should not match because template has more detail

  # Named lists

  lst.5 <- list(1, list(a = 1, b = 2, c = list(d = 1)))
  lst.6 <- list(1, list(a = 1, b = 2, c = list(d = "hello")))
  lst.5.1 <- list(1, list(a = 1, b = 2, `c d` = list(d = 1)))
  lst.6.1 <- list(1, list(a = 1, b = 2, `c d` = list(d = "hello")))

  alike(lst.5, lst.6)
  alike(lst.6, lst.5)

  alike(lst.5.1, lst.6.1)
  alike(lst.6.1, lst.5.1)

  # Pair lists

  alike(pairlist(a=1, b="character"), pairlist(a=1, b=letters))
  alike(pairlist(1, "character"), pairlist(1, letters))
})
unitizer_sect("NULL values as wildcards", {
  alike(NULL, 1:3)                  # not a wild card at top level
  alike(list(NULL), list(1:3))      # but yes when nested
  alike(list(NULL, NULL), list(list(list(1, 2, 3)), 1:25))
  alike(list(NULL), list(1, 2))
  alike(list(), list(1, 2))
})
unitizer_sect("Matrix / Arrays", {
  alike(matrix(integer(), ncol=7), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(character(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=4), matrix(1:21, nrow=3))
  alike(
    matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))),
    matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B")))
  )
  alike(
    matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)),
    matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B")))
  )
  alike(
    matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)),
    matrix(1:9, nrow=3, dimnames=list(NULL, c("R", "G", "B")))
  )
  alike(
    matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)),
    matrix(
      1:9, nrow=3, dimnames=list(c("R", "G", "B"),
      c("bacon", "turkey", "bravo"))
  ) )
  alike(matrix(1:9, nrow = 3), 1:9)

  # Adding tests from docs

  mx.tpl <- matrix(
    integer(), ncol=3, dimnames=list(row.id=NULL, c("R", "G", "B"))
  )
  mx.cur <- matrix(
    sample(0:255, 12), ncol=3, dimnames=list(row.id=1:4, rgb=c("R", "G", "B"))
  )
  mx.cur2 <-
    matrix(sample(0:255, 12), ncol=3, dimnames=list(1:4, c("R", "G", "B")))

  alike(mx.tpl, mx.cur)
  alike(mx.tpl, mx.cur2)
} )
unitizer_sect("Data Frames", {
  alike(mtcars, 1:3)
  alike(1:3, mtcars)
  alike(
    data.frame(),
    data.frame(a=1:3, b=letters[1:3])
  )  # TRUE
  alike(       # TRUE, note this is recursive
    data.frame(a=integer(), b=factor()),
    data.frame(a=1:3, b=letters[1:3])
  )
  # FALSE mis-match at index[[1]]
  alike(
    data.frame(a=factor(), b=factor()),
    data.frame(a=1:3, b=letters[1:3])
  )
  # FALSE mis-match at index[[2]] (class)
  alike(
    list(NULL, structure("a", class="x")),
    list(NULL, structure("a", class="y"))
  )
  # TRUE, more complex nested structure
  # row.names / names (note use `structure` to get around `data.frame` checks)

  alike(
    list(
      integer(), data.frame(a=integer(), b=numeric()),
      matrix(integer(), nrow=3)
    ),
    list(1:10, data.frame(a=1:200, b=runif(20)), matrix(1:27, nrow=3))
  )

  df.tpl <- structure(
    list(1:4, factor(LETTERS[1:4], levels=LETTERS)),
    row.names=c("one", "", "", ""), names=c("id", ""), class="data.frame"
  )
  df.cur <- `row.names<-`(
    data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)),
    c("one", "two", "tre", "qtr")
  )
  df.cur2 <- `row.names<-`(
    data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)),
    c("uno", "due", "tre", "qtr")
  )
  alike(df.tpl, df.cur)   # TRUE
  alike(df.cur, df.tpl)   # Nope, names won't match reversed
  alike(df.tpl, df.cur2)  # Nope, row.names won't match

  # NA names

  df.tpl <-
    structure(list(1:4, letters[1:4]), names=c("id", NA), class="data.frame")
  df.cur <-
    structure(list(1:4, letters[1:4]), names=c("id", "val"), class="data.frame")

  alike(df.tpl, df.tpl)
  alike(df.tpl, df.cur)

  # special treatment

  alike(mtcars, iris)
  alike(mtcars, mtcars[1:10,])
  alike(mtcars[-5], mtcars)
})
unitizer_sect("Time Series", {
  ts.1 <- ts(runif(24), 1970, frequency=12)
  ts.2 <- ts(runif(24), 1970, frequency=4)
  ts.3 <- abstract(ts.1, "end")
  ts.4 <- abstract(ts.2, "frequency")

  alike(ts.1, ts.2)
  alike(ts.3, ts.1)
  alike(ts.1, ts.3)
  alike(ts.3, ts.2)

  ts.5 <- ts(matrix(runif(24 * 3), ncol=3), 1970, frequency=12)
  ts.6 <- ts(matrix(runif(12 * 3), ncol=3), 1970, frequency=12)

  alike(ts.5, ts.6)
  alike(ts.5, matrix(runif(24 * 3), ncol=3))
})
unitizer_sect("Factors", {
  f1 <- factor(letters[1:5])
  f2 <- factor(letters[1:5], levels=letters[5:1])
  f3 <- f1
  levels(f3)[[5]] <- ""
  f4 <- factor(c(letters[1:4], "f"))

  alike(f1, f2)   # FALSE
  alike(f1, f3)   # FALSE
  alike(f1, f4)   # FALSE
  alike(f3, f1)   # TRUE, wildcard matches anything
  alike(f3, f4)   # TRUE, wildcard matches anything
})
unitizer_sect("Environments / Pairlists", {
  env0 <- new.env()
  env1 <- list2env(list(a=character(), b=list(), c=NULL))
  env2 <- list2env(list(a="hello", b=iris, c=matrix(1:3)))
  env3 <- list2env(list(a="hello", b=iris))
  env4 <- list2env(list(a="hello", b=iris, c=logical(1L), d=logical(1L)))
  env5 <- list2env(list(b=iris, a="hello", c=matrix(1:3)))

  alike(env0, env2)  # zero length, matches anything
  alike(env1, env2)  # TRUE
  alike(env1, env3)  # length mismatch
  alike(env3, env1)  # component mismatch
  alike(env1, env4)  # TRUE length mismatch but longer allowed
  alike(env1, env5)  # order change, should still match

  # Test infinite recursion protection

  rec.env <- rec.env.cpy <- new.env()

  for(i in 1:50) {
    rec.env.cpy$a <- new.env()
    rec.env.cpy <- rec.env.cpy$a
  }
  rec.env.cpy$a <- rec.env;
  alike(rec.env, rec.env)

  plst1 <- pairlist(a=character(), b=list(), c=NULL)
  plst2 <- pairlist(a="hello", b=iris, c=matrix(1:3))
  plst3 <- pairlist(a="hello", b=iris)
  plst4 <- pairlist(a="hello", b=iris, c=logical(1L), d=logical(1L))
  plst5 <- pairlist(a=character(), b=list(), integer())
  plst6 <- pairlist(a=character(), b=list(), boogey=1:3)
  plst7 <- pairlist(a=character(), boogey=1:3, b=list())

  alike(plst1, plst2)  # TRUE
  alike(plst1, plst3)  # length mismatch
  alike(plst1, plst4)  # length mismatch
  alike(plst1, plst5)  # fail, missing name
  alike(plst5, plst6)  # TRUE, no name matches anything
  alike(plst5, plst7)  # FALSE, order matters in pair lists

  # Nesting

  env7 <- list2env(list(a=character(), b=plst1))
  env8 <- list2env(list(a=letters[1:3], b=plst2))
  env9 <- list2env(list(a=letters[1:3], b=plst5))

  alike(env7, env8)   # pass
  alike(env7, env9)   # fail

  # Overwhelm env nesting

  env.nest.1 <- env.nest.1.cpy <- new.env()
  env.nest.2 <- env.nest.2.cpy <- new.env()
  for(i in 1:26) {
    env.nest.1.cpy[[letters[i]]] <- new.env();
    env.nest.1.cpy <- env.nest.1.cpy[[letters[i]]]
    env.nest.2.cpy[[letters[i]]] <- new.env();
    env.nest.2.cpy <- env.nest.2.cpy[[letters[i]]]
  }
  alike(env.nest.1, env.nest.2, settings=vetr_settings(env.depth.max=16))

  # Global env test

  alike(.GlobalEnv, env.nest.1)
})
unitizer_sect("Calls / Formulas", {
  alike(quote(1 + 1), quote(x + y))
  alike(quote(fun(1 + 1)), quote(fun(x + y, 9)))
  alike(quote(fun(x + y, 9)), quote(fun(1 + 1)))

  # Need to add parens in error messages, which we will illustrate with an
  # operator

  "%plusq%" <- function(x, y) call("+", substitute(x), substitute(y))
  alike(quote(1 + 1), 1 %plusq% b)

  # With defined fun

  fun <- function(a, b, c) NULL
  # TRUE, since constants including NULL match any constants
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))
  alike(  # FALSE, match.call disabled
    quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)),
    settings=vetr_settings(lang.mode=1)
  )
  # FALSE, mismatch
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(fun2(a, b), NULL, 1)))
  alike(quote(fun(a=1)), quote(fun(b=1)))  # FALSE, name mismatch

  alike(quote(fun(1, 2)), quote(fun(1)))   # FALSE
  alike(quote(fun(1)), quote(fun(1, 2)))   # FALSE

  alike(quote(fun(1, 2)), quote(fun2(1, 2)))            # FALSE, fun mismatch
  # FALSE, fun mismatch, nested
  alike(quote(fun(1, fun2(3))), quote(fun(1, fun(3))))

  # zero len matches anything

  alike(quote(fun()), quote(fun(a, b, c)))    # TRUE
  alike(quote(fun()), quote(fun2(a, b, c)))   # FALSE, still need match fun names
  alike(quote(fun(a, fun2())), quote(fun(b, fun2(a, b, c))))    # TRUE

  # Attributes on sub-components should not affect anything
  # actually, these tests need to be with alike since lang_alike doesn't check
  # attributes

  c0 <- quote(fun(a, b, a, 25))
  c0.1 <- c0.2 <- c0
  attr(c0.1, "blah") <- "hello"
  attr(c0.2, "blah") <- 1:3

  alike(c0, c0.1)     # TRUE
  alike(c0.1, c0)     # Missing attribute
  alike(c0.1, c0.2)   # Attribute mismatch

  # Formulas

  alike(x ~ y, z ~ w)
  alike(x ~ y, z ~ w + 1)
  alike(x ~ y + 2, z ~ w + 1)
  alike(x ~ y + z:y, w ~ v + u:v)
  alike(z ~ w + 1, x ~ y)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 2 + l * j + j + w:j)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 3 + l * j + j + w:j)

  # # Repeating parses to deal with potential parse issues in clean R runs

  exp.1 <- parse(text="x + y; fun2(fun(1, 2, 3), z)", keep.source=TRUE)
  exp.2 <- parse(text="z + 2; fun(fun2(1, 2, 3), q)", keep.source=TRUE)
  exp.3 <- parse(text="z + fun(3); fun(fun2(a, b, c), 3)", keep.source=TRUE)

  alike(exp.1, exp.2)
  alike(exp.2, exp.3)
  alike(exp.3, exp.2)

  exp.4 <- expression(1 + 1, 2 + x)
  exp.5 <- expression(1 + 1, 5 + y)
  exp.6 <- expression(1 + 1, 2 + 2)

  alike(exp.4, exp.5) # TRUE
  alike(exp.4, exp.6) # FALSE

  # Symbols

  alike(quote(x), quote(y))      # TRUE
  alike(NULL, quote(x))          # FALSE, overridden by type comparison
  alike(quote((NULL)), quote(y)) # TRUE, NULL matches anything as language object
  alike(quote(NULL), quote(x))   # FALSE, quoting NULL doesn't make it language
  alike(quote(x), c0)          # FALSE
  alike(c0, quote(x))          # FALSE
  alike(quote((x)), quote(y))    # TRUE, parens shouldn't matter
} )
# Most fun tests in internal/type, here to make sure interface working
unitizer_sect("Functions", {
  alike(print, print.data.frame)              # TRUE
  alike(print.data.frame, print)              # FALSE
  alike(`&&`, function() NULL)                # TRUE

  # check srcref issues

  fun <- fun2 <- function() NULL
  attributes(fun2) <- NULL

  alike(fun, fun2)   # TRUE
  alike(fun, fun2, settings=vetr_settings(attr.mode=2L))
  alike(fun2, fun, settings=vetr_settings(attr.mode=1L))
  alike(fun2, fun, settings=vetr_settings(attr.mode=2L))
})

# Subset of tests for version with settings

unitizer_sect("settings", {
  alike(1L, 1.0, settings=vetr_settings(type.mode=1L))
  alike(1.0, 1L, settings=vetr_settings(type.mode=1L))
  alike(1.0, 1L, settings=vetr_settings(type.mode=2L))   # FALSE
  alike(1:101, 1:101 + 0.0)  # FALSE
  # TRUE
  alike(1:101, 1:101 + 0.0, settings=vetr_settings(fuzzy.int.max.len=200))
  # TRUE
  alike(1:101, 1:101 + 0.0, settings=vetr_settings(fuzzy.int.max.len=-1))
  alike(list(a=1:10), data.frame(a=1:10))
  alike(list(a=1:10), data.frame(a=1:10), settings=vetr_settings(attr.mode=1L))
  # FALSE
  alike(list(a=1:10), data.frame(a=1:10), settings=vetr_settings(attr.mode=2L))

  fun <- function(a, b, c) NULL

  call.a <- quote(fun(b=fun2(x, y), 1, 3))
  call.b <- quote(fun(NULL, fun2(a, b), 1))

  # FALSE, function not defined in empty env

  alike(call.a, call.b, settings=vetr_settings(env=emptyenv()))

  # FALSE, also not defined this way

  alike(call.a, call.b, env=emptyenv())

  # TRUE, verify that settings takes precedence
  alike(
    call.a, call.b, env=emptyenv(), settings=vetr_settings(env=environment())
  )
  # TRUE
  alike(
    call.a, call.b
  )
  # FALSE

  alike(`&&`, function() NULL, settings=vetr_settings(type.mode=1))

  # Error

  alike(1, 2, settings=letters)
  alike(1, 2, settings=list())
  alike(1, 2, settings=setNames(vector("list", 16), letters[1:16]))
  alike(1, 2, settings=vector("list", 16))
} )
# These are also part of the examples, but here as well so that issues are
# detected during development and not the last minute package checks

unitizer_sect("Examples", {
  alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
  alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
  alike(1.1, 1L)         # TRUE, by default, integers are always considered real

  alike(1:100, 1:100 + 0.0)  # TRUE
  # FALSE, we do not check numerics for integerness if longer than 100
  alike(1:101, 1:101 + 0.0)

  # Scalarness can now be checked at same time as type

  alike(integer(1L), 1)            # integer-like and length 1?
  alike(logical(1L), TRUE)         # logical and length 1?
  alike(integer(1L), 1:3)
  alike(logical(1L), c(TRUE, TRUE))

  # Zero length match any length of same type

  alike(integer(), 1:10)
  alike(1:10, integer())   # but not the other way around

  # Recursive objects compared recursively

  alike(
    list(integer(), list(character(), logical(1L))),
    list(1:10, list(letters, TRUE))
  )
  alike(
    list(integer(), list(character(), logical(1L))),
    list(1:10, list(letters, c(TRUE, FALSE)))
  )
  # `NULL` is a wild card when nested within recursive objects

  alike(list(NULL, NULL), list(iris, mtcars))
  alike(NULL, mtcars)    # but not at top level

  # Since `data.frame` are lists, we can compare them recursively:

  iris.fake <- transform(iris, Species=as.character(Species))
  alike(iris, iris.fake)
  iris.fake2 <- transform(
    iris,
    Species=factor(Species, levels="[[<-"(levels(Species), 3, "americana"))
  )
  alike(iris, iris.fake2)  # we even check attributes (factor levels must match)!

  # We can use partially specified objects as templates

  iris.tpl <- abstract(iris)
  str(iris.tpl)
  alike(iris.tpl, iris)
  # any row sample of iris matches our iris template
  alike(iris.tpl, iris[sample(1:nrow(iris), 10), ])
  alike(iris.tpl, iris[c(2, 1, 3, 4, 5)])              # but column order matters

  # Also works with matrices / arrays

  alike(matrix(integer(), 3, 3), matrix(1:9, nrow=3))         # 3 x 3 integer
  # 3 x 3, but not integer!
  alike(matrix(integer(), 3, 3), matrix(runif(9), nrow=3))
  # partial spec, any 3 row integer matrix
  alike(matrix(integer(), 3), matrix(1:12, nrow=3))
  alike(matrix(integer(), 3), matrix(1:12, nrow=4))
  # Any logical matrix (but not arrays)
  alike(matrix(logical()), array(rep(TRUE, 8), rep(2, 3)))

  # In order for objects to be alike, they must share a family tree, not just
  # a common class

  obj.tpl <- structure(TRUE, class=letters[1:3])
  obj.cur.1 <-  structure(TRUE, class=c("x", letters[1:3]))
  obj.cur.2 <-  structure(TRUE, class=c(letters[1:3], "x"))

  alike(obj.tpl, obj.cur.1)
  alike(obj.tpl, obj.cur.2)

  # You can compare language objects; these are alike if they are self
  # consistent; we don't care what the symbols are, so long as they are used
  # consistently across target and current:

  # TRUE, symbols are consistent (adding two different symbols)
  alike(quote(x + y), quote(a + b))
  alike(quote(x + y), quote(a - b))   # FALSE, different function
  alike(quote(x + y), quote(a + a))   # FALSE, inconsistent symbols
} )
unitizer_sect("Raw", {
  # check for warning, in the future if we properly support RAW then this will
  # no longer produce a warning.  Really just looking for a valid STRSXP type.

  alike(as.raw(integer(3)), as.raw(integer(3)))
})
unitizer_sect("Errors", {
  alike(NULL, NULL, settings=vetr_settings(type.mode=3))
  alike(NULL, NULL, settings=vetr_settings(attr.mode=letters))
  alike(NULL, NULL, settings=vetr_settings(lang.mode=letters))
  alike(NULL, NULL, settings=vetr_settings(fuzzy.int.max.len=NA_integer_))
  alike(NULL, NULL, settings=vetr_settings(suppress.warnings=NA))
  alike(NULL, NULL, settings=vetr_settings(env=letters))
  alike(NULL, NULL, settings=vetr_settings(width=letters))
  alike(NULL, NULL, settings=vetr_settings(env.depth.max=-1L))
})
unitizer_sect("Attributes", {
  # There are implicit and explicit attribute checks elsewhere, this
  # is just for a corner case that showed up in coverage

  obj.tpl <- structure(integer(), a=integer())
  obj.obj <- structure(1:10, a=1:3)

  alike(obj.tpl, obj.obj)

  # issue93 More corner cases from the transition to sorted attribute checks

  obj.tpl.a <- structure(integer(), class='foo')
  obj.obj.a <- structure(matrix(1:3))

  alike(obj.tpl.a, obj.obj.a)

  obj.tpl.b <- structure(matrix(integer()), class='foo')
  obj.obj.b <- structure(matrix(1:3))

  alike(obj.tpl.b, obj.obj.b)

  obj.tpl.c <- structure(matrix(integer()), class='foo')
  obj.obj.c <- structure(matrix(1:3), class='foo')

  alike(obj.tpl.c, obj.obj.c)

  obj.tpl.d <- structure(integer(), class='foo')
  obj.obj.d <- structure(matrix(1:3), class='foo')

  alike(obj.tpl.d, obj.obj.d)

  obj.tpl.e <- structure(integer())
  obj.obj.e <- structure(matrix(1:3), class='foo')

  alike(obj.tpl.e, obj.obj.e)

  obj.tpl.f <- structure(integer(), a=integer())
  obj.obj.f <- structure(matrix(1:3), z=integer(), class='foo')

  alike(obj.tpl.f, obj.obj.f)

  obj.tpl.g <- structure(integer(), a=integer())
  obj.obj.g <- structure(matrix(1:3), z=integer(), class='foo')

  alike(obj.tpl.g, obj.obj.g)

  obj.tpl.h <- structure(matrix(integer()))
  obj.obj.h <- structure(1:3, a=integer())

  alike(obj.tpl.h, obj.obj.h)
  alike(obj.tpl.h, obj.obj.h, settings=vetr_settings(attr.mode=2))

  obj.tpl.i <- structure(character(), a=integer(), class='boo')
  obj.obj.i <- structure(matrix(1:3))

  alike(obj.tpl.i, obj.obj.i)

  obj.tpl.k <- integer()
  obj.obj.k <- matrix(1:3)

  alike(obj.tpl.k, obj.obj.k)
  alike(obj.tpl.k, obj.obj.k, settings=vetr_settings(attr.mode=2))
})
