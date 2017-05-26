# redefine funs to give us flexibility if we change packages without having
# to export the internal functions

library(vetr)

unitizer_sect("Match Calls", {
  vetr:::match_call_alike(quote(var(y=1:10, runif(10))), baseenv())
  env0 <- new.env()
  env0$var <- function(yollo, zambia) NULL
  vetr:::match_call_alike(quote(var(y=1:10, runif(10))), env0)
})
unitizer_sect("Calls", {
  c0 <- quote(fun(a, b, a, 25))
  c1 <- quote(fun(x, y, x, "hello"))
  c2 <- quote(fun(x, y, z, "hello"))
  c3 <- quote(FUN(x, y, x, 1.01))
  c4 <- quote(fun(x, y, x, z))
  c5 <- quote(fun(a + b + a, FUN(z, a + 1)))
  c6 <- quote(fun(x + y + x, FUN(w, x + 2)))
  c7 <- quote(fun(x + y + x, FUN(w, y + 2)))
  c8 <- quote(fun(x + y + x, FUN(w, x - 2)))
  c9 <- quote(fun(x + y + x, FUN(w, x + "hello")))
  c10 <- quote(fun(1))
  c11 <- quote(fun(1, 2))

  c12 <- quote(a + b + c)
  c13 <- quote((a + b) + c)
  c14 <- quote(a + (b + c))

  vetr:::lang_alike(c0, c1, NULL)  # TRUE
  vetr:::lang_alike(c0, c2, NULL)  # no, inconsistent
  vetr:::lang_alike(c0, c3, NULL)  # no, wrong fun name
  vetr:::lang_alike(c0, c4, NULL)  # extra symbol
  vetr:::lang_alike(c5, c6, NULL)  # TRUE
  vetr:::lang_alike(c5, c7, NULL)  # inconsistent
  vetr:::lang_alike(c5, c8, NULL)  # wrong call `-`
  vetr:::lang_alike(c5, c9, NULL)  # TRUE
  vetr:::lang_alike(c11, c10, NULL)# Length mismatch

  # Parens

  vetr:::lang_alike(c12, c13)   # equivalent
  vetr:::lang_alike(c12, c14)   # not equivalent
  vetr:::lang_alike(c13, c14)   # not equivalent

  vetr:::lang_alike(c14, c13)   # not equivalent

  # with defined fun

  fun <- function(abc, bcd, efg) NULL

  ca <- quote(fun(a, b, a))
  cb <- quote(fun(x, e=x, y))

  vetr:::lang_alike(ca, cb, NULL)      # shouldn't match without match.call
  vetr:::lang_alike(cb, ca, NULL)      # false, different error
  vetr:::lang_alike(ca, cb)            # TRUE, should match

  # Actually use a function (and not just name of fun)

  ca.1 <- ca
  cb.1 <- cb

  ca.1[[1]] <- fun
  cb.1[[1]] <- fun

  vetr:::lang_alike(ca.1, cb.1)         # TRUE, should match

  # test nested match.call

  cc <- quote(fun(a, b, fun(b=1)))
  cd <- quote(fun(a, b, fun(c=1)))

  vetr:::lang_alike(cc, cd)

  # NULL in target matches anything

  ce <- quote(fun(a, b, NULL))

  vetr:::lang_alike(cc, ce)  # FALSE
  vetr:::lang_alike(ce, cc)  # TRUE

  # mismatched functions

  da <- quote(ff(a=1, b=2, c=3))
  db <- quote(ff(a=1, d=2, c=3))

  vetr:::lang_alike(da, db)

  # Errors

  vetr:::lang_alike(cc, 1:10)
  vetr:::lang_alike(ce, cc, match.call.env=1:10)
})
unitizer_sect("Calls as char", {
  vetr:::lang_alike_chr(c0, c1, NULL)  # TRUE
  vetr:::lang_alike_chr(c0, c2, NULL)  # no, inconsistent
  vetr:::lang_alike_chr(c0, c3, NULL)  # no, wrong fun name
  vetr:::lang_alike_chr(c0, c4, NULL)  # extra symbol
  vetr:::lang_alike_chr(c5, c6, NULL)  # TRUE
  vetr:::lang_alike_chr(c5, c7, NULL)  # inconsistent
  vetr:::lang_alike_chr(c5, c8, NULL)  # wrong call `-`
  vetr:::lang_alike_chr(c5, c9, NULL)  # TRUE

  vetr:::lang_alike_chr(ca, cb, NULL)      # shouldn't match without match.call
  vetr:::lang_alike_chr(cb, ca, NULL)      # false, different error
  vetr:::lang_alike_chr(ca, cb)            # TRUE, should match

  # test nested match.call

  vetr:::lang_alike_chr(cc, cd)

  # NULL in target matches anything

  vetr:::lang_alike_chr(cc, ce)  # FALSE
  vetr:::lang_alike_chr(ce, cc)  # TRUE
})
unitizer_sect("Formulas", {
  f0 <- y ~ x + 1
  f1 <- a ~ b + 1
  f2 <- a ~ b + 2
  f3 <- y ~ x + log(x) + z - 1
  f4 <- a ~ b + log(b) + c - 1
  f5 <- a ~ b + log(c) + b - 1
  f6 <- a ~ b + ln(b) + c - 1
  f7 <- a ~ b + log(b) + c + 1

  vetr:::lang_alike(f0, f1, NULL)        # TRUE
  vetr:::lang_alike(f0, f2, NULL)        # FALSE
  vetr:::lang_alike(f3, f4, NULL)        # TRUE
  vetr:::lang_alike(f3, f5, NULL)        # FALSE
  vetr:::lang_alike(f3, f6, NULL)        # FALSE
  vetr:::lang_alike(f3, f7, NULL)        # FALSE
})
unitizer_sect("Deparse", {
  l0 <- quote(
    a + b + fun(x + funz(
      matrix_over[25, 32]) + transform(iris, x = Sepal.Width * 3) /
      the_donkey_ate_a_carrot %in% {
        paste0(
          match(letter, LETTERS),
          c("hello there")
  ) } ) )
  # simple deparse

  (dep.txt <- vetr:::dep_alike(l0))
  vetr:::dep_alike(l0, 30)

  # manip the deparse

  vetr:::pad(dep.txt)
  old.opt <- options(prompt=">>", continue=" |")
  vetr:::pad(dep.txt)
  options(old.opt)
  vetr:::pad(dep.txt, pad=4)
  vetr:::pad(dep.txt, pad=4, lines=2)

  # oneline

  vetr:::dep_oneline(quote(1 + 1 + 3 + 944254235), 10)
  vetr:::dep_oneline(quote(1 + 1 + 3), 10)
  vetr:::dep_oneline(quote(1 + 1 + 3), "hello")
  vetr:::dep_oneline(quote(1 + 1 + 3 - (mean(1:10) + 3)), 15, 1L)
})
