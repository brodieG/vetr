library(vetr)

set.seed(42)
x <- runif(100)
x[1] <- 1
x[2] <- 0

unitizer_sect('all_bw - real', {
  all_bw(x, 0, 1)
  all_bw(x, 0, 1, bounds="[)") # fail
  all_bw(x, 0, 1, bounds="(]") # fail
  all_bw(x, 0, 1, bounds="()") # fail

  all_bw(x, 0, 1 + 1e-6, bounds="[)") # pass
  all_bw(x, 0 - 1e-6, 1, bounds="(]") # pass
  all_bw(x, 0 - 1e-6, 1 + 1e-6, bounds="()") # pass

  y <- z <- x
  y[50] <- NA
  z[50] <- NaN

  all_bw(y, 0, 1)                           # fail
  all_bw(y, 0, 1, na.rm=TRUE)               # pass
  all_bw(y, 0.5, .75, na.rm=TRUE)           # fail

  all_bw(y, -1, 2, na.rm=TRUE, bounds="()") # pass
  all_bw(y, 0, 1, na.rm=TRUE, bounds="()")  # fail
  all_bw(y, 0 - 1e-6, 1, na.rm=TRUE, bounds="(]")  # pass
  all_bw(y, 0, 1, na.rm=TRUE, bounds="(]")  # fail
  all_bw(y, 0, 1 + 1e-6, na.rm=TRUE, bounds="[)")  # pass
  all_bw(y, 0, 1, na.rm=TRUE, bounds="[)")  # fail

  all_bw(z, 0, 1)              # fail
  all_bw(z, 0, 1, na.rm=TRUE)  # pass

  all_bw(z) # fail, NaN never inside
  all_bw(z, na.rm=TRUE) # pass

  w <- runif(1e3, -1e3, 1e3)
  all_bw(w, -1e3, 1e3)
  all_bw(w, -1.5e3, 0.5e3)
  all_bw(w, -0.5e3, 1.5e3)
})
unitizer_sect('corner cases - real', {
  all_bw(x, 0, 0)                        # fail
  all_bw(0, 0, 0)                        # pass
  all_bw(0, 0, 0, bounds="()")   # fail

  all_bw(NA_real_)               # fail
  all_bw(NA_real_, bounds="()")  # fail

  all_bw(numeric(), 0, 1)  # pass
  all_bw(numeric(), 0, 0, bounds="()")  # pass?
})
unitizer_sect('Infinity - real', {

  n1e100 <- 1e100
  n_1e100 <- -1e100
  n2e100 <- 2e100
  n_2e100 <- -2e100
  n11e100 <- 1.1e100
  n_11e100 <- -1.1e100
  n1e200 <- 1e200
  n_1e200 <- -1e200

  z <- runif(100, n_1e100, n1e100)
  z[1] <- n_1e100
  z[2] <- n1e100

  r <- w <- v <- u <- x <- z

  # Infinitiy in bounds

  all_bw(z, -Inf, n1e100) # pass
  all_bw(z, -Inf, n1e100, bounds="[)") # fail
  all_bw(z, -Inf, n1e100, bounds="()") # fail
  all_bw(z, -Inf, n1e100, bounds="(]") # pass
  all_bw(c(z, n2e100), -Inf, n1e100, bounds="(]") # fail

  all_bw(z, -Inf, n11e100, bounds="[)") # pass
  all_bw(z, -Inf, n11e100, bounds="()") # pass

  all_bw(z, n_1e100, Inf) # pass
  all_bw(z, n_1e100, Inf, bounds="(]") # fail
  all_bw(z, n_1e100, Inf, bounds="()") # fail
  all_bw(z, n_1e100, Inf, bounds="[)") # pass
  all_bw(c(z, n_2e100), n_1e100, Inf, bounds="[)") # fail

  all_bw(z, n_11e100, Inf, bounds="(]") # pass
  all_bw(z, n_11e100, Inf, bounds="()") # pass

  # Infinity + NA

  r[50] <- NA_real_

  all_bw(r, -Inf, n1e100)                # fail
  all_bw(r, -Inf, n1e100, bounds="[)")   # fail
  all_bw(r, -Inf, n1e100, bounds="()")   # fail
  all_bw(r, -Inf, n1e100, bounds="(]")   # fail

  all_bw(r, -Inf, n11e100, bounds="[)")  # fail
  all_bw(r, -Inf, n11e100, bounds="()")  # fail

  all_bw(r, n_1e100, Inf)  # fail
  all_bw(r, n_1e100, Inf, bounds="(]")  # fail
  all_bw(r, n_1e100, Inf, bounds="()")  # fail
  all_bw(r, n_1e100, Inf, bounds="[)")  # fail

  all_bw(r, n_11e100, Inf, bounds="(]")  # fail
  all_bw(r, n_11e100, Inf, bounds="()")  # fail

  all_bw(r, -Inf, n1e100, na.rm=TRUE)  # pass
  all_bw(r, -Inf, n1e100, bounds="[)", na.rm=TRUE)  # fail
  all_bw(r, -Inf, n1e100, bounds="()", na.rm=TRUE)  # fail
  all_bw(r, -Inf, n1e100, bounds="(]", na.rm=TRUE)  # pass

  all_bw(r, -Inf, n11e100, bounds="[)", na.rm=TRUE) # pass
  all_bw(r, -Inf, n11e100, bounds="()", na.rm=TRUE) # pass

  all_bw(r, n_1e100, Inf, na.rm=TRUE) # pass
  all_bw(r, n_1e100, Inf, bounds="(]", na.rm=TRUE)  # fail
  all_bw(r, n_1e100, Inf, bounds="()", na.rm=TRUE)  # fail
  all_bw(r, n_1e100, Inf, bounds="[)", na.rm=TRUE)  # pass

  all_bw(r, n_11e100, Inf, bounds="(]", na.rm=TRUE) # pass
  all_bw(r, n_11e100, Inf, bounds="()", na.rm=TRUE) # pass

  # special loop case

  all_bw(c(r, n2e100), -Inf, n1e100, na.rm=TRUE)  # fail
  all_bw(c(r, n_2e100), n_1e100, Inf, na.rm=TRUE) # fail

  # Infinity in values

  all_bw(z, -Inf, Inf)  # pass

  u[50] <- -Inf

  all_bw(u, n_1e200, n1e200)    # fail

  v[50] <- Inf

  all_bw(v, n_1e200, n1e200)    # fail

  w[50] <- -Inf
  w[51] <- Inf

  all_bw(w, -Inf, Inf)        # pass?
  all_bw(w, -Inf, Inf, bounds="[)")  # fail
  all_bw(w, -Inf, Inf, bounds="(]")  # fail
})

x.int <- sample(-50:50)

unitizer_sect('all_bw - int', {
  all_bw(x.int, -50, 50)
  all_bw(x.int, -50L, 50L)
  all_bw(x.int, -50, 50, bounds="[)") # fail
  all_bw(x.int, -50, 50, bounds="(]") # fail
  all_bw(x.int, -50, 50, bounds="()") # fail

  all_bw(x.int, -50, 50 + 1e-6, bounds="[)") # pass
  all_bw(x.int, -50 - 1e-6, 50, bounds="(]") # pass
  all_bw(x.int, -50 - 1e-6, 50 + 1e-6, bounds="()") # pass

  y.int <- z.int <- x.int
  y.int[50] <- NA

  all_bw(y.int, -50, 50)                           # fail
  all_bw(y.int, -50, 50, na.rm=TRUE)               # pass
  all_bw(y.int, -49.5, 49.5, na.rm=TRUE)           # fail

  all_bw(y.int, -51, 51, na.rm=TRUE, bounds="()") # pass
  all_bw(y.int, -50.5, 50.5, na.rm=TRUE, bounds="()") # pass
  all_bw(y.int, -50, 50, na.rm=TRUE, bounds="()")  # fail
  all_bw(y.int, -50 - 1e-6, 50, na.rm=TRUE, bounds="(]")  # pass
  all_bw(y.int, -50, 50, na.rm=TRUE, bounds="(]")  # fail
  all_bw(y.int, -50, 50 + 1e-6, na.rm=TRUE, bounds="[)")  # pass
  all_bw(y.int, -50, 50, na.rm=TRUE, bounds="[)")  # fail
})
unitizer_sect('corner cases - int', {
  all_bw(x.int, 0, 0)                        # fail
  all_bw(0L, 0, 0)                        # pass
  all_bw(0L, 0, 0, bounds="()")   # fail

  all_bw(NA_integer_)            # fail
  all_bw(NA)                     # fail
})
unitizer_sect('Infinity - int', {
  int.max <- (Reduce(`*`, rep(2L, 30L)) - 1L) * 2L + 1L
  int.min <- -int.max

  z.int <- x.int
  z.int[1] <- int.max
  z.int[2] <- int.min

  r.int <- w.int <- v.int <- u.int <- x.int <- z.int

  # Infinitiy in bounds

  all_bw(z.int, -Inf, int.max) # pass
  all_bw(z.int, -Inf, int.max, bounds="[)") # fail
  all_bw(z.int, -Inf, int.max, bounds="()") # fail
  all_bw(z.int, -Inf, int.max, bounds="(]") # pass
  all_bw(z.int, -Inf, int.max - 1L, bounds="(]") # fail

  all_bw(z.int, int.min - 1, int.max + 1) # pass
  all_bw(z.int, int.min - 1, int.max + 1, bounds="()") # pass?

  all_bw(int.max - 1L, -Inf, int.max, bounds="()") # pass

  all_bw(z.int, int.min, Inf) # pass
  all_bw(z.int, int.min, Inf, bounds="(]") # fail
  all_bw(z.int, int.min, Inf, bounds="()") # fail
  all_bw(z.int, int.min, Inf, bounds="[)") # pass
  all_bw(z.int, int.min + 1L, Inf, bounds="[)") # fail

  # Infinity + NA

  r.int[50] <- NA_integer_

  all_bw(r.int, -Inf, int.max)
  all_bw(r.int, -Inf, int.max, bounds="[)")
  all_bw(r.int, -Inf, int.max, bounds="()")
  all_bw(r.int, -Inf, int.max, bounds="(]")
  all_bw(r.int, -Inf, int.max - 10, bounds="(]")  # fail

  all_bw(z.int, -Inf, int.max - 1L, bounds="(]", na.rm=TRUE) # fail
  all_bw(c(int.max - 1L, NA), -Inf, int.max, bounds="()", na.rm=TRUE) # pass

  all_bw(r.int, int.min, Inf)
  all_bw(r.int, int.min, Inf, bounds="(]")
  all_bw(r.int, int.min, Inf, bounds="()")
  all_bw(r.int, int.min, Inf, bounds="[)")

  all_bw(r.int, -Inf, int.max, na.rm=TRUE)
  all_bw(r.int, -Inf, int.max, bounds="[)", na.rm=TRUE)
  all_bw(r.int, -Inf, int.max, bounds="()", na.rm=TRUE)
  all_bw(r.int, -Inf, int.max, bounds="(]", na.rm=TRUE)

  all_bw(r.int, int.min + 10, Inf, bounds="[)", na.rm=TRUE)  # fail

  all_bw(r.int, int.min, Inf, na.rm=TRUE)
  all_bw(r.int, int.min, Inf, bounds="(]", na.rm=TRUE)
  all_bw(r.int, int.min, Inf, bounds="()", na.rm=TRUE)
  all_bw(r.int, int.min, Inf, bounds="[)", na.rm=TRUE)
})
unitizer_sect('error', {
  all_bw(x, 0, -1)
  all_bw(x, -1, 1, na.rm=1)
  all_bw(x, -1, 1, na.rm=c(TRUE, FALSE))
  all_bw(x, -1, 1, na.rm=NA)

  all_bw(x, -1, 1, bounds=TRUE)
  all_bw(x, -1, 1, bounds=letters)
  all_bw(x, -1, 1, bounds="[[")
  all_bw(x, -1, 1, bounds="))")
  all_bw(x, -1, 1, bounds="[")
  all_bw(x, -1, 1, bounds="[.]")
  all_bw(x, -1, 1, bounds=NA_character_)

  all_bw(x, 1:3, 4)
  all_bw(x, 1, 4:5)

  all_bw(list(), 1, 2)
  all_bw(x, list(), 2)
  all_bw(x, 1, list())
  all_bw(x, list(1), 2)
  all_bw(x, 1, list(1))
  all_bw(x, "a", 1)
  all_bw(x, 1, "a")
})

unitizer_sect('all_bw - strings', {

  two.let <- two.let.na <- two.let.inf <- c(
    letters,
    do.call(paste0, expand.grid(letters, letters))
  )
  all_bw(letters, "a", "z")
  all_bw(letters, "z", "a") # error
  all_bw(letters, "a", "z", bounds="[)")
  all_bw(letters, "a", "z", bounds="(]")

  all_bw(two.let, "aa", "zz") # no, "a" is less

  all_bw(two.let, "a", "zz")
  all_bw(two.let, "a", "zz", bounds="()")
  all_bw("A", "a", "z", bounds="(]")

  # exclude ends, but still pass

  two.let.2 <- tail(head(two.let, -1), -1)
  all_bw(two.let.2, "a", "zz", bounds="()")
  all_bw(two.let.2, "a", "zz", bounds="[)")
  all_bw(two.let.2, "a", "zz", bounds="(]")

  two.let.2[50] <- NA_character_
  all_bw(two.let.2, "a", "zz", bounds="()", na.rm=TRUE)
  all_bw(two.let.2, "a", "zz", bounds="[)", na.rm=TRUE)
  all_bw(two.let.2, "a", "zz", bounds="(]", na.rm=TRUE)

  # inf bounds

  all_bw(two.let, -Inf, Inf, bounds="()")
  all_bw(two.let, -Inf, Inf, bounds="[)")
  all_bw(two.let, -Inf, Inf, bounds="(]")
  all_bw(two.let, -Inf, Inf, bounds="[]")

  all_bw(two.let, "a", Inf, bounds="()")
  all_bw(two.let, "a", Inf, bounds="[)")
  all_bw(two.let, "a", Inf, bounds="(]")
  all_bw(two.let, "a", Inf, bounds="[]")
  all_bw(two.let, "\t", Inf, bounds="(]")

  all_bw(two.let, -Inf, "zz", bounds="()")
  all_bw(two.let, -Inf, "zz", bounds="[)")
  all_bw(two.let, -Inf, "zz", bounds="(]")
  all_bw(two.let, -Inf, "zz", bounds="[]")

  two.let.inf[1] <- Inf
  two.let.inf[2] <- -Inf

  # All true b/c Inf values coerced to character

  all_bw(two.let.inf, -Inf, Inf, bounds="()")
  all_bw(two.let.inf, -Inf, Inf, bounds="[)")
  all_bw(two.let.inf, -Inf, Inf, bounds="(]")
  all_bw(two.let.inf, -Inf, Inf, bounds="[]")

  # All fail

  two.let.na[50] <- NA_character_

  all_bw(two.let.na, "a", "zz", bounds="()")
  all_bw(two.let.na, "a", "zz", bounds="[)")
  all_bw(two.let.na, "a", "zz", bounds="(]")
  all_bw(two.let.na, "a", "zz", bounds="[]")
  all_bw(two.let.na, -Inf, Inf, bounds="[]")

  # Some pass

  all_bw(two.let.na, "a", "zz", bounds="()", na.rm=TRUE)
  all_bw(two.let.na, "a", "zz", bounds="[)", na.rm=TRUE)
  all_bw(two.let.na, "a", "zz", bounds="(]", na.rm=TRUE)
  all_bw(two.let.na, "a", "zz", na.rm=TRUE)

  all_bw(two.let.na, "b", "zy", na.rm=TRUE)   # fail

  all_bw(two.let.na, -Inf, "zz", na.rm=TRUE)  # pass
  all_bw(two.let.na, -Inf, "zy", na.rm=TRUE)  # fail
  all_bw(two.let.na, -Inf, "zy") # fail

  all_bw(two.let.na, -Inf, "zzz", bounds="[)", na.rm=TRUE) # pass
  all_bw(two.let.na, -Inf, "zz", bounds="[)", na.rm=TRUE)  # fail

  all_bw(two.let.na, -Inf, "zz")  # fail

  all_bw(two.let.na, "a", Inf, na.rm=TRUE)  # pass
  all_bw(two.let.na, "a", Inf)              # fail
  all_bw(two.let.na, "b", Inf, na.rm=TRUE)  # fail

  all_bw(two.let.na, "\t",Inf,  bounds="(]", na.rm=TRUE) # pass
  all_bw(two.let.na, "a", Inf, bounds="(]", na.rm=TRUE)  # fail

  all_bw(two.let.na, "a", Inf)  # fail

  utf8 <- list(
    s4="\xF0\x90\x80\x80",  # four byte start
    e4="\xF4\xBF\xBF\xBF",  # four byte end
    s3="\xE0\xA0\x80",      # three byte start
    e3="\xEF\xBF\xBF",      # three byte end
    s2="\xC2\x80",          # two byte start
    e2="\xDF\xBF"           # two byte end
  )
  for(i in seq_along(utf8)) Encoding(utf8[[i]]) <- "UTF-8"

  # simple tests with utf8 bookends

  all_bw(lorem.ru.phrases, "\t", utf8$e2)
  all_bw(lorem.cn.phrases, "\t", utf8$e3)

  # # these two produce unsupressable warnings on Solaris
  # all_bw(lorem.ru.phrases, "\t", utf8$s2)
  # all_bw(lorem.cn.phrases, "\t", utf8$e2)

  # Unfortunately something is going wrong with how out-of-BMP unicode is read
  # in by windows so we have to comment out these tests; see #82
  # all_bw(lorem.emo.phrases, "\t", utf8$s4)
  # all_bw(lorem.emo.phrases, "\t", utf8$e4)
})
