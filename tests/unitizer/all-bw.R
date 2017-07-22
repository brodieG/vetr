library(vetr)

x <- runif(100)
x[1] <- 1
x[2] <- 0

unitizer_sect('all_bw', {
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
unitizer_sect('corner cases', {
  all_bw(x, 0, 0)                        # fail
  all_bw(0, 0, 0)                        # pass
  all_bw(0, 0, 0, bounds="()")   # fail

  all_bw(NA_real_)                       # pass
  all_bw(NA_real_, bounds="()")  # fail
})
unitizer_sect('Infinity', {
  z <- runif(100, -1e100, 1e100)
  z[1] <- -1e100
  z[2] <- 1e100

  r <- w <- v <- u <- x <- z

  # Infinitiy in bounds

  all_bw(z, -Inf, 1e100) # pass
  all_bw(z, -Inf, 1e100, bounds="[)") # fail
  all_bw(z, -Inf, 1e100, bounds="()") # fail
  all_bw(z, -Inf, 1e100, bounds="(]") # pass
  all_bw(c(z, 2e100), -Inf, 1e100, bounds="(]") # fail

  all_bw(z, -Inf, 1.1e100, bounds="[)") # pass
  all_bw(z, -Inf, 1.1e100, bounds="()") # pass

  all_bw(z, -1e100, Inf) # pass
  all_bw(z, -1e100, Inf, bounds="(]") # fail
  all_bw(z, -1e100, Inf, bounds="()") # fail
  all_bw(z, -1e100, Inf, bounds="[)") # pass
  all_bw(c(z, -2e100), -1e100, Inf, bounds="[)") # fail

  all_bw(z, -1.1e100, Inf, bounds="(]") # pass
  all_bw(z, -1.1e100, Inf, bounds="()") # pass

  # Infinity + NA

  r[50] <- NA_real_

  all_bw(r, -Inf, 1e100)
  all_bw(r, -Inf, 1e100, bounds="[)")
  all_bw(r, -Inf, 1e100, bounds="()")
  all_bw(r, -Inf, 1e100, bounds="(]")

  all_bw(r, -Inf, 1.1e100, bounds="[)")
  all_bw(r, -Inf, 1.1e100, bounds="()")

  all_bw(r, -1e100, Inf)
  all_bw(r, -1e100, Inf, bounds="(]")
  all_bw(r, -1e100, Inf, bounds="()")
  all_bw(r, -1e100, Inf, bounds="[)")

  all_bw(r, -1.1e100, Inf, bounds="(]")
  all_bw(r, -1.1e100, Inf, bounds="()")

  all_bw(r, -Inf, 1e100, na.rm=TRUE)
  all_bw(r, -Inf, 1e100, bounds="[)", na.rm=TRUE)
  all_bw(r, -Inf, 1e100, bounds="()", na.rm=TRUE)
  all_bw(r, -Inf, 1e100, bounds="(]", na.rm=TRUE)

  all_bw(r, -Inf, 1.1e100, bounds="[)", na.rm=TRUE)
  all_bw(r, -Inf, 1.1e100, bounds="()", na.rm=TRUE)

  all_bw(r, -1e100, Inf, na.rm=TRUE)
  all_bw(r, -1e100, Inf, bounds="(]", na.rm=TRUE)
  all_bw(r, -1e100, Inf, bounds="()", na.rm=TRUE)
  all_bw(r, -1e100, Inf, bounds="[)", na.rm=TRUE)

  all_bw(r, -1.1e100, Inf, bounds="(]", na.rm=TRUE)
  all_bw(r, -1.1e100, Inf, bounds="()", na.rm=TRUE)

  # special loop case

  all_bw(c(r, 2e100), -Inf, 1e100, na.rm=TRUE)  # fail
  all_bw(c(r, -2e100), -1e100, Inf, na.rm=TRUE) # fail

  # Infinity in values

  all_bw(z, -Inf, Inf)  # pass

  u[50] <- -Inf

  all_bw(u, -1e200, 1e200)    # fail

  v[50] <- Inf

  all_bw(v, -1e200, 1e200)    # fail

  w[50] <- -Inf
  w[51] <- Inf

  all_bw(w, -Inf, Inf)        # pass?
  all_bw(w, -Inf, Inf, bounds="[)")  # fail
  all_bw(w, -Inf, Inf, bounds="(]")  # fail
})
unitizer_sect('errors', {
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
