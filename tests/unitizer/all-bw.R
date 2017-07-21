library(vetr)

x <- runif(100)
x[1] <- 1
x[2] <- 0

unitizer_sect('all_bw', {
  all_bw(x, 0, 1)
  all_bw(x, 0, 1, include.bounds="[)") # fail
  all_bw(x, 0, 1, include.bounds="(]") # fail
  all_bw(x, 0, 1, include.bounds="()") # fail

  all_bw(x, 0, 1 + 1e-6, include.bounds="[)") # pass
  all_bw(x, 0 - 1e-6, 1, include.bounds="(]") # pass
  all_bw(x, 0 - 1e-6, 1 + 1e-6, include.bounds="()") # pass

  y <- z <- x
  y[50] <- NA
  z[50] <- NaN

  all_bw(y, 0, 1)              # fail
  all_bw(y, 0, 1, na.rm=TRUE)  # pass

  all_bw(z, 0, 1)              # fail
  all_bw(z, 0, 1, na.rm=TRUE)  # pass

  w <- runif(1e3, -1e3, 1e3)
  all_bw(w, -1e3, 1e3)
  all_bw(w, -1.5e3, 0.5e3)
  all_bw(w, -0.5e3, 1.5e3)
})
unitizer_sect('corner cases', {
  all_bw(x, 0, 0)                        # fail
  all_bw(0, 0, 0)                        # pass
  all_bw(0, 0, 0, include.bounds="()")   # fail

  all_bw(NA_real_)                       # pass
  all_bw(NA_real_, include.bounds="()")  # fail
})
unitizer_sect('Infinity', {
  z <- w <- v <- u <- runif(100, 1e-100, 1e100)

  all_bw(z, -Inf, Inf)  # pass

  u[50] <- -Inf

  all_bw(u, -1e200, 1e200)    # fail

  v[50] <- Inf

  all_bw(v, -1e200, 1e200)    # fail

  w[50] <- -Inf
  w[51] <- Inf

  all_bw(w, -Inf, Inf)        # pass?
  all_bw(w, -Inf, Inf, include.bounds="[)")  # fail
  all_bw(w, -Inf, Inf, include.bounds="(]")  # fail
})
unitizer_sect('errors', {
  all_bw(x, 0, -1)
  all_bw(x, -1, 1, na.rm=1)
  all_bw(x, -1, 1, na.rm=c(TRUE, FALSE))
  all_bw(x, -1, 1, na.rm=NA)

  all_bw(x, -1, 1, include.bounds=TRUE)
  all_bw(x, -1, 1, include.bounds=letters)
  all_bw(x, -1, 1, include.bounds="[[")
  all_bw(x, -1, 1, include.bounds="))")
})
