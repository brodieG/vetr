# Copyright (C) 2023 Brodie Gaslam
#
# This file is part of "vetr - Trust, but Verify"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

library(vetr)

unitizer_sect("All", {
  vetr:::val_all(1:10)  # -2
  vetr:::val_all(rep(TRUE, 10)) # 1
  vetr:::val_all(c(rep(TRUE, 10), FALSE, TRUE)) # 0
  vetr:::val_all(c(rep(TRUE, 5), NA, rep(TRUE, 5))) # -4
  vetr:::val_all(FALSE) # -1
  vetr:::val_all(TRUE)  # 2
  vetr:::val_all(logical()) # 3, this used to be -5
  vetr:::val_all(NA)  # -3
  vetr:::val_all(c(TRUE, TRUE, NA, TRUE))  # -4
})

unitizer_sect("Hash", {
  # these should all equal 193
  vetr:::hash_fun(c("f b", "n b", "n d", "t m", "b r", "n w", "q w", "o x"))
})
unitizer_sect("bench_mark", {
  # three different time frames that should trigger all the code, need to remove
  # the time piece so that the tests don't fail due to variations

  capt_wo_time <- function(x) {
    txt <- capture.output(x)
    gsub("~ *-?[0-9.e+\\-]*", "~", txt)
  }
  capt_wo_time(bench_mark(Sys.sleep(1.2), times=1))
  capt_wo_time(bench_mark(Sys.sleep(.01), times=10))
  capt_wo_time(bench_mark(1 + 1, NULL, times=100))
})
unitizer_sect("sort pair lists", {
  vetr:::list_as_sorted_vec(pairlist(c=1, a=list(), b=NULL))
  # # equal names not stable, but we should never hit this with attribute lists
  # vetr:::list_as_sorted_vec(pairlist(a=1, a=list(), a=NULL))
  vetr:::list_as_sorted_vec(pairlist(b=1, 2, a=3))
  vetr:::list_as_sorted_vec(pairlist())
  vetr:::list_as_sorted_vec(pairlist(a=1))
})
