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

# this file should be excluded via .Rbuildignore

unitizer_sect("ggplot", {
  # Rather experimental; we store the ggplot objects to avoid the suggests
  df1 <- data.frame(x=runif(20), y=runif(20))
  df2 <- data.frame(x=runif(20), y=runif(20), z=rep(c("a", "b"), 10))
  df3 <- data.frame(a=runif(30), b=runif(30))

  if(
    suppressWarnings(
      suppressPackageStartupMessages(
        require(ggplot2, quietly=TRUE)
  ) ) ) {
    # one day this will break, but can't figure out right now how to get the
    # deprecation warnings consistently enough to figure out what to do about
    # them

    old.opt <- options(lifecycle_verbose_soft_deprecation=FALSE)
    on.exit(options(old.opt))
    g1 <- ggplot(df1) + geom_point(aes(x=x, y=y))
    g2 <- ggplot(df1) + geom_line(aes(x=x, y=y))
    g3 <- ggplot(df3) + geom_point(aes(x=a, y=b))
    g4 <- ggplot(df1, aes(x=x, y=y)) + geom_point() + geom_line()

    g.abs <- abstract(g1)

    list(alike(g.abs, g1), alike(g.abs, g2), alike(g.abs, g3))
  } else {
    # this is what the result should be so that this works when we skip the
    # tests for lack of ggplot2

    list(
      TRUE,
      "`class(g2$layers[[1]]$geom)[2]` should be \"GeomPoint\" (is \"GeomPath\")",
      TRUE
    )
  }
})

