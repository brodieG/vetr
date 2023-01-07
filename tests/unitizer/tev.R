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

unitizer_sect("tev", {
  tev(runif(2), numeric(2))
  tev(runif(3), numeric(2))

  # # we can no longer do this without including magrittr in suggests
  # has.magrittr <- suppressWarnings(require(magrittr, quietly=TRUE))

  # if(has.magrittr) runif(2) %>% tev(numeric(2)) %>% isTRUE else TRUE
  # if(has.magrittr) runif(3) %>% tev(numeric(2)) %>% isTRUE else FALSE
})
