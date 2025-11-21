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

unitizer_sect("remove parens", {
  vetr:::remove_parens(quote((a)))
  vetr:::remove_parens(quote(.(a)))
  vetr:::remove_parens(quote((((a)))))
  vetr:::remove_parens(quote((.((.(a))))))
  vetr:::remove_parens(quote((a) && .(a)))  # Nothing should be removed
})
unitizer_sect("parse", {
  x <- quote(.(.) && ((a)))
  vetr:::parse_validator(x, quote(arg_to_validate))
  x # make sure unchanged from previous assignment

  vetr:::parse_validator(quote(FALSE), quote(arg_to_validate))
  vetr:::parse_validator(quote(((FALSE))), quote(arg_to_validate))
  vetr:::parse_validator(quote(((FALSE && ((TRUE))))), quote(arg_to_validate))
  vetr:::parse_validator(quote(.(FALSE)), quote(arg_to_validate))
  vetr:::parse_validator(quote(.), quote(arg_to_validate))
  vetr:::parse_validator(quote(. && a), quote(arg_to_validate))
  vetr:::parse_validator(quote(.(.)), quote(arg_to_validate))
  vetr:::parse_validator(quote(((a && b) || .(.))), quote(arg_to_validate))
  vetr:::parse_validator(quote(matrix(nrow=3)), quote(arg_to_validate))
  vetr:::parse_validator(quote(matrix(nrow=3) && .(.)), quote(arg_to_validate))
  vetr:::parse_validator(quote((a || ((b && c))) && .(a + .)), quote(arg_to_validate))
  vetr:::parse_validator(quote((a || ((b && .(c)))) && (a + .(.))), quote(arg_to_validate))

  vetr:::parse_validator(quote(a && (b + .(c))), quote(arg_to_validate))  # uninterpretable?
  vetr:::parse_validator(quote(a && .), "hello")                          # uninterpretable?
} )
unitizer_sect("token sub", {
  vetr:::symb_sub(INT.1)
  vetr:::symb_sub(NO.NA)

  # Dot unescaping
  `..` <- quote(yes)
  `.zzz` <- `zzz.` <- quote(yup)
  `.` <- quote(...)
  vetr:::symb_sub(quote(..))
  vetr:::symb_sub(quote(...))
  vetr:::symb_sub(quote(.zzz))
  vetr:::symb_sub(quote(zzz.))

  # Errors
  `.` <- quote(..)
  vetr:::symb_sub(quote(..))
  vetr:::symb_sub(quote(.))

  # Identity operations on non-symbols
  vetr:::symb_sub(quote(.(zzz)))
  vetr:::symb_sub("hello")
})

unitizer_sect("preset tokens", {
  x <- quote(integer(1L))
  y <- quote(integer(1L) || NULL)
  z <- quote(integer(1L) && .(!any(is.na(.))))
  vetr:::parse_validator(quote(x), quote(w))
  vetr:::parse_validator(quote(y), quote(w))
  vetr:::parse_validator(quote(z), quote(w))
  vetr:::parse_validator(quote(z || NULL), quote(w))
} )
unitizer_sect("validators", {
  vetr:::parse_validator(INT.1, quote(w))
  vetr:::parse_validator(INT, quote(w))
  vetr:::parse_validator(CHR.1, quote(w))
  vetr:::parse_validator(CHR, quote(w))
  vetr:::parse_validator(NUM.1, quote(w))
  vetr:::parse_validator(NUM, quote(w))
  vetr:::parse_validator(LGL.1, quote(w))
  vetr:::parse_validator(LGL, quote(w))
  vetr:::parse_validator(CPX.1, quote(w))
  vetr:::parse_validator(CPX, quote(w))
} )
