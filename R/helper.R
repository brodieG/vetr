# Copyright (C) 2020 Brodie Gaslam
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

# This file contains interfaces to internal C functions for use with R based
# unit testing frameworks

## Manipulate Dot Only Names
##
## Used to support internal dot substitution and escaping
##
## @keywords internal
## @param symb an R symbol
## @param arg_name another R symbol
## @param mode 1L or 2L
## @return symbol

name_sub <- function(symb, arg_name)
  .Call(VALC_name_sub, symb, arg_name)

## Expand variable names to experessions if they contain language
##
## @keywords internal

symb_sub <- function(symb, env=parent.frame())
  .Call(VALC_symb_sub, symb, env)


## Parse Expressions For \code{`validate`} Use
##
## Takes expressions provided to \code{`\link{validate}`} and identifies which
## ones are templates, which ones are normal expressions, and substitutes the
## actual argument name for \code{`.`}.
##
## Internal function exposed for unit testing purposes
##
## @keywords internal
## @param symb an R symbol
## @param arg_name another R symbol
## @param an environment to look for language expressions to substitute
## @return list

parse_validator <- function(lang, arg_name, rho=parent.frame())
  .Call(VALC_parse, lang, arg_name, rho)

## Remove Parens and \code{`.(`} From Calls
##
## @keywords internal

remove_parens <- function(lang)
  .Call(VALC_remove_parens, lang)

## Evaluates a test
##
## For unit testing
##
## @keywords internal

eval_check <- function(lang, arg_name, arg_value, env=parent.frame())
  .Call(
    VALC_eval_check, lang, arg_name, arg_name, arg_value, sys.call(), env
  )

## Internal version of `all`
##
## @keywords internal

val_all <- function(x) .Call(VALC_all, x)



