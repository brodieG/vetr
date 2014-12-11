# This file contains interfaces to internal C functions for use with R based
# unit testing frameworks

#' Manipulate Dot Only Names
#'
#' Used to support internal dot substitution and escaping
#'
#' @keywords internal
#' @param symb an R symbol
#' @param arg_name another R symbol
#' @param mode 1L or 2L
#' @return symbol

name_sub <- function(symb, arg_name, mode)
  .Call(VALC_name_sub, symb, arg_name, mode)

#' Parse Expressions For \code{`validate`} Use
#'
#' Takes expressions provided to \code{`\link{validate}`} and identifies which
#' ones are templates, which ones are normal expressions, and substitutes the
#' actual argument name for \code{`.`}.
#'
#' Internal function exposed for unit testing purposes
#'
#' @keywords internal
#' @param symb an R symbol
#' @param arg_name another R symbol
#' @return list

parse_validator <- function(lang, arg_name)
  .Call(VALC_parse, lang, arg_name)
