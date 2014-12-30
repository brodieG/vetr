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

name_sub <- function(symb, arg_name)
  .Call(VALC_name_sub, symb, arg_name)

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

#' Remove Parens and \code{`.(`} From Calls
#'
#' @keywords internal

remove_parens <- function(lang)
  .Call(VALC_remove_parens, lang)

#' Evaluates a test
#'
#' For unit testing
#'
#' @keywords internal

eval_check <- function(lang, arg_name, arg_value)
  .Call(VALC_eval_check, lang, arg_name, arg_value, sys.call(), parent.frame())

valtest <- function(a, b)
  .Call(VALC_test, a, sys.frame(sys.nframe()))

valtest1 <- function(a){
  .Call(VALC_test1, a)
}
valtest2 <- function(a) {
  .Call(VALC_test2, a, sys.frame(sys.nframe()))
}
valtest3 <- function(a) {
  .Call(VALC_test2, a, parent.frame())
}
