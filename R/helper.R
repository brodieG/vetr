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
