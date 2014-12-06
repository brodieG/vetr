#' Retrieve name objects from a call
#' 
#' @param language R language object
#' @return list of names contained in the the language object
#' @export
#' @examples
#' getNames(quote(log10(589) + a + b + c + 10))

getNames <- function(language) {
  getNamesRecurse <- function(element) {
    if(!is.language(element)) {
      NULL
    } else if(is.name(element)) {
      names <<- c(names, list(element))
      NULL
    } else {
      lapply(element, getNamesRecurse)
    }
  }
  names <- list()
  getNamesRecurse(language)
  names
}