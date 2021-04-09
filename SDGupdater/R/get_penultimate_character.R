#' Get penultimate character
#'
#' Get the second to last character of a string
#'
#' @param variable Character string.
#'
#' @return Character.
#'
#' @examples
#' get_penultimate_character("abcde")
#'
#' @export
get_penultimate_character <- function(variable) {
  substr(variable, nchar(variable) - 1, nchar(variable) - 1)
}
