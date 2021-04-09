#' Get fourth from last character
#'
#' Get the fourth from last character of a string
#'
#' @param variable Character string.
#'
#' @return Character.
#'
#' @examples
#' get_fourth_from_last_character("abcde")
#'
#' @export
get_fourth_from_last_character <- function(variable) {
  substr(variable, nchar(variable) - 3, nchar(variable) - 3)
}