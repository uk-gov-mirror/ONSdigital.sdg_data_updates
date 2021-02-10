library(roxygen2)

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

#' Get all charcters following a dot
#'
#' Get all charcters following the last dot in a character string
#'
#' @param variable Character string.
#'
#' @return Character string.
#'
#' @examples
#' get_characters_after_dot("ab.cde")
#' get_characters_after_dot("ab.cde.fgh")
#'
#' @export
get_characters_after_dot <- function(string) {sub(".*[.]", "", string)}


#' not in
#'
#' Opposite of %in%
#'
#' @examples
#' get_characters_after_dot("ab.cde")
#' get_characters_after_dot("ab.cde.fgh")
#'
#' @export
`%not_in%` <- Negate(`%in%`)

