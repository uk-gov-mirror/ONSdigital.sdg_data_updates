#' Get all charcters following a dot
#'
#' Get all charcters following the last period in a character string
#'
#' @param variable Character string.
#'
#' @return Character string. If there was no period, returns a blank ("")
#'
#' @examples
#' get_characters_after_dot("ab.cde")
#' get_characters_after_dot("ab.cde.fgh")
#'
#' @export
get_characters_after_dot <- function(string) {
  
  ifelse(grepl("\\.", string) == TRUE,
         sub(".*[.]", "", string),
         "")
}