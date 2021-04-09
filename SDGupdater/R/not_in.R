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
