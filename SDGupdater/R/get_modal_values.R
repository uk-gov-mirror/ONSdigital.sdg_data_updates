#' Get modal values
#'
#' Finds which value occurs most frequently. If there is more than one mode, all
#' will be returned
#'
#' @seealso From thread:
#'   https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#'
#' @param values Numeric or character vector.
#'
#' @return Vector of same type as values.
#'
#' @examples
#' get_modal_values(1,2,3,1,3)
#' get_modal_values(1,1,2,3)
#' get_modal_values("a", "a", "b")
#'
#' @export
get_modal_values <- function(values) {
    unique_values <- unique(values)
    tab <- tabulate(match(values, unique_values))
    unique_values[tab == max(tab)]
}
