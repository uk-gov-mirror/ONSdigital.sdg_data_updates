#' wrong tab warning based on key words
#'
#' Message for when expected key words are not found
#'
#' @param tab_name String
#' @param desired_data String describing desired data (e.g. table description).
#'
#' @return String
#'
#' @export
#'
#' @examples
#' warning(wrong_tab_message("Table 2", "territorial greenhouse gas emissions by source sector"))

wrong_tab_message <- function (tab_name, desired_data) {

paste("It looks like", tab_name, "might not give", desired_data, "data. (key words were not found).
Please check that the correct data have been used for this disaggregation.
If not, please find the correct tab name and rerun the code.")

}
