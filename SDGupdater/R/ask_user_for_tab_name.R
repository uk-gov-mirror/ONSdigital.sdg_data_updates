#' Get tab name
#'
#' Ask the user if the default table name is correct for a particular set of data. If the answer is no, the user is asked to input the correct name.
#'
#' @param disaggregation,tab_name Character string.
#'
#' @return Character string.
#'
#' @export
#'
#' @examples
#' tab_name <- ask_user_for_tab_name("Region", "Table 1")
ask_user_for_tab_name <- function (disaggregation, tab_name) {

  tab_name_query <- menu(c("Yes", "No"),
                         title = paste0("Are data for ", disaggregation, " in tab '", tab_name,
                                        "'? \nPlease type 1 for yes, or 2 for no"))

  if (tab_name_query == 2) {

    new_tab_name <- readline(prompt = paste("Please enter the exact name of the tab containing data for", disaggregation, " (without quote marks):  "))

  } else {

    new_tab_name <- tab_name

  }
}
