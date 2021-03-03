#' Unique entries to string
#'
#' Turns a vector that hold only character strings into a single character
#' string, with values separated by separated by ", ". NAs are ignored.
#'
#' @param variable Vector of character strings.
#'
#' @return Single character string of unique entries, not including NAs.
#'
#' @examples
#' Year <- c("2018", "2015, 2016", NA)
#' Country <- c(NA, NA, "United Kingdom")
#' unique_to_string(Year)
#' unique_to_string(Country)
#'
#' @export
unique_to_string <- function(variable) {

  variable_without_NAs <- variable[!is.na(variable)]
  unique_entries <- unique(variable_without_NAs)
  paste(c(unique_entries), collapse = ", ")

}
