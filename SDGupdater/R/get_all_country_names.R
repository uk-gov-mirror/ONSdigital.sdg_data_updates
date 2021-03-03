#' Extract country from string containing country name
#'
#' Identifies whether a string contains the name of one or more of the UK
#' countries and if it does, it gives the names of that country as a string,
#' otherwise it returns NA.
#'
#' @param variable Character vector you want to check to see if it contains the
#'   name of a UK country.
#'
#' @return Character vector giving name of country or NA.
#'
#' @examples
#' test_dat <- c("England is a country of the UK", "this does not contain a
#' country name") get_all_country_names(test_dat)
#'
#' @export
get_all_country_names <- function (variable) {

  possible_countries_vector <- c("England\ and\ Wales", "England",  "Wales", "Scotland", "Northern\ Ireland", "UK", "United\ Kingdom")
  possible_countries <- paste(possible_countries_vector, collapse = "|")

  list_of_countries <- ifelse(str_detect(variable, possible_countries),
                              str_extract_all(variable, possible_countries), NA)

  list_into_vector_of_strings(list_of_countries)

}

#' Warning for when multiple countries have been identified by get_all_country_names()
#'
#' @param filename string giving name of the file
#' @param tab string giving name of the tab
#' @param description string describing the data that is affected (e.g. which disaggregation)#'
#'
#' @return a warning
#'
#' @describeIn get_all_country_names Extract country from string containing country name
#'
#' @examples
#' multiple_country_warning("foo.xlsx", "Tab 1", "sex_by_age disaggregation")
#'
#' @export
multiple_country_warning <- function (filename, tab, description) {

  if (grepl(",", country) == TRUE) {
    warning(paste("More than one country identified",
                  filename, tab, "where only one was expected.
  \nTO DO: Please check that Country is correct in the output for", description))
  }
}
