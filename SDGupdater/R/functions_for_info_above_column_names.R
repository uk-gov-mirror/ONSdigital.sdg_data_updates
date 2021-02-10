#' Isolate cells above column headings
#'
#' Gets year and country info from the rows above the column headings in a
#' dataframe created using xlsx_cells
#'
#' Assumes that the cell containing the year ends with the year
#'
#' @importFrom dplyr %>% filter distinct mutate
#'
#' @param dat Dataframe imported using xlsx_cells where there are header cells
#'   above the column names containing info on date and year.
#' @param first_header_row Numeric value. The row number in the excel file
#'   containing the first set of column headings.
#'
#' @return Tibble containing all year and country info.
#'
#' @examples
#' first_header_row <- 4
#' test_dat <- data.frame(row = c(1:5), character = c("data for 2017 ", "England and", "this is Wales", "impala", "elephant"))
#' get_info_cells(test_dat, first_header_row)
#'
#' @export
get_info_cells <- function(dat, first_header_row) {


  dat %>%
    filter(row %in% 1:(first_header_row - 1)) %>%
    distinct(character) %>%
    filter(!is.na(character)) %>%
    mutate(character = trimws(character,  which = "both")) %>%
    mutate(Year = get_all_years(character)) %>%
    mutate(Country = get_all_country_names(character))

}

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
#' test_dat <- c("England is a country of the UK", "this does not contain a country name")
#' get_all_country_names(test_dat)
#'
#' @export
get_all_country_names <- function (variable) {

  possible_countries_vector <- c("England\ and\ Wales", "England",  "Wales", "Scotland", "Northern\ Ireland", "UK", "United\ Kingdom")
  possible_countries <- paste(possible_countries_vector, collapse = "|")

  list_of_countries <- ifelse(str_detect(variable, possible_countries),
                              str_extract_all(variable, possible_countries), NA)

  list_into_vector_of_strings(list_of_countries)

}

#' Extract years from strings
#'
#' Identifies whether a string contains a number that looks like a year from the
#' 20th or 21st century. If it does, it returns the year as a string, otherwise
#' it returns NA. If it contains more than one year, all years are returned as a
#' comma separated string
#'
#' @importFrom stringr str_detect str_extract_all
#'
#' @param variable Character vector.
#'
#' @return Character vector giving year or NA.
#'
#' @examples
#' test_dat <- c("a date: 2005", "1995 6", "1993 end", "a1995", "1995a", "a date:1995, another date: 2012",
#'               "not a date: 475", "also not a date: 1234", "abc 12345",  "34200943", "190932")#' get_all_country_names(test_dat)
#' get_all_years(test_dat)
#'
#' @export
get_all_years <- function (variable) {

  not_preceded_by_number <- "(?<![0-9])"
  four_digits_starting_19_or_20 <- "(19|20)\\d{2}"
  not_followed_by_number <- "(?![0-9])"
  year_pattern <- paste(c(not_preceded_by_number, four_digits_starting_19_or_20, not_followed_by_number), collapse = "")


  list_of_years <- ifelse(str_detect(variable, year_pattern),
                          str_extract_all(variable, year_pattern), NA)

  list_into_vector_of_strings(list_of_years)

}

#' Turn a list of character vectors/matrices into a vector of character strings
#'
#' Turns a list of vectors/matrices that hold only character strings into a
#' single vector of character strings, separated by ", ". The elements that make
#' up each component of the input list are pasted into a single string. Each
#' element of the new vector is formed from one component of the input list.
#'
#' @param input_list List of character vectors or matrices.
#'
#' @return Vector with one string for each component of the list. If the
#'   input_list is a matrix, the order is first down rows then across columns.
#'
#' @examples
#' matrix_1 <- as.matrix(c("England", "UK"))
#' matrix_2 <- as.matrix(c("Wales"))
#' matrix_3 <- as.matrix(c("NA"))
#' matrix_list <- list(matrix_1, matrix_2, matrix_3)
#' list_into_vector_of_strings(matrix_list)
#'
#' @export
list_into_vector_of_strings <- function (input_list) {

  vector_of_strings <- c()

  for (i in 1:length(input_list)) {

    vector_of_strings[i] <- paste(unlist(input_list[[i]]), collapse = ", ")

  }

  ifelse(vector_of_strings == "NA", NA, vector_of_strings)

}

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
