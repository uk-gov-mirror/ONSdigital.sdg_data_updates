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
#' first_header_row <- 4 test_dat <- data.frame(row = c(1:5), character =
#' c("data for 2017 ", "England and", "this is Wales", "impala", "elephant"))
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
