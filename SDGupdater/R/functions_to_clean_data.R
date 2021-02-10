#' Remove superscripts
#'
#' Remove superscripts, whether they are read as actual superscripts or just as numbers.
#'
#' @param variable Character vector you want superscripts removed from.
#'
#' @return Character vector.
#'
#' @export
#'
#' @examples
#' character <- c("Wales1","Rates²", "Numbers1,2")
#' remove_superscripts(character)
#' remove_real_superscripts(character)
#' remove_double_superscripts(character)
#' remove_false_superscripts(character)

remove_superscripts <- function(variable) {

  no_real_superscripts <- remove_real_superscripts(variable)
  no_double_superscripts <- remove_double_superscripts(no_real_superscripts)

  remove_false_superscripts(no_double_superscripts)

}

#' @rdname remove_superscripts
#'
#' @export
remove_real_superscripts <- function (variable) {

  superscript_regex_codes <- "[\u{2070}\u{00B9}\u{00B2}\u{00B3}\u{2074}-\u{2079}]"

  gsub(superscript_regex_codes, '', variable)

}

#' @rdname remove_superscripts
#' @export
remove_double_superscripts <- function (variable) {

  double_superscript_at_end <- "[0-9],[0-9]$"

  ifelse(could_contain_superscript(variable) == TRUE,
         gsub(double_superscript_at_end, "", variable),
         variable)

}

#' @rdname remove_superscripts
#' @export
remove_false_superscripts <- function (variable) {

  ifelse(could_contain_superscript(variable) == TRUE, gsub("[1-9]$", '', variable), variable)

}

#' @rdname remove_superscripts
#' @export
could_contain_superscript <- function (variable) {

  # We dont want to accidentally identify a number that is not a superscript as a superscript.
  # e.g. we dont want to truncate a number, or a code that ends in a number (e.g. an area code) because that number was misidentified as a superscript
  #
  # If first TWO characters are 'letters', this identifies the string as a string not a number
  # (If we only required the FIRST character to be a letter, Area code numbers would be affected)
  # In addition, superscripts are unlikely to be preceded by a number or a space

  ifelse(substr(variable, 1, 1) %in% c(LETTERS, letters) &
           substr(variable, 2, 2) %in% c(LETTERS, letters, " ") &
           get_penultimate_character(variable) %not_in% c(" ", 1:9),
         TRUE, FALSE)

}

#' Remove blanks and cells above the column headings
#'
#' Remove blanks and cells above the column headings to clean xlsx_cells data.
#' Do this before using behead()
#'
#' @importFrom dplyr %>% mutate filter
#'
#' @param dat Tibble originaly imported using xlsx_cells.
#' @param first_header_row Numeric. Row number of first row in the xlsx file
#'   containing column names.
#'
#' @return Tibble.
#'
#' @export
#'
#' @examples
#' test_dat <- dplyr::tibble(row = c(1:6),
#'                    character = c("2017", NA, "England", "Rates", NA, NA),
#'                    is_blank = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
#'                    data_type = c("character", "blank", "character", "character", "blank", "numeric"),
#'                    numeric = c(NA, NA, NA, NA, NA, 20))
#' remove_blanks_and_info_cells(test_dat, 4)
remove_blanks_and_info_cells <- function(dat, first_header_row) {

  dat %>%
    mutate(is_blank = ifelse(character == "" & data_type == "character", TRUE, is_blank)) %>%
    filter(is_blank == FALSE & row %not_in% 1:(first_header_row - 1))

}

#' Format region names
#'
#' Corrects the capitalisation of region names
#'
#' @param variable Character vector of region names.
#'
#' @return Character vector. Function overwrites region variable with correct
#'   capitalisation.
#' @export
#'
#' @examples
# test_dat <- tibble(Region = c("YORKSHIRE AND THE HUMBER", "EAST OF ENGLAND", "The shire"))
# test_dat %>% mutate(Region = format_region_names(Region))
#'
#' test_var <- c("YORKSHIRE AND THE HUMBER", "EAST OF ENGLAND", "The shire")
#' Region <- format_region_names(test_var)
format_region_names <- function(variable) {
  dplyr::case_when(variable == "YORKSHIRE AND THE HUMBER" ~ "Yorkshire and the Humber",
                   variable == "EAST OF ENGLAND" ~ "East of England",
                   variable == "Yorkshire and the Humber" ~ "Yorkshire and the Humber",
                   variable == "East of England" ~ "East of England",
                   TRUE ~ stringr::str_to_title(variable))
}
