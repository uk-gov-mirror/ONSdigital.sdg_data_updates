#' @title Remove blanks and cells above the column headings
#'
#' @description Remove blanks and cells above the column headings to clean
#'   xlsx_cells data. Do this before using behead()
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
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
#' test_dat <- dplyr::tibble(row = c(1:6), character = c("2017", NA, "England",
#' "Rates", NA, NA), is_blank = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
#' data_type = c("character", "blank", "character", "character", "blank",
#' "numeric"), numeric = c(NA, NA, NA, NA, NA, 20))
#' remove_blanks_and_info_cells(test_dat, 4)
remove_blanks_and_info_cells <- function(dat, first_header_row) {

  dat %>%
    mutate(is_blank = ifelse(character == "" & data_type == "character", TRUE, is_blank)) %>%
    filter(is_blank == FALSE & row %not_in% 1:(first_header_row - 1))

}
