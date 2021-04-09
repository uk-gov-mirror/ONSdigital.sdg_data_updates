#' @title Format region names
#'
#' @description Corrects the capitalisation of region names, and corrects East
#'   of England to East (which is standard for charts and tables).
#'
#' @param variable Character vector of region names
#'
#' @return Character vector. Function overwrites region variable with correct
#'   capitalisation.
#' @export
#'
#' @examples
#' test_dat <- tibble(Region = c("YORKSHIRE AND THE HUMBER", "EAST OF ENGLAND", "The shire"))
#' test_dat %>% mutate(Region = format_region_names(Region))
#'
#' test_var <- c("YORKSHIRE AND THE HUMBER", "EAST OF ENGLAND", "The shire")
#' Region <- format_region_names(test_var)
format_region_names <- function(variable) {
  dplyr::case_when(variable == "YORKSHIRE AND THE HUMBER" ~ "Yorkshire and the Humber",
                   variable == "EAST OF ENGLAND" ~ "East",
                   variable == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
                   variable == "East of England" ~ "East",
                   TRUE ~ stringr::str_to_title(variable))
}
