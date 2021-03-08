#' @title Format region names
#'
#' @description Corrects the capitalisation of region names
#'
#' @param variable Character vector of region names.
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
                   variable == "EAST OF ENGLAND" ~ "East of England",
                   variable == "Yorkshire and the Humber" ~ "Yorkshire and the Humber",
                   variable == "East of England" ~ "East of England",
                   TRUE ~ stringr::str_to_title(variable))
}
