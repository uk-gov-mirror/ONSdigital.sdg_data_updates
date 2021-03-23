#' Count number of decimal places
#'
#' Uses two different methods for calculating the number of decimal places, and
#' returns the modal value. NA's in value are ignored. If there is more than one
#' modal value, the maximum is returned.
#'
#' For large numbers, neither counting method is reliable, so the count of these
#' numbers is given as NA. Where the two methods disagree, count is recorded as
#' NA. If NA is the modal value a default value is returned and a warning is
#' given.
#'
#' The mode is used to remove unusual counts, and therefore get the value most
#' likely to be correct.
#'
#' count_decimal_places_numerically counts the number of decimal places used for
#' a numeric variable. This should be used with caution as 'R stores numbers
#' internally in a binary representation and the exact conversion of this
#' representation to a decimal number may not match the exact value of the
#' decimal representation of the original number'.
#'
#' count_decimal_places_as_string counts the number of decimal places used for a
#' numeric variable after converting it to a character string. This should be
#' used with caution as the way R converts large numbers or numbers with lots of
#' decimal places can cause the incorrect value to be returned. Some such cases
#' that are known to be incorrect are returned as NA (e.g 1.23e+20 would return
#' NA)
#'
#' @seealso Thread on counting decimal places:
#'   https://stat.ethz.ch/pipermail/r-help/2012-July/317676.html Rationale for
#'   setting default number of decimal places to 3:
#'   https://www.bmj.com/content/350/bmj.h1845
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#'
#' @param value Numeric vector.
#' @param default The default number to be returned if the count is
#'   unsuccessful. Optional (defaults to 3).
#'
#' @return Numeric vector.
#'
#' @examples
#' test_dat <- c(1.1, 1.234)
#' count_decimal_places(test_dat) # 3dp
#' count_decimal_places_numerically(test_dat) # returns correct values
#' count_decimal_places_as_string(test_dat) # returns correct values
#'
#' test_dat2 <- c(12345678891011.234)
#' count_decimal_places(test_dat2) # character count gives 1dp, while numeric count correctly gives 3dp
#' count_decimal_places_numerically(test_dat2) # returns correct values
#' count_decimal_places_as_string(test_dat2) # returns 1 because as.character() truncates number.
#'
#' test_dat3 <- c(12.12345678901234567)
#' count_decimal_places(test_dat3) # character count gives 13, numeric count gives 15. Both are incorrect.
#' count_decimal_places_numerically(test_dat3) # returns 15, where the correct figure is 17.
#' count_decimal_places_as_string(test_dat3) # returns 13, where the correct figure is 17 because as.character() truncates number.
#'
#' test_dat4 <- c(123456789012345678.9012345678901234567890)
#' count_decimal_places_as_string(test_dat4) # returns 0 because as.character() truncates number.
#'
#' test_dat5 <- c(123456789012345678.234)
#' count_decimal_places_as_string(test_dat5) # returns 18 because of conversion to 1.234568e+18
#'
#' @export
count_decimal_places <- function (value, default = 3) {

  no_NAs <- na.omit(value)

  character_count <- count_decimal_places_as_string(no_NAs)
  numeric_count <- count_decimal_places_numerically(no_NAs)

  quality_adjusted <- data.frame(character_count = character_count, numeric_count = numeric_count) %>%
    mutate(methods_match = ifelse(numeric_count == character_count, TRUE, FALSE),
           numeric_count = ifelse(is.na(character_count), NA, numeric_count)) %>%
    mutate(reliable = ifelse(methods_match == TRUE &
                               !is.na(character_count) & !is.na(numeric_count), TRUE, FALSE))

  non_NA_counts <- filter(quality_adjusted, !is.na(numeric_count) & !is.na(character_count))

  non_NA_most_common_counts <- get_modal_values(c(non_NA_counts$numeric_count, non_NA_counts$character_count))

  if (is.logical(non_NA_most_common_counts) == FALSE) {
    max_count <- max(non_NA_most_common_counts)
  } else {
    max_count <- NA
  }

  methods_always_match <- ifelse(length(no_NAs) ==  sum(quality_adjusted$methods_match, na.rm = TRUE),
                                 TRUE, FALSE)
  suspect_counts_exist <- ifelse(sum(is.na(quality_adjusted) > 0), TRUE, FALSE)
  number_of_reliable_counts <- sum(quality_adjusted$reliable)
  fifty_percent_of_counts <- 0.5 * length(no_NAs)


  if (is.na(max_count) == TRUE) {
    warning("Unable to count decimal places, please check returned number of decimal places is acceptable and manually adjust if not.")
    max_count <- default
  } else if ((suspect_counts_exist == TRUE | methods_always_match == FALSE) &
             number_of_reliable_counts <= fifty_percent_of_counts ) {
    warning(paste0("Result based on small number of reliable counts (reliable counts = ", number_of_reliable_counts, ")" ))
  }

  return(max_count)

}

#' @rdname count_decimal_places
#' @export
count_decimal_places_numerically <- function(value) {

  if (is.numeric(value) == FALSE) { stop("value must be numeric") }

  numeric_count <- c()

  for (i in 1:length(value)) {

    if(is.na(value[i])) {
      numeric_count[i] <- NA
    } else {
      numeric_count[i] <- min(which(value[i]*10^(0:20) == floor(value[i]*10^(0:20)))) - 1
    }
  }

  return(numeric_count)
}


#' @rdname count_decimal_places
#' @export
count_decimal_places_as_string <- function(value) {

  character_count <- c()

  for (i in 1:length(value)){

    if (!is.numeric(value[i]) == TRUE) { stop("value must be numeric") }

    if (grepl("e+", value[i]) == TRUE) { value[i] <- NA } # this prevents an incorrect count due to very large numbers being read in scientific format e.g. 1.234567890123456e+20

    if (is.na(value[i])) {

      character_count[i] <- NA

    } else {

      value_as_character <- as.character(value[i]) # could alternatively use print(), but print(value, 22) returns a larger number of decimal places but behaves strangely after about 17dp
      character_count[i] <- nchar(get_characters_after_dot(value_as_character))

    }
  }

  return(character_count)
}
