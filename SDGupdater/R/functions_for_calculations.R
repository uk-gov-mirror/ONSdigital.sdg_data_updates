#' Count mismatches between numbers
#'
#' Counts the number of mismatches between two number vectors.
#'
#' @param number_var_1,number_var_2 Numeric vector.
#'
#' @return Number of mismatches.
#'
#' @examples
#' correct_number <- c(1:4)
#' calculated_number <- c(1:3,5)
#' count_mismatches(correct_number, calculated_number)
#'
#' correct_char <- letters[1:4]
#' created_char <- c(letters[1:3], "e")
#' count_mismatches(correct_char, created_char)
#'
#' @export
count_mismatches <- function(number_var_1, number_var_2)  {

  comparison <- ifelse(number_var_1 == number_var_2, 0, 1)
  mismatch_count <- sum(comparison, na.rm = TRUE)
  return(mismatch_count)

}


#' Count number of decimal places
#'
#' Counts the maximum number of decimal places used for a numeric variable as
#' calculated using two different methods. As both methods fail when the number
#' of significant figures are large (but fail differently), this function
#' compares the two and if they return different values a warning is given.
#'
#' count_decimal_places_numerically counts the maximum number of decimal places
#' used for a numeric variable. This should be used with caution as 'R stores
#' numbers internally in a binary representation and the exact conversion of
#' this representation to a decimal number may not match the exact value of the
#' decimal representation of the original number'.
#'
#' count_decimal_places_as_string counts the maximum number of decimal places
#' used for a numeric variable after converting it to a character string. This
#' should be used with caution as the way R converts large numbers or numbers
#' with lots of decimal places can cause the incorrect value to be returned.
#'
#' @seealso See thread:
#'   https://stat.ethz.ch/pipermail/r-help/2012-July/317676.html
#'
#' @param value Numeric vector.
#'
#' @return Numeric value.
#'
#' @examples
#' test_dat <- c(1.1, 1.234)
#' count_max_decimal_places(test_dat) # 3dp
#' count_decimal_places_numerically(test_dat) # returns correct values
#' count_decimal_places_as_string(test_dat) # returns correct values
#'
#' test_dat2 <- c(12345678891011.234)
#' count_max_decimal_places(test_dat2) # character count gives 1dp, while numeric count correctly gives 3dp
#' count_decimal_places_numerically(test_dat2) # returns correct values
#' count_decimal_places_as_string(test_dat2) # returns 1 because as.character() truncates number.
#'
#' test_dat3 <- c(12.12345678901234567)
#' count_max_decimal_places(test_dat3) # character count gives 13, numeric count gives 15. Both are incorrect.
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
count_max_decimal_places <- function (value) {

  character_count <- count_decimal_places_as_string(value)

  numeric_count <- c()

  for (i in 1:length(value)) {
    numeric_count[i] <- count_decimal_places_numerically(value[i])
  }

  counts_match <- ifelse(sum(numeric_count == character_count) == length(value), TRUE, FALSE)

  if (counts_match == FALSE) {
    warning("calculations of number of decimal places disagree between methods. Please check number of decimal places given for calculations are as expected")
  }

  max_count <- max(numeric_count, character_count)
  return(max_count)

}

#' @rdname count_max_decimal_places
#' @export
count_decimal_places_numerically <- function(value) {

  min(which(value*10^(0:20) == floor(value*10^(0:20)))) - 1

}

#' @rdname count_max_decimal_places
#' @export
count_decimal_places_as_string <- function(value) {

  value_as_character <- as.character(value) # print(value, 22) returns a larger number of decimal places but behaves strangely after about 17dp
  nchar(get_characters_after_dot(value_as_character))

}

#' Rates per 1000
#'
#' Calculates the rate per 1000, when the numerator is greater than 3.
#'
#' @param numerator,denominator,decimal_places Numeric vector.
#'
#' @return Numeric vector.
#'
#' @examples
#' calculate_valid_rates_per_1000(13, 11000, 2)
#' calculate_valid_rates_per_1000(3, 11000, 2)
#'
#' @export
calculate_valid_rates_per_1000 <- function (numerator, denominator, decimal_places) {

  ifelse(numerator > 3,
         round(numerator / (denominator / 1000), decimal_places),
         NA)

}
