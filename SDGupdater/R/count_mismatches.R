#' Count mismatches between numbers
#'
#' Counts the number of mismatches between two number vectors of the same length.
#'
#' @param number_var_1,number_var_2 Numeric vector - both number_var vectors should be of the same length.
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

  data_frame <- data.frame(var1 = number_var_1, var2 = number_var_2)
  no_NAs <- na.omit(data_frame)
  comparison <- ifelse(no_NAs$number_var_1 == no_NAs$number_var_2, 0, 1)
  mismatch_count <- sum(comparison, na.rm = TRUE)
  return(mismatch_count)

}
