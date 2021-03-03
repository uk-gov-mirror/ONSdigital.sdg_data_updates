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

  if (is.numeric(numerator) == FALSE) { stop("numerator must be numeric")  }
  if (is.numeric(denominator) == FALSE) { stop("denominator must be numeric") }
  if (is.numeric(decimal_places) == FALSE) { stop("decimal_places must be numeric") }

  if (numerator < 0 == TRUE) { stop("numerator must be greater than 0") }
  if (denominator < 0 == TRUE) { stop("denominator must be greater than 0") }
  if (decimal_places < 0 == TRUE) { stop("decimal_places must be greater than 0") }

  ifelse(numerator > 3,
         round(numerator / (denominator / 1000), decimal_places),
         NA)

}
