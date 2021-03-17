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

  if (length(numerator) != length(denominator)) { stop("numerator and denominator vectors must be of the same length")  }

  if (is.numeric(numerator) == FALSE) { stop("numerator must be numeric") }
  if (is.numeric(denominator) == FALSE) { stop("denominator must be numeric") }
  if (is.numeric(decimal_places) == FALSE) { stop("decimal_places must be numeric") }

  if (numerator < 0) { stop("numerator must be greater than 0") }
  if (denominator < 0) { stop("denominator must be greater than 0") }
  if (decimal_places < 0) { stop("decimal_places must be greater than 0") }

  if (numerator > denominator) { stop("numerator must be smaller than or equal to denominator") }

  ifelse(numerator > 3,
         round(numerator / (denominator / 1000), decimal_places),
         NA)

}
