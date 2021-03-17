#' Turn a list of character vectors/matrices into a vector of character strings
#'
#' Turns a list of vectors/matrices that hold only character strings into a
#' single vector of character strings, separated by ", ". The elements that make
#' up each component of the input list are pasted into a single string. Each
#' element of the new vector is formed from one component of the input list.
#'
#' @param input_list List of character vectors or matrices.
#'
#' @return Vector with one string for each component of the list. If the
#'   input_list is a matrix, the order is first down rows then across columns.
#'
#' @examples
#' matrix_1 <- as.matrix(c("England", "UK"))
#' matrix_2 <- as.matrix(c("Wales"))
#' matrix_3 <- as.matrix(c("NA"))
#' matrix_list <- list(matrix_1, matrix_2, matrix_3)
#' list_into_vector_of_strings(matrix_list)
#'
#' @export
list_into_vector_of_strings <- function (input_list) {

  vector_of_strings <- c()

  for (i in 1:length(input_list)) {

    vector_of_strings[i] <- paste(unlist(input_list[[i]]), collapse = ", ")

  }

  ifelse(vector_of_strings == "NA", NA, vector_of_strings)

}
