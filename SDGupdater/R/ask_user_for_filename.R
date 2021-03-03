#' Get filename
#'
#' Ask the user to select the file they want to work with. All files in the current directory are given as options.
#'
#' @param file_path Character string. If left blank, the working directory is used.
#'
#' @return Character string.
#'
#' @export
#'
#' @examples
#' ask_user_for_filename()
ask_user_for_filename <- function (file_path = "."){

  input_files <- list.files(file_path)

  input_file_selection <- menu(input_files,
                               title = paste("Please select the relevant number for the file containing the data you want"))
  filename <- input_files[input_file_selection]
}
