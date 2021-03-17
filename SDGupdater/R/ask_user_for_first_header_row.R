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

#' Get tab name
#'
#' Ask the user if the default table name is correct for a particular set of data. If the answer is no, the user is asked to input the correct name.
#'
#' @param disaggregation,tab_name Character string.
#' 
#' @return Character string.
#'
#' @export
#' 
#' @examples
#' tab_name <- ask_user_for_tab_name("Region", "Table 1")
ask_user_for_tab_name <- function (disaggregation, tab_name) {
  
  tab_name_query <- menu(c("Yes", "No"), 
                             title = paste0("Are data for ", disaggregation, " in tab '", tab_name,
                                            "'? \nPlease type 1 for yes, or 2 for no"))
  
  if (tab_name_query == 2) {
    
    new_tab_name <- readline(prompt = paste("Please enter the exact name of the tab containing data for", disaggregation, " (without quote marks):  "))
  
    } else { 
    
    new_tab_name <- tab_name
    
    }
}


#' Get first header row
#'
#' Ask the user if the default first header row is correct for a particular set
#' of data. If the answer is no, the user is asked to input the correct name.
#'
#' @param tab_name Character string.
#' @param row_numebr Numeric.
#' 
#' @return Character string.
#'
#' @export
#'
#' @examples
#' tab_name <- ask_user_for_first_header_row("Table 1", 4)
ask_user_for_first_header_row <- function (tab_name, row_number) {
  
  row_number_query <-  menu(c("Yes", "No"), 
                            title = paste0("Is the first row of column headings in tab '", tab_name, "' in row ", row_number, 
                                           "? \nPlease type 1 for yes, or 2 for no"))

  if (row_number_query == 2) {
    
    new_row_number <- readline(prompt = paste("Please enter the number of the first row of column headings for tab", tab_name, " (without quote marks):  "))
    
  } else { 
    new_row_number <- row_number
  }
  
  as.numeric(new_row_number)
}