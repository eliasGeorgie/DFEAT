
#' This function checks if the provided input daily climate dataset contains the required columns and prints
#' appropriate messages. It also reports any extra columns present in the input dataset.
#'
#' @title Check and Print Column Names in the Daily Climate Dataset
#' @description This function checks if all the required columns (e.g., 'year', 'month',
#' 'day', 'prec', 'Tmax') are present in the input daily climate dataset. It also identifies any extra
#' columns that may be in the dataset. The function prints warnings if any required
#' columns are missing and prints messages for extra columns found. It returns a
#' message indicating if the dataset has the correct columns or is missing any required columns.
#'
#' @param data A data frame containing the daily climate dataset to be checked. The dataset should
#' include specific columns like 'year', 'month', 'day', 'prec', 'Tmax', but additional columns
#' are also allowed.
#' @param required_cols A vector of character strings representing the names of the
#' required columns. Default is `c('year', 'month', 'day', 'prec', 'Tmax')`.
#'
#' @return This function does not return a value but prints messages to the console.
#' If all required columns are present, it prints "Dataset has the correct columns"
#' If any required columns are missing, it prints "Dataset is missing some required columns"
#' It also prints any extra columns found in the dataset.
#'
#' @examples
#' # Assuming 'tab_clim' is the daily input climate dataset
#' check_columns_names(tab_clim)
#'

check_columns_names <- function(data, required_cols = c('year', 'month', 'day', 'prec', 'Tmax')) {

  # Get the actual column names from the data
  actual_cols <- tolower(colnames(data))  # Convert column names to lowercase for case-insensitivity

  # Convert the required columns to lowercase as well
  required_cols <- tolower(required_cols)

  # Check if all required columns are present in the dataset
  missing_cols <- setdiff(required_cols, actual_cols)
  extra_cols <- setdiff(actual_cols, required_cols)

  # Report missing columns
  if (length(missing_cols) > 0) {
    warning("The following required columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # Report extra columns
  if (length(extra_cols) > 0) {
    message("Extra columns found: ", paste(extra_cols, collapse = ", "))
  }

  # Final check and print outcome
  if (length(missing_cols) == 0) {
    message("Dataset has the correct columns")
  } else {
    message("Dataset is missing some required columns")
  }
}


