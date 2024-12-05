#' @title Check Missing Values in each column of the daily climate dataset
#' @description This function checks whether any missing values (NA) are present in the input climate dataset.
#'  Before doing so, users should ensures that all years are in chronological order, contain either 365 or 366 (leap year) days, and start on January 1st, using the function `check_years_days_and_start`
#'  If missing values are found, it prints a summary of the number of missing values per variable.
#'  If no missing values are detected, it prints a confirmation message indicating that no missing values were found.
#' @param data A daily climate data frame that will be checked for missing values (NA).
#' @return This function does not return a value. It prints either a summary of missing values
#' per variable if missing values are detected, or a message indicating that no missing values are found.
#'
#' @examples
#' # Check missing values in 'tab_clim' dataset
#' check_missing_values(tab_clim)
#'
#'

check_missing_values <- function(data) {

  # Check if any missing values (NA) are present
  any_missing <- any(is.na(data))

  if (any_missing) {
    # Create a summary of missing values per variable
    missing_summary <- data.frame(
      variable = names(data),
      missing_values = colSums(is.na(data))
    )
    warning("Missing values are detected")
    message(missing_summary)
  } else {
    message("No missing values (NA) detected")
  }
}
