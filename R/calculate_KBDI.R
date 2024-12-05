#' @title Calculate KBDI (Mediterranean Version) for Drought Monitoring (Ganatsas et al., 2011)
#' @description This function calculates the KBDI (Keetch-Byram Drought Index) for Mediterranean conditions, based on
#' daily precipitation (`prec`) and maximum temperature (`Tmax`) data. The KBDI is calculated using a time series
#' of daily data, and an optional argument allows saving the output to a CSV file.
#' It is highly recommended that users verify the input climate dataset with the provided checking functions `check_years_days_and_start` and `check_missing_values` before running the KBDI calculation.
#' This ensures that all days within each year are present with no missing values (NA), the years are in chronological order, and each year begins on January 1st.
#' @param data A daily climate data frame containing the input data.
#' It must include columns for 'year', 'month', 'day', daily precipitation, and daily maximum temperature.
#' The default columns are "prec" for precipitation and "Tmax" for maximum temperature, but these can be specified using `prec_col` and `temp_col` arguments.
#' @param prec_col A string specifying the name of the precipitation column. Default is "prec".
#' @param temp_col A string specifying the name of the temperature column. Default is "Tmax".
#' @param FC A numeric value representing the field capacity (in mm). Default is 200.
#' @param save_to_csv A logical value (`TRUE` or `FALSE`). If `TRUE`, the function will save the results to a CSV file. Default is `FALSE`.
#' @param file_name A string specifying the name of the CSV file to save the results if `save_to_csv` is `TRUE`. Default is "KBDI_output.csv".
#'
#' @return A data frame containing the original `year`, `month`, and `day` columns from the input data, along with the calculated `DF` (drying factor) and `KBDI` values for each day.
#' If `save_to_csv` is `TRUE`, the results will also be saved as a CSV file.
#'
#' @examples
#' # Calculate KBDI for a dataset 'tab_clim' with columns 'prec' and 'Tmax'
#' result_kbdi <- calculate_KBDI(tab_clim)
#'
#' # Calculate KBDI and save the results as a CSV file
#' result_kbdi_with_csv_saved <- calculate_KBDI(tab_clim, save_to_csv = TRUE, file_name = "KBDI_output.csv")
#'

calculate_KBDI <- function(data, prec_col = "prec", temp_col = "Tmax", FC = 200, save_to_csv = FALSE, file_name = "KBDI_output.csv") {

  # Ensure that the year, month, and day columns exist in the data
  if (!all(c("year", "month", "day") %in% colnames(data))) {
    stop("The data must contain columns: 'year', 'month', and 'day'")
  }

  # Ensure that the necessary weather data columns exist in the data
  if (!(prec_col %in% colnames(data)) | !(temp_col %in% colnames(data))) {
    stop("Precipitation (prec) or Maximum Temperature (Tmax) columns not found in the data")
  }

  # Number of observations (rows) in the dataset
  obs_count <- nrow(data)

  # Initialize the KBDI and DF vectors
  KBDI <- numeric(obs_count)
  DF <- numeric(obs_count)

  # Calculate the average precipitation and yearly mean
  average_precipitation <- mean(data[[prec_col]], na.rm = TRUE)
  yearly_mean <- average_precipitation * 365.25

  # Time increment for the index calculation (1 day)
  dt <- 1

  # Loop to compute KBDI
  for(i in 1:obs_count) {
    if (i == 1) {
      KBDIn <- 0
    } else {
      KBDIn <- KBDI[i-1] - max(0, data[[prec_col]][i] - 3)
    }

    # Calculate the drying factor (DF) for each day
    DF[i] <- (FC - KBDIn) * (1.713 * exp(0.0875 * data[[temp_col]][i] + 1.5552) - 14.59) * dt * 0.001 / (1 + 10.88 * exp(-0.001736 * yearly_mean))

    # Update the KBDI value
    KBDI[i] <- KBDIn + DF[i]

    # Ensure KBDI does not go below 0
    if (KBDI[i] < 0) {
      KBDI[i] <- 0
    }
  }

  # Create the result data frame, retaining original year, month, and day columns
  result <- data.frame(data[, c("year", "month", "day")], DF = DF, KBDI = KBDI)

  # If save_to_csv is TRUE, save the result as a CSV file
  if (save_to_csv) {
    write.csv(result, file = file_name, row.names = FALSE)
    message(paste("File saved as", file_name))
  }

  return(result)
}
