#' @title Identify Minimum KBDI Value and Corresponding Day Since Beginning Between Two Consecutive Peak Drought Days (Peak.KBDI.DOY)
#'
#' @description
#' This function retrieve the minimum KBDI value and its corresponding day since the beginning of the KBDI time series,
#' particularly, between two consecutive peak KBDI days (Peak.KBDI.DOY).The retrieved 'minDmax_DOY' value will subsequently be used in drought features extraction,
#' serving to mark the end of consecutive hydrological years, which may occur after December 31st.
#' It's corresponding value 'min_KBDI' could be zero when the soil is fully replenished to its field capacity at the end of the dry season.
#' However, in the case of a MYD drought, the soil may not reach full replenishment, and the minimum 'min_KBDI' value could be greater than zero.
#' The function uses the peak drought days identified from the `identify_peak_drought_days` function to define the range for finding
#' the minimum KBDI value.
#' This function will return a table with the number of rows (equal to the number of years) reduced by one, as it cannot calculate the minDmax_DOY for the final year.
#' @param kbdi_data A data frame containing the daily calculated KBDI values in a column named "KBDI", in addition of 'year', 'month', and 'day' columns, which are the output of the `calculate_KBDI` function.
#' Users should ensure that all target years are fully represented, with each day included, and that all KBDI values contain no missing (NA) values.
#' @param peak_drought_data A data frame containing yearly peak KBDI days (DOY) and peak KBDI values,
#'        which is the output from the `identify_peak_drought_days` function. It should include three columns:
#'        "year", "Peak.KBDI.DOY", and "Peak.KBDI".
#'
#' @return A data frame with columns:
#' \item{year}{The year corresponding to each calculated minimum KBDI.}
#' \item{minDmax_DOY}{The day of the minimum KBDI between two consecutive peak days,
#' in days since the beginning of the KBDI time series.}
#' \item{min_KBDI}{The minimum KBDI value between two consecutive peak KBDI days.}
#'
#' @examples
#' # Example usage:
#' # Assuming `result_kbdi` is the output of the `calculate_KBDI` function, and `peak_drought_info` is the output of `identify_peak_drought_days` function
#' min_kbdi_between_drought_peaks <- identify_min_kbdi_between_two_drought_peaks(result_kbdi, peak_drought_info)
#'
#'

identify_min_kbdi_between_two_drought_peaks <- function(kbdi_data, peak_drought_data) {

  # Ensure the necessary columns are in the kbdi_data
  if (!all(c("year", "KBDI") %in% colnames(kbdi_data))) {
    stop("The KBDI data must contain both 'year' and 'KBDI' columns")
  }

  # Ensure the necessary columns are in the peak_drought_data
  if (!all(c("year", "Peak.KBDI.DOY", "Peak.KBDI") %in% colnames(peak_drought_data))) {
    stop("The peak_drought_data must contain the 'year', 'Peak.KBDI.DOY', and 'Peak.KBDI' columns")
  }


  # Check if the same number of years are present in both datasets
  kbdi_years <- unique(kbdi_data$year)
  peak_drought_years <- unique(peak_drought_data$year)

  if (length(kbdi_years) != length(peak_drought_years)) {
    missing_years <- setdiff(kbdi_years, peak_drought_years)
    extra_years <- setdiff(peak_drought_years, kbdi_years)

    stop_message <- "Mismatch in years between kbdi_data and peak_drought_data:\n"
    if (length(missing_years) > 0) {
      stop_message <- paste0(stop_message,
                             "The following years are in kbdi_data but missing in peak_drought_data: ",
                             paste(missing_years, collapse = ", "), ".\n")
    }
    if (length(extra_years) > 0) {
      stop_message <- paste0(stop_message,
                             "The following years are in peak_drought_data but missing in kbdi_data: ",
                             paste(extra_years, collapse = ", "), ".\n")
    }

    stop(stop_message)
  }


  # Extract the number of years and initialize vectors for storing results
  number_of_years <- nrow(peak_drought_data) - 1  # Exclude the last year due to calculation requirement

  # Initialize result vectors
  minDmax <- integer(number_of_years)
  min_KBDI <- numeric(number_of_years)

  # Loop over each year to identify the minimum KBDI between two peak KBDI DOY
  for (i in 1:number_of_years) {
    # Define start and end positions based on the peak DOY (since begining)
    start_since_beginning <- trunc(1 + (i - 1) * 365.25) + peak_drought_data$Peak.KBDI.DOY[i]
    end_since_beginning <- trunc(peak_drought_data$Peak.KBDI.DOY[i + 1] + 400 + (i - 1) * 365.25) - 30

    # Extract KBDI values within the defined range for the current year
    tab_moisture <- kbdi_data$KBDI[start_since_beginning:end_since_beginning]

    # Extract minimum KBDI and its DOY since beginning within this period
    min_KBDI[i] <- min(tab_moisture, na.rm = TRUE)
    DOY_mini <- which(tab_moisture == min_KBDI[i])[1]
    minDmax[i] <- trunc(peak_drought_data$Peak.KBDI.DOY[i] + ((i - 1) * 365.25)) + DOY_mini
  }

  # Return the results in a data frame
  result <- data.frame(
    year = peak_drought_data$year[1:number_of_years],
    minDmax_DOY = minDmax, # the day of minimum KBDI values between two consecutive peak drought days in day since beginning
    min_KBDI = min_KBDI   # the corresponding minimum retrieved KBDI value for that day since beginning
  )

  return(result)
}
