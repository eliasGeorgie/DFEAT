% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_peak_drought_days.R
\name{identify_peak_drought_days}
\alias{identify_peak_drought_days}
\title{Identify Peak Drought Intensity Day (DOY) and its Maximum KBDI Value for Each Year}
\usage{
identify_peak_drought_days(kbdi_data)
}
\arguments{
\item{kbdi_data}{A data frame containing the daily calculated KBDI all over targeted years, with columns for 'year', 'month', and 'day'.
This data frame is primarily the output of the `calculate_KBDI` function and includes the following columns:
- \code{year}: Year of the observation.
- \code{month}: Month of the observation.
- \code{day}: Day of the observation.
- \code{KBDI}: The KBDI value for each day.

The data should be structured with daily observations, and the columns must be named exactly as described.}
}
\value{
A data frame with the following columns:
- \code{year}: The year of the observation.
- \code{Peak.KBDI.DOY}: The day of year (DOY) of the peak KBDI value .
- \code{Peak.KBDI}: The peak KBDI value for that specific peak drought day, truncated to an integer value.
}
\description{
This function identifies the day of year (DOY) with the highest KBDI value (Peak.KBDI.DOY)
for each considered year, focusing on the period from 1st May to 31st December (period of soil moisture drought propagation under Mediterranean conditions). It also returns
the corresponding peak KBDI value for that day (Peak.KBDI). The function assumes that the input data contains complete and successive
daily 'KBDI' values with associated 'year', 'month', and 'day' columns.
The result is returned as a data frame containing the year, peak drought day (Peak.KBDI.DOY), and peak KBDI value (Peak.KBDI).
If any year is missing days, or if a fully present year contains missing KBDI values, the function will stop and notify the user with a warning about the affected years.
}
\details{
The function calculates the peak KBDI value and the corresponding day of year (DOY) for each year.
The function assumes that the input data frame is structured by day and includes the required columns for `year`, `month`, `day`, and `KBDI`.
}
\examples{
# Assuming `result_kbdi` is the output of the `calculate_KBDI` function:
peak_drought_info <- identify_peak_drought_days(result_kbdi)
head(peak_drought_info)

}
