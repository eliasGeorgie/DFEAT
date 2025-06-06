% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MYD_characterization.R
\name{MYD_characterization}
\alias{MYD_characterization}
\title{Multi-Year Drought (MYD) Missing Drought Features Characterization}
\usage{
MYD_characterization(
  kbdi_data,
  minDmax,
  drought_features_data,
  save_to_csv = FALSE,
  file_name = "DF_characterization_under_MYD.csv"
)
}
\arguments{
\item{kbdi_data}{A data frame containing daily KBDI data, which is the output of the `calculate_KBDI` function.
It is crucial to ensure that all years are fully present, in chronological order, and without any missing KBDI values for any given day.}

\item{minDmax}{A data frame containing the days since beginning of minimum kbdi value between two consecutive peak drought days and its corresponding KBDI value (min_KBDI), which is the output of the `identify_min_kbdi_between_two_KBDI_peaks` function}

\item{save_to_csv}{A logical value (`TRUE` or `FALSE`). If `TRUE`, the function will save the results to a CSV file. Default is `FALSE`.}

\item{file_name}{A string specifying the name of the CSV file to save the results if `save_to_csv` is `TRUE`. Default is "DF_characterization_under_MYD.csv".}
}
\value{
The function returns the same drought features table, but with missing values recalculated for each hydrological year.
}
\description{
This function takes as primary inputs the extracted drought features table, generated using the
`Drought_features_extraction function`, along with the kbdi_data calculated from the
`calculate_KBDI` function, and the outputs from the `identify_min_kbdi_between_two_drought_peaks`function.
Together, these inputs are used to re-calculate missing drought features for prolonged Multi-Year Drought (MYD) events,
where various soil desiccation thresholds could not be reached due to insufficient rainfall for soil moisture recovery to field capacity.
For a detailed explanation of the methodologies applied to retrieve each missing drought feature by threshold or soil desiccation levels, users should consult the work of Elias et al. (2024).
Some years, even after MYD recalculation, may still have missing values for extreme drought onset, offset, duration, and severity. This primarily occurs when the KBDI time series desiccation for those years does not exceed the KBDI-150 threshold.
}
\examples{
# calculate KBDI
result_kbdi <- calculate_KBDI(tab_clim)

# calculate Peak.KBDI.DOY and its corresponding value Peak.KBDI over each hydrological year
peak_drought_info <- identify_peak_drought_days(result_kbdi)

# Identify Minimum KBDI day and corresponding value Between Two Consecutive Peak Drought Days (Peak.KBDI.DOY)
min_kbdi_between_drought_peaks <- identify_min_kbdi_between_two_drought_peaks(result_kbdi, peak_drought_info)

# Initialize an empty table to store the extracted drought features
drought_features_table  <- initialize_drought_features_table(result_kbdi)


# Drought features extraction
DF <- Drought_features_extraction (kbdi_data = result_kbdi,
                                   minDmax = min_kbdi_between_drought_peaks,
                                   drought_features_table = drought_features_table,
                                   save_to_csv = F,
                                   plot_features_extraction = T)

# Multi-year drought characterization
MYD_features_calculation <- MYD_characterization(kbdi_data = result_kbdi,
                                                 minDmax = min_kbdi_between_drought_peaks,
                                                 drought_features_data = DF,
                                                 save_to_csv = FALSE)

}
