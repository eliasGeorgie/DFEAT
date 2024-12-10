# DFEAT

<!-- badges: start -->

<!-- badges: end -->

Agro-environmental impacts are more closely linked to the multifaceted temporal patterns of soil water desiccation,
surpassing the confines of a singular drought feature of duration or severity,
hardly yet conceptualized and standardized in a single tool for intercomparison between studies.
We describe here the application of a new tool, DFEAT, to compute a diverse set of drought features,
utilizing a daily soil water content time series standardized here using the empirical Keetch-Byram Drought Index (KBDI).
More particularly, the extracted and characterized features are related to the duration, severity, onset, offset, drying and wetting rates, driest peak day, and rainfall pulses of each drought event.
These features are assessed across three soil desiccation thresholds, which represent varying levels of drought propagation in soil moisture, selected for their potential impact on agro-ecosystems.


The package contains nine user-friendly functions, each clearly described and documented, intended for sequential implementation.
It requires a single input table, a daily climate dataset with columns for 'year', 'month', 'day',
daily precipitation ('prec'), and daily maximum temperature ('Tmax'). When the climate data is loaded,
the initial set of functions verifies the variable names in the specified columns, allowing for additional columns if present but notifying the user.
These functions also ensure that the dataset contains complete years in correct chronological order without any missing values. Once verified,
the subsequent functions, including KBDI calculation and related features extraction functions, can be implemented sequentially.

The package has been developed using the Mediterranean-calibrated version of the KBDI and has been successfully tested across various Mediterranean countries,
yielding reliable results. However, it should be noted that applying this package outside of Mediterranean conditions
may require adjustments to the defined methodological approach to ensure accuracy and relevance in differing climates.

The following example, which begins calculations using the 'tab_clim' dataset, illustrates a typical Mediterranean soil moisture desiccation pattern.
This pattern exceeds the three defined soil desiccation thresholds and subsequently recovers to field capacity, a scenario characteristic of sub-humid and humid conditions.
For Multi-Year Drought (MYD) analysis, users should instead run the example with the 'MYD_tab_clim' dataset.

## License
This software is licensed under a proprietary license. Please refer to the `LICENSE` file for detailed terms and conditions.

## Installation

You can install the development version of DFEAT from [GitHub](https://github.com/eliasGeorgie/DFEAT.git) with:

``` r
devtools::install_github('https://github.com/eliasGeorgie/DFEAT.git')
```

## Example

This is a simple example demonstrating the step-by-step implementation of functions within the DFEAT package:



``` r
library(DFEAT)
data("tab_clim")
head(tab_clim)

# First verifies that all columns are named consistently as specified
check_columns_names(tab_clim)

# Check if each year has 365 or 366 (leap year) days, if it start or no on January 1st, and if years are in chornological order
check_years_days_and_start(tab_clim)

# Check for missing values in the input climate table
check_missing_values(tab_clim)

# Calculate KBDI
result_kbdi <- calculate_KBDI(tab_clim)
# View the result
head(result_kbdi)

# Identify Peak Drought Intensity Day (DOY) and its Maximum KBDI Value for Each Year
# Assuming `result_kbdi` is the output of the `calculate_KBDI` function
peak_drought_info <- identify_peak_drought_days(result_kbdi)
head(peak_drought_info)

# Identify Minimum KBDI Value and Corresponding Day Since Beginning Between Two Consecutive Peak Drought Days (Peak.KBDI.DOY)
min_kbdi_between_drought_peaks <- identify_min_kbdi_between_two_drought_peaks(result_kbdi, peak_drought_info)

# Initialize Empty Drought Features Table
drought_features_table  <- initialize_drought_features_table(result_kbdi)
View(drought_features_table)


# Yearly Drought Features Extraction
DF <- Drought_features_extraction (kbdi_data = result_kbdi,
                                   minDmax = min_kbdi_between_drought_peaks,
                                   drought_features_table = drought_features_table,
                                   save_to_csv = F,
                                   plot_features_extraction = T)

View(DF)

# Multi-Year Drought (MYD) Missing Drought Features Characterization
MYD_features_calculation <- MYD_characterization(kbdi_data = result_kbdi,
                                                 minDmax = min_kbdi_between_drought_peaks,
                                                 drought_features_data = DF,
                                                 save_to_csv = FALSE)
View(MYD_features_calculation)

```
