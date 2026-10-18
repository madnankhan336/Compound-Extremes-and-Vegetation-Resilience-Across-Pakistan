# Diagnostic table interpretation

The supplied summary values are retained without adjustment. These additional diagnostics have different observation sets and definitions from the revised primary delayed-response and censored-recovery analysis.

| Files | Interpretation |
| --- | --- |
| Observation_density.csv | Historical sensor-era scene density; its combined single-scene/interpolated category is not revised NA-only coverage. |
| Sensor_offsets.csv | Reported raw offsets and 2013 steps; standardized shifts are offset divided by the supplied interannual SD. These are different diagnostics, not proof of uniform sensor equivalence. |
| July_NA_cleaned.csv | Illustrative July series; zero-scene interpolated entries remain NA. Its location is not supplied and observed anomaly references have not been recalculated. Do not treat it as a national series. |
| Resistance_sensitivities.csv | Original, detrended and harmonized spectral resistance-proxy summaries for their own sensitivity sample. They do not replace the primary lagged-response estimates. |
| Recovery_clocks.csv | Separate calendar-month and growing-season recovery summaries. Only columns explicitly labelled mean or median specify that statistic; remaining suppressed-event summaries retain their supplied labels. Do not infer a pooled median or Kaplan-Meier probability from these values. |
| Landcover_stratification.csv | Belt-specific cell shares, suppression probabilities, resistance proxies and observed-return medians. Static classes contextualize management and vegetation differences. |
| Weighted_arithmetic_check.csv | Cell-share weighted arithmetic checks; subgroup-summary weights are not pooled event weights and a weighted average of medians is not a pooled median. |
| P75_P50_selected_year_paired_diagnostics.csv; P75_P50_2018_monthly_diagnostics.csv; Table_S39_P75_P50_selected_year_summary.csv | Raw NDVI compositing differences from six selected years. The differences cannot be converted directly into standardized suppression or recovery-time bias. Yearly ranges are not confidence intervals. |

P75 and P50 use identical QA-screened observations at 30 m. Differences are averaged over common valid pixels within each 0.05-degree cell. Cell-months with no observation or less than 10% valid-pixel coverage remain NA; interpolated months are excluded. Summaries use cos(latitude) cell-area weights. The years are 1990, 2001, 2009, 2012, 2018 and 2024. Missingness restricts the diagnostic to observable conditions, especially in mountains.

These are published summary tables. Their underlying per-observation calculation exports are not included, so the package reproduces their displayed summaries and interpretation rather than claiming a new independent image-level validation. Primary event-analysis denominators are recorded separately in the primary run outputs.
