# Reproducing elevation stratified exposure and response tables

The revised tables can be regenerated from prepared monthly inputs. Running the archived code alone does not acquire raw products, prepare the common grid or remove the need to configure paths and packages.

## Use the revised framework

Use `09_HD_R1C2_Primary_Sensitivity_Monthwise_v2.R` for the revised primary estimates and baseline/NDVI sensitivities. It embeds the lagged-response and censored-recovery functions; a second script need not be sourced. `08_HD_R1C1_Lag_Recovery_Revised.R` is retained for the earlier dedicated lag diagnostic, not as an extra step before script 09.

The local legacy v1.0.0 archive contains earlier full-period/within-event definitions, path placeholders and no monthly input rasters. Its generic script 04 also sets the wet threshold to P90; the manuscript hot-wet diagnostic and original `Analysis-01.R` use P95. That legacy runner is not interchangeable with the revised framework and should not be used to assert reproduction of revised primary estimates. This guide records local archive inspection; it does not certify the current public deposit contents.

## Required prepared inputs

Provide the following paths in `R1C2_SETTINGS` or through the supported `r1c2.settings` option:

| Setting | Existing input filename | Requirement |
| --- | --- | --- |
| ndvi_file | ndvi_monthly_1985_2025.nc | Raw monthly NDVI, not already standardized anomalies |
| temperature_file | tc_tmx_monthly_1985_2025.nc | Monthly maximum temperature in the analysis units |
| precipitation_file | tc_pr_monthly_1985_2025.nc | Monthly precipitation in the analysis units |
| elevation_file | analysis_outputs/elevation_projected_to_ndviGrid.tif | Elevation in metres from the Copernicus GLO-30 DEM (Google Earth Engine), aligned to the NDVI grid |

The three monthly stacks must have 492 date-indexed layers in order from January 1985 through December 2025. All four inputs must have matching geometry. The study grid is 0.05 degrees in EPSG:4326, 269 rows by 340 columns. The script checks geometry and dates; it does not perform upstream raw-product acquisition, Landsat QA/compositing or grid preparation. Retain missing monthly observations as NA, including October-December 1994; do not interpolate or substitute zeros.

Install R packages `terra`, `data.table`, `ggplot2` and `survival`. Configure an existing input directory and a writable output parent. The original script retains the study computer's E-drive paths; change settings to the reader's paths before running. The script records `sessionInfo()` and writes to a new timestamped output folder.

Example after placing the prepared files under a local input directory:

```r
options(r1c2.settings = list(
  input_dir = "D:/study_inputs",
  elevation_file = "analysis_outputs/elevation_projected_to_ndviGrid.tif",
  output_parent = "D:/study_outputs",
  max_cells = 0L
))
source("D:/revision_code/09_HD_R1C2_Primary_Sensitivity_Monthwise_v2.R")
```

`max_cells=0L` is the full run. A positive value is a software smoke test and must not be used as manuscript evidence. This guide does not claim a fresh independent full rerun.

## Outputs and table selection

- `tables/Framework_manifest.csv` identifies references and processing choices. Primary results are `PRIMARY_H14_ORIGINAL`: fixed 1985-2014 climate thresholds and original NDVI calendar-month references. Alternative references and detrending remain sensitivities.
- `tables/Annual_climate_exposure_by_reference.csv` and `Exposure_trend_comparison.csv` contain the national/elevation climate summaries and trends. Annual affected area is distinct from annual area-month totals; select the metric named in the target table.
- Within `frameworks/PRIMARY_H14_ORIGINAL/Event_Response_<timestamp>/tables/`, `Suppression_lag_comparison_by_elevation.csv`, `Recovery_censoring_by_elevation.csv`, `Recovery_curves_KM_and_competing_HD.csv` and `Recovery_eligibility_and_response_completeness.csv` provide response, eligible-denominator and censoring summaries by elevation. Use response_lag=3 for the main delayed-response specification and cohort=all_suppressed for the reported recovery cohort. Other lag windows/cohorts are distinct sensitivities.
- `tables/All_frameworks_suppression.csv` and `All_frameworks_recovery_censoring.csv` consolidate the alternatives. Do not pool framework rows or average medians across cells/cohorts to obtain a national event statistic.
- Table S40 uses `hd_area_fraction_complete` over 1985-2025. Table S41 uses primary lag-3 `all_suppressed` eligible counts. The included `R2C4_8_summarize_existing_outputs.py` documents the existing result-table consolidation and accepts --root and --out to select this package and an output directory.
- Audit `NA_preservation_check.csv`, `Raw_monthly_input_audit.csv`, `Observed_NDVI_with_unestimable_reference.csv` and `Month_specific_retention_by_elevation.csv` before interpreting missingness or high-elevation coverage. A censored event is not an observed 12-month recovery; a Kaplan-Meier median not reached remains NA.

## GitHub revision and source workflow

The updated GitHub folder contains clean manuscripts, revised scripts, summary tables and exact run metadata. Raw monthly input rasters are not included. Updating GitHub does not update the separate Zenodo record.

The original repository includes `scripts/gee_export_monthly_landsat_ndvi.js`: Landsat Collection 2 Level-2 TM/ETM+/OLI/OLI-2, monthly P75, spatial mean aggregation and annual 12-band exports. The author confirms Landsat imagery. Its recorded defaults use a 10,000 m export scale and enable short-gap filling; they differ from the revised analysis-grid and missing-data requirements. A copy with gap filling disabled is included under `code/upstream/`. Neither script was executed during packaging. Actual export identifiers and grid-preparation metadata are needed to link exports to the prepared-input inventory. The existing analysis retains missing values as NA.

## Included existing run records

`Reproduction_metadata/` contains the existing primary run input MD5/size inventory, framework manifest, settings, R session information and NA-preservation audit. `tables/Primary_reproduction_source_tables/` contains existing climate and primary response source summaries. These are copied unchanged from the completed 09_R1C2_Monthwise_v2_20261014_221940 run; monthly input rasters are not duplicated in this package.
