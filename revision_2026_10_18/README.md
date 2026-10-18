# Revised data and code for the Pakistan vegetation assessment

Compound Heat–Drought Extremes in Pakistan (1985–2025): Separating Climatic Exposure from Observed NDVI Response

This folder contains the 18 October 2026 revision. The repository's original files remain available alongside it. Use this folder for the revised primary lagged-response and censored-recovery results.

## Contents

- `documents/`: clean manuscript and supplementary material, without reviewer colours.
- `code/`: revised R analyses, targeted GEE diagnostics, upstream export scripts and execution guide.
- `tables/`: manuscript source summaries and additional diagnostic tables.
- `figures/`: final main and supplementary figures and captions.
- `run_outputs/`: existing revised primary/sensitivity summary CSVs and run metadata.
- `metadata/`: prepared-input filenames, sizes, MD5 hashes, settings, software session and source-workflow description.
- `MANIFEST_SHA256.json`: file integrity inventory.

Monthly input rasters and the large per-event CSV inventories are not included in this GitHub folder. Their analysis scripts are included; prepared inputs and an aligned elevation grid are required. Run `code/09_HD_R1C2_Primary_Sensitivity_Monthwise_v2.R` after configuring paths. It embeds lag/recovery functions; script 08 is an earlier dedicated diagnostic. See `code/README_Elevation_Table_Reproduction.md`.

## Primary framework

Climate thresholds and original NDVI calendar-month anomalies use fixed 1985-2014 references. Hot-dry months exceed local P95 maximum temperature and fall below local P10 precipitation. The main response window includes the event and three subsequent months, stopping before another event. Recovery is evaluated after observed suppression, requires two consecutive observed nonnegative anomaly months, and retains incomplete follow-up as censored. Missing observations remain NA, including October-December 1994. Alternative references and detrended NDVI are separate sensitivity analyses. RF/QRF and SHAP outputs describe historical associations rather than operational forecasts.

## Landsat source workflow

The author confirms Landsat imagery. The original repository script at `../scripts/gee_export_monthly_landsat_ndvi.js` selects Collection 2 Level-2 Landsat 5, 7, 8 and 9, computes monthly P75 NDVI, averages spatially and exports annual stacks. The script does not apply a further monthly-maximum operator. Its original defaults use a 10 km export scale and enable short-gap filling. The copy in `code/upstream/gee_export_monthly_landsat_ndvi_NA.js` disables filling. The prepared R inputs are on a 0.05-degree grid; their original export identifiers and grid-preparation metadata are needed to establish the exact input linkage. No new imagery export was executed for this update, and the existing scientific datasets were not replaced.

## Additional diagnostics

The coauthors' sensor, land-cover, seasonal and six-year P75-P50 values are retained as supplied supporting summaries. Different samples, anomaly references, clocks and weights are not pooled with the primary event cohort. `tables/DIAGNOSTIC_TABLE_GUIDE.md` explains the available definitions. Full underlying per-observation calculation exports for these additional tables are not included. Raw P75-P50 differences alone do not measure bias in standardized suppression or censored recovery; yearly ranges are not confidence intervals. No new validation result is inferred from the supplied summaries.

## Reproduce Tables S40 and S41 from existing outputs

With Python and pandas installed, run from this folder:

```sh
python code/R2C4_8_summarize_existing_outputs.py --root . --out reproduced_tables
```

This consolidates existing result tables; it does not rerun the raster analysis. For a full R run, provide 492 monthly layers (January 1985-December 2025), the matching elevation grid and the packages listed in the execution guide. Source export scripts require the appropriate Earth Engine project and study boundary asset. Original files with different definitions remain historical code and are not interchangeable with the revised primary framework.

## Licence and citation

Code retains the repository's MIT licence; derived data retain CC BY 4.0. Third-party products remain subject to their original distribution terms. Cite the study and the existing record at https://doi.org/10.5281/zenodo.21821042. Updating this GitHub folder does not update that separate record or assign a new DOI.
