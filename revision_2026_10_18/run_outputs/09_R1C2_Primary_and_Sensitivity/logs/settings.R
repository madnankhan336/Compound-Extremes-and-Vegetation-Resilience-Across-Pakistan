list(input_dir = "E:/proj-china/AdnanB/Dataset/netcdfs", ndvi_file = "ndvi_monthly_1985_2025.nc", 
    temperature_file = "tc_tmx_monthly_1985_2025.nc", precipitation_file = "tc_pr_monthly_1985_2025.nc", 
    elevation_file = "analysis_outputs/elevation_projected_to_ndviGrid.tif", 
    output_parent = "E:/proj-china/AdnanB/Dataset/netcdfs/analysis_outputs", 
    rows_per_block = 10L, minimum_reference_fraction = 0.8, minimum_sd = 1e-06, 
    detrend_minimum_years = 20L, response_lags = 0:3, recovery_horizon = 12L, 
    recovery_consecutive_months = 2L, antecedent_months = 6L, 
    max_cells = 0L)
