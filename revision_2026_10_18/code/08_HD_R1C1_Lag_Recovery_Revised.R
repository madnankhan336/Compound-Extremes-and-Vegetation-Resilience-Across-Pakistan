# Reviewer 1 Comment 1
# Targeted revision of Stage 8 using the inputs produced by Analysis-01.R.
# Originals and earlier outputs are never overwritten.
# Run the whole file with source(file.choose()) in a fresh R session.

R1C1_SETTINGS <- list(
  input_dir = 'E:/proj-china/AdnanB/Dataset/netcdfs/analysis_outputs',
  ndvi_file = 'ndvi_z_1985_2025.nc',
  hd_file = 'flag_hot_dry_1985_2025.nc',
  elevation_file = 'elevation_projected_to_ndviGrid.tif',
  # These labels describe the ORIGINAL inputs created by Analysis-01.R.
  # If using newer inputs, change the file paths AND these provenance labels.
  climate_threshold_reference = '1985-2025 full period; original Analysis-01.R',
  ndvi_reference = '1985-2025 calendar-month mean and SD; original Analysis-01.R',
  output_parent = 'E:/proj-china/AdnanB/Dataset/netcdfs/analysis_outputs',
  response_lags = 0:3,
  recovery_horizon = 12L, # months after climatic event end, for ALL lag windows
  recovery_consecutive_months = 2L,
  antecedent_months = 6L,
  rows_per_block = 10L,
  # 0 = ALL common-grid cells. A positive limit is ONLY a software smoke test.
  max_cells = 0L,
  # Leave NULL unless dates truly are absent and layer order is verified.
  verified_start_date_if_missing = NULL
)

# ------------------------------------------------------------------
# Base-R event functions. No package is needed to run the self-tests.
# ------------------------------------------------------------------
r1c1_seq <- function(a, b) if (a <= b) seq.int(a, b) else integer(0)
r1c1_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
r1c1_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE)) else NA_real_
}

r1c1_events <- function(flag) {
  known <- is.finite(flag)
  if (any(known & !(flag %in% c(0, 1)))) stop('HD flags must be 0, 1, or NA.')
  on <- known & flag == 1
  rr <- rle(on)
  ends <- cumsum(rr$lengths)
  starts <- ends - rr$lengths + 1L
  ii <- which(rr$values)
  if (!length(ii)) return(data.frame(start = integer(), end = integer(),
                                    duration = integer(), boundary_complete = logical()))
  st <- starts[ii]; en <- ends[ii]
  # Unknown climate does not become a known non-event month.
  before <- vapply(st, function(i) i > 1L && known[i-1L] && flag[i-1L] == 0, logical(1))
  after <- vapply(en, function(i) i < length(flag) && known[i+1L] && flag[i+1L] == 0, logical(1))
  data.frame(start = st, end = en, duration = en-st+1L,
             boundary_complete = before & after)
}

r1c1_first_return <- function(y, start, end, consec = 2L) {
  # Both months must be observed and inside the allowed window.
  for (i in r1c1_seq(start, end-consec+1L)) {
    ix <- seq.int(i, i+consec-1L)
    if (all(is.finite(y[ix])) && all(y[ix] >= 0))
      return(c(onset = i, confirmation = i+consec-1L))
  }
  c(onset = NA_integer_, confirmation = NA_integer_)
}

r1c1_one_event <- function(y, flag, start, end, next_start, lag,
                          boundary_complete = TRUE, horizon = 12L,
                          consec = 2L, preL = 6L) {
  nT <- length(y)
  stopifnot(length(flag) == nT, start >= 1L, start <= end, end <= nT,
            lag >= 0L, lag < horizon, consec >= 1L)
  requested_end <- end + lag
  # Unknown climate after this event also limits event attribution.
  unknown <- which(!is.finite(flag) & seq_len(nT) > end)
  unknown_at <- if (length(unknown)) unknown[1L] else Inf
  next_at <- if (is.finite(next_start)) next_start else Inf
  response_end <- as.integer(min(requested_end, nT, next_at-1L, unknown_at-1L))
  response_ix <- r1c1_seq(start, response_end)
  yy <- y[response_ix]
  finite <- is.finite(yy)
  complete <- response_end == requested_end && all(finite)
  minimum <- if (any(finite)) min(yy[finite]) else NA_real_
  min_idx <- if (any(finite)) response_ix[which.min(replace(yy, !finite, Inf))] else NA_integer_
  # A partial window can establish a observed negative value, but cannot
  # establish absence of suppression over the entire requested window.
  suppressed <- if (is.finite(minimum) && minimum < 0) TRUE else if (complete) FALSE else NA
  pre_ix <- r1c1_seq(max(1L, start-preL), start-1L)
  pre_last <- if (start > 1L && is.finite(y[start-1L])) y[start-1L] else NA_real_
  pre_mean <- r1c1_mean(y[pre_ix])
  new_onset <- if (is.na(suppressed) || !is.finite(pre_last)) NA else suppressed && pre_last >= 0
  first_suppression <- if (any(finite & yy < 0)) response_ix[which(finite & yy < 0)[1L]] else NA_integer_
  truncation <- if (complete) 'complete' else if (any(!finite)) 'missing_NDVI' else
    if (unknown_at <= min(requested_end, nT, next_at-1L)) 'unknown_climate' else
    if (next_at <= requested_end) 'next_HD' else 'dataset_end'
  ans <- list(
    response_end_index = response_end, response_window_complete = complete,
    response_window_status = truncation, response_observed_months = sum(finite),
    response_min_z = minimum, minimum_index = min_idx,
    first_suppression_index = first_suppression,
    minimum_lag_from_event_end = min_idx-end,
    suppressed = suppressed, suppression_magnitude = if (isTRUE(suppressed)) -minimum else NA_real_,
    pre_last_z = pre_last, pre_mean_z = pre_mean, pre_observed_months = sum(is.finite(y[pre_ix])),
    pre_suppressed_last_month = if (is.finite(pre_last)) pre_last < 0 else NA,
    new_onset_suppression = new_onset,
    recovery_eligible = FALSE, recovery_exclusion = 'not_suppressed',
    landmark_index = NA_integer_, recovery_onset_index = NA_integer_,
    recovery_confirmation_index = NA_integer_, recovery_onset_months_from_event_end = NA_real_,
    recovery_confirmation_months_from_event_end = NA_real_,
    followup_months = NA_real_, recovery_observed = NA_integer_,
    censor_reason = NA_character_, competing_time = NA_real_, competing_status = NA_integer_
  )
  if (is.na(suppressed)) { ans$recovery_exclusion <- 'suppression_unassessable'; return(ans) }
  if (!isTRUE(suppressed)) return(ans)
  if (!boundary_complete) { ans$recovery_exclusion <- 'incomplete_climate_event_boundary'; return(ans) }
  # Reset the primary recovery clock at the LATER of event end and the
  # response minimum. This prevents crediting a return before delayed decline.
  landmark <- max(end, min_idx)
  # A truncated lag window with observed suppression is RETAINED. A next
  # event or dataset end will then censor it, rather than drop it. Missing
  # NDVI before the landmark makes that recovery clock unassessable; report
  # that exclusion separately. Missingness AFTER it is handled as censoring.
  if (any(!is.finite(y[seq.int(start,landmark)]))) {
    ans$recovery_exclusion <- 'missing_NDVI_before_recovery_landmark'; return(ans)
  }
  admin_end <- end + horizon
  cutoffs <- c(window_12_months = admin_end, dataset_end = nT,
               next_HD = next_at-1L, unknown_climate = unknown_at-1L)
  limit <- min(cutoffs)
  # Prioritize interruptions over administrative censoring when limits tie.
  reason <- if (is.finite(next_at) && next_at <= admin_end && next_at-1L == limit) 'next_HD' else
    if (is.finite(unknown_at) && unknown_at <= admin_end && unknown_at-1L == limit) 'unknown_climate' else
    if (nT < admin_end && nT == limit) 'dataset_end' else 'window_12_months'
  recovery_ix <- r1c1_seq(landmark+1L, as.integer(limit))
  missing <- recovery_ix[!is.finite(y[recovery_ix])]
  # A missing month prevents knowing whether recovery was confirmed there.
  # Conservatively censor at the preceding month; do not bridge the gap.
  if (length(missing)) { limit <- missing[1L]-1L; reason <- 'missing_NDVI' }
  ret <- r1c1_first_return(y, landmark+1L, as.integer(limit), consec)
  ans$recovery_eligible <- TRUE
  ans$recovery_exclusion <- 'eligible'
  ans$landmark_index <- landmark
  ans$recovery_observed <- as.integer(is.finite(ret['confirmation']))
  if (ans$recovery_observed == 1L) {
    ans$recovery_onset_index <- unname(ret['onset'])
    ans$recovery_confirmation_index <- unname(ret['confirmation'])
    ans$recovery_onset_months_from_event_end <- unname(ret['onset'])-end
    ans$recovery_confirmation_months_from_event_end <- unname(ret['confirmation'])-end
    ans$followup_months <- unname(ret['confirmation'])-landmark
    ans$censor_reason <- 'recovered'
    ans$competing_time <- ans$followup_months
    ans$competing_status <- 1L
  } else {
    ans$followup_months <- max(0L, limit-landmark)
    ans$censor_reason <- reason
    ans$competing_time <- ans$followup_months
    ans$competing_status <- 0L
    # Competing-event sensitivity records the next HD at its actual month,
    # while the primary analysis censors immediately BEFORE that month.
    if (reason == 'next_HD') {
      ans$competing_time <- next_at-landmark
      ans$competing_status <- 2L
    }
  }
  ans
}

r1c1_self_tests <- function() {
  flag <- c(0,1,rep(0,16)); y <- rep(0.2,18)
  a <- r1c1_one_event(y,flag,2,2,Inf,0)
  stopifnot(!a$suppressed, !a$recovery_eligible)
  y[3] <- -1
  a0 <- r1c1_one_event(y,flag,2,2,Inf,0)
  a1 <- r1c1_one_event(y,flag,2,2,Inf,1)
  stopifnot(!a0$suppressed, a1$suppressed, a1$minimum_index == 3L,
            a1$recovery_onset_index == 4L, a1$recovery_confirmation_index == 5L,
            a1$followup_months == 2L)
  # Never recovered: retains 12 months of follow-up and status 0.
  y[2:18] <- -1
  a <- r1c1_one_event(y,flag,2,2,Inf,0)
  stopifnot(a$recovery_observed == 0L, a$followup_months == 12L,
            a$censor_reason == 'window_12_months')
  # Next event: a positive month alone cannot confirm recovery across it.
  flag[5] <- 1; y[4:6] <- 0.2
  a <- r1c1_one_event(y,flag,2,2,5,0)
  stopifnot(a$recovery_observed == 0L, a$followup_months == 2L,
            a$censor_reason == 'next_HD', a$competing_status == 2L,
            a$competing_time == 3L)
  # Recovery confirmed before next event is retained.
  y[3:4] <- 0.2
  a <- r1c1_one_event(y,flag,2,2,5,0)
  stopifnot(a$recovery_observed == 1L, a$followup_months == 2L)
  # Next event truncates lag window; its negative NDVI is not credited.
  y <- rep(0.2,18); y[5] <- -5
  a <- r1c1_one_event(y,flag,2,2,5,3)
  stopifnot(is.na(a$suppressed), !a$response_window_complete,
            a$response_min_z == 0.2, !a$recovery_eligible)
  y[2:4] <- -1
  a <- r1c1_one_event(y,flag,2,2,5,3)
  stopifnot(a$suppressed, !a$response_window_complete, a$recovery_eligible,
            a$recovery_observed==0L, a$censor_reason=='next_HD')
  # Missing NDVI censors before gap even if later months are positive.
  flag <- c(0,1,rep(0,16)); y <- rep(-1,18); y[1] <- 0.2; y[4] <- NA; y[5:6] <- 0.2
  a <- r1c1_one_event(y,flag,2,2,Inf,0)
  stopifnot(a$censor_reason == 'missing_NDVI', a$followup_months == 1L)
  # End of dataset, one nonnegative month: no two-month confirmation.
  a <- r1c1_one_event(c(0.2,-1,-1,0.2),c(0,1,0,0),2,2,Inf,0)
  stopifnot(a$censor_reason == 'dataset_end', a$followup_months == 2L,
            a$recovery_observed == 0L)
  # Final-layer event, unknown climate boundaries, and no-event input.
  e <- r1c1_events(c(0,1,NA,1,0,1))
  stopifnot(nrow(e)==3L, !any(e$boundary_complete), nrow(r1c1_events(rep(0,5)))==0L)
  a <- r1c1_one_event(c(0.2,-1),c(0,1),2,2,Inf,0,FALSE)
  stopifnot(!a$recovery_eligible, a$recovery_exclusion == 'incomplete_climate_event_boundary')
  # Unknown climate cannot be used as a known disturbance-free month.
  a <- r1c1_one_event(c(0.2,-1,-1,0.2,0.2),c(0,1,0,NA,0),2,2,Inf,0)
  stopifnot(a$censor_reason == 'unknown_climate', a$followup_months == 1L)
  # Exactly 3500 m belongs to the highest belt (tested in main band assignment).
  # A pre-existing deficit is not labelled newly observed suppression.
  a <- r1c1_one_event(c(-0.2,-1,rep(0.2,16)),flag,2,2,Inf,0)
  stopifnot(a$pre_suppressed_last_month, !a$new_onset_suppression)
  # Two-month confirmation must be wholly inside the 12-month horizon.
  y <- rep(-1,18); y[1] <- 0.2; y[14:15] <- 0.2
  a <- r1c1_one_event(y,flag,2,2,Inf,0)
  stopifnot(a$recovery_observed == 0L, a$followup_months == 12L)
  cat('PASS: event logic self-tests (lags, recovery, interruptions, missingness, boundaries).\n')
  invisible(TRUE)
}

r1c1_curves <- function(d, horizon, cohort, lag, band, band_label) {
  # Primary curves censor next events. AJ sensitivity treats them as competing.
  # Repeated events within a pixel use pixel-clustered robust standard errors.
  d <- data.table::copy(d)
  if (!any(d$recovery_observed==1L)) {
    times <- 0:horizon
    km_zero <- ifelse(times<=max(d$followup_months),0,NA_real_)
    aj_zero <- ifelse(times<=max(d$competing_time),0,NA_real_)
    # No recovery endpoints: a zero estimated curve, with no estimated CI.
    # This also handles single-record strata without a multistate fit failure.
    return(list(median=NA_real_,curve=data.table::data.table(
      cohort=cohort,response_lag=lag,elev_band=band,elevation=band_label,
      month_from_landmark=times,
      n_at_risk=vapply(times,function(t)sum(d$followup_months>=t),integer(1)),
      km_recovery_probability=km_zero,km_lower=NA_real_,km_upper=NA_real_,
      aj_recovery_before_next_HD=aj_zero,aj_lower=NA_real_,aj_upper=NA_real_)))
  }
  enough_clusters <- length(unique(d$cell_id))>=2L
  km <- survival::survfit(survival::Surv(followup_months, recovery_observed) ~ 1,
                          data=d, cluster=if(enough_clusters)d$cell_id else NULL,
                          robust=enough_clusters,se.fit=enough_clusters)
  state <- factor(d$competing_status, levels=0:2,
                  labels=c('censored','recovered','new_HD'))
  d[, endpoint := state]
  aj <- survival::survfit(survival::Surv(competing_time, endpoint) ~ 1,
                          data=d, id=event_uid,cluster=if(enough_clusters)d$cell_id else NULL,
                          robust=TRUE,se.fit=enough_clusters)
  times <- 0:horizon
  at <- function(tt, values, initial) {
    vapply(times, function(t) {
      ii <- which(tt <= t)
      if (length(ii)) values[max(ii)] else initial
    }, numeric(1))
  }
  # Do not extend past the final supported follow-up and call it observed.
  support_km <- max(d$followup_months)
  support_aj <- max(d$competing_time)
  probability <- 1-at(km$time,km$surv,1)
  lower <- if(is.null(km$upper))rep(NA_real_,length(times)) else 1-at(km$time,km$upper,1)
  upper <- if(is.null(km$lower))rep(NA_real_,length(times)) else 1-at(km$time,km$lower,1)
  recovered_column <- match('recovered',aj$states)
  competing_probability <- if (is.na(recovered_column)) rep(0,length(times)) else
    at(aj$time,aj$pstate[,recovered_column],0)
  competing_lower <- if (is.na(recovered_column) || is.null(aj$lower)) rep(NA_real_,length(times)) else
    at(aj$time,aj$lower[,recovered_column],0)
  competing_upper <- if (is.na(recovered_column) || is.null(aj$upper)) rep(NA_real_,length(times)) else
    at(aj$time,aj$upper[,recovered_column],0)
  probability[times>support_km] <- lower[times>support_km] <- upper[times>support_km] <- NA_real_
  competing_probability[times>support_aj] <- competing_lower[times>support_aj] <-
    competing_upper[times>support_aj] <- NA_real_
  curve <- data.table::data.table(
    cohort=cohort, response_lag=lag, elev_band=band, elevation=band_label,
    month_from_landmark=times,
    n_at_risk=vapply(times,function(t)sum(d$followup_months>=t),integer(1)),
    km_recovery_probability=probability, km_lower=lower, km_upper=upper,
    aj_recovery_before_next_HD=competing_probability, aj_lower=competing_lower, aj_upper=competing_upper
  )
  crossing <- which(km$surv<=0.5)
  median_km <- if (length(crossing)) km$time[crossing[1L]] else NA_real_
  list(curve=curve, median=median_km)
}

r1c1_main <- function(cfg=R1C1_SETTINGS) {
  needed <- c('terra','data.table','ggplot2','survival')
  missing <- needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]
  if (length(missing)) stop('Install missing packages first: install.packages(c(',
                            paste(sprintf('"%s"',missing),collapse=', '),'))')
  suppressPackageStartupMessages({library(terra); library(data.table); library(ggplot2); library(survival)})
  stopifnot(identical(as.integer(cfg$response_lags),0:3), cfg$recovery_horizon==12L,
            cfg$recovery_consecutive_months==2L, cfg$rows_per_block>=1L, cfg$max_cells>=0L)
  r1c1_self_tests()
  paths <- vapply(c('ndvi_file','hd_file','elevation_file'),function(nm) {
    p <- cfg[[nm]]
    if (grepl('^([A-Za-z]:|/)',p)) p else file.path(cfg$input_dir,p)
  },character(1))
  if (any(!file.exists(paths))) stop('Missing input files:\n',paste(paths[!file.exists(paths)],collapse='\n'))
  ndvi <- terra::rast(paths['ndvi_file']); hd <- terra::rast(paths['hd_file'])
  elev <- terra::rast(paths['elevation_file'])
  terra::compareGeom(ndvi[[1]],hd[[1]],stopOnError=TRUE)
  terra::compareGeom(ndvi[[1]],elev,stopOnError=TRUE)
  stopifnot(terra::nlyr(elev)==1L, terra::nlyr(ndvi)==terra::nlyr(hd))
  dates <- terra::time(ndvi); hd_dates <- terra::time(hd)
  normalize_dates <- function(x,n,label) {
    if (!inherits(x,c('Date','POSIXt')) || length(x)!=n || anyNA(x)) {
      if (is.null(cfg$verified_start_date_if_missing))
        stop(label,' has no reliable date axis; set verified_start_date_if_missing only after checking layer order.')
      x <- seq(as.Date(cfg$verified_start_date_if_missing),by='month',length.out=n)
      warning('Using explicitly verified fallback dates for ',label)
    }
    as.Date(paste0(format(x,'%Y-%m'),'-01'))
  }
  nT <- terra::nlyr(ndvi)
  dates <- normalize_dates(dates,nT,'NDVI'); hd_dates <- normalize_dates(hd_dates,nT,'HD')
  if (!identical(dates,hd_dates) || !identical(dates,seq(dates[1],by='month',length.out=nT)))
    stop('NDVI and HD must have identical, consecutive monthly dates.')
  ev <- as.vector(terra::values(elev,mat=FALSE))
  first_ndvi <- as.vector(terra::values(ndvi[[1]],mat=FALSE))
  domain <- is.finite(ev) & is.finite(first_ndvi)
  bands <- findInterval(ev,c(500,1500,2500,3500))+1L
  bands[!domain] <- NA_integer_
  stopifnot(identical(findInterval(c(499.9,500,1499.9,1500,2499.9,2500,3499.9,3500),
                                  c(500,1500,2500,3500))+1L,c(1L,2L,2L,3L,3L,4L,4L,5L)))
  if (!any(domain)) stop('No cells in the common first-layer NDVI/elevation mask.')
  labels <- c('National','<500 m','500-1499 m','1500-2499 m','2500-3499 m','>=3500 m')
  all_cells <- which(domain)
  selected <- all_cells
  if (cfg$max_cells>0L && length(selected)>cfg$max_cells) {
    # Evenly spaced deterministic subset for software checks only.
    selected <- selected[unique(round(seq(1,length(selected),length.out=cfg$max_cells)))]
  }
  stamp <- format(Sys.time(),'%Y%m%d_%H%M%S')
  stem <- paste0('08_R1C1_Lag_Recovery_',stamp,if(cfg$max_cells>0L)'_SMOKE' else '')
  out <- file.path(cfg$output_parent,stem); suffix <- 1L
  while (dir.exists(out)) { suffix<-suffix+1L; out<-file.path(cfg$output_parent,paste0(stem,'_v',suffix)) }
  if (!dir.create(out,recursive=TRUE)) stop('Cannot create output directory: ',out)
  for (folder in c('tables','figures','logs')) dir.create(file.path(out,folder))
  log_connection <- file(file.path(out,'logs','console_log.txt'),open='wt')
  sink(log_connection,split=TRUE)
  on.exit({sink(); close(log_connection)},add=TRUE)
  cat('Output directory:',out,'\nInput dates:',as.character(dates[1]),'to',as.character(tail(dates,1)),'\n')
  cat('Common-grid cells:',length(all_cells),'  Processed cells:',length(selected),'\n')
  if (cfg$max_cells>0L) cat('SOFTWARE SMOKE TEST ONLY. Do not use these summaries in the manuscript.\n')
  writeLines(capture.output(dput(cfg)),file.path(out,'logs','settings.R'))
  inventory <- data.table::data.table(input=names(paths),path=unname(paths),
                                      bytes=as.numeric(file.info(paths)$size),
                                      modified=as.character(file.info(paths)$mtime),
                                      md5=unname(tools::md5sum(paths)))
  data.table::fwrite(inventory,file.path(out,'tables','Input_inventory.csv'))
  # Read complete monthly raster blocks once. Avoid repeated per-band reads.
  terra::readStart(ndvi); terra::readStart(hd)
  on.exit({terra::readStop(ndvi);terra::readStop(hd)},add=TRUE)
  nr <- terra::nrow(ndvi); nc <- terra::ncol(ndvi)
  row_starts <- seq.int(1L,nr,by=cfg$rows_per_block)
  ntotal <- nfinite <- nzero <- nlarge <- flag_known <- flag_on <- numeric(nT)
  sums <- numeric(nT); mins <- rep(Inf,nT); maxs <- rep(-Inf,nT)
  event_path <- file.path(out,'tables','HD_event_records_all_lags.csv')
  wrote <- FALSE; cells_done <- 0L
  pixel_event_counts <- integer(terra::ncell(elev))
  for (ib in seq_along(row_starts)) {
    row <- row_starts[ib]; rows <- min(cfg$rows_per_block,nr-row+1L)
    ym <- terra::readValues(ndvi,row=row,nrows=rows,mat=TRUE)
    fm <- terra::readValues(hd,row=row,nrows=rows,mat=TRUE)
    cells <- seq.int((row-1L)*nc+1L,(row+rows-1L)*nc)
    good <- which(domain[cells])
    if (length(good)) {
      ya <- ym[good,,drop=FALSE]; fa <- fm[good,,drop=FALSE]
      yf <- is.finite(ya); ff <- is.finite(fa)
      if (any(ff & !(fa %in% c(0,1)))) stop('Nonbinary HD flag values found in row block ',row)
      ntotal <- ntotal+nrow(ya); nfinite <- nfinite+colSums(yf)
      nzero <- nzero+colSums(yf & ya==0); nlarge <- nlarge+colSums(yf & abs(ya)>20)
      ya[!yf] <- NA_real_
      sums <- sums+colSums(ya,na.rm=TRUE)
      mins <- pmin(mins,apply(ya,2,function(x)if(any(is.finite(x)))min(x,na.rm=TRUE) else Inf))
      maxs <- pmax(maxs,apply(ya,2,function(x)if(any(is.finite(x)))max(x,na.rm=TRUE) else -Inf))
      flag_known <- flag_known+colSums(ff); flag_on <- flag_on+colSums(ff & fa==1)
    }
    take <- which(cells %in% selected)
    rows_out <- list(); ri <- 0L
    for (j in take) {
      cell <- cells[j]; y <- as.numeric(ym[j,]); flag <- as.numeric(fm[j,])
      events <- r1c1_events(flag); pixel_event_counts[cell] <- nrow(events)
      cells_done <- cells_done+1L
      if (!nrow(events)) next
      for (k in seq_len(nrow(events))) {
        st <- events$start[k]; en <- events$end[k]
        next_st <- if (k<nrow(events)) events$start[k+1L] else Inf
        legacy <- r1c1_first_return(y,en+1L,min(en+cfg$recovery_horizon,nT),2L)
        common <- r1c1_one_event(y,flag,st,en,next_st,max(cfg$response_lags),
                                 events$boundary_complete[k],cfg$recovery_horizon,2L,cfg$antecedent_months)
        for (lag in cfg$response_lags) {
          ans <- r1c1_one_event(y,flag,st,en,next_st,lag,events$boundary_complete[k],
                                cfg$recovery_horizon,2L,cfg$antecedent_months)
          ri <- ri+1L
          rows_out[[ri]] <- c(list(cell_id=cell,event_number=k,event_uid=paste(cell,k,sep='_'),
                                    elev_m=ev[cell],elev_band=bands[cell],elevation=labels[bands[cell]+1L],
                                    start_index=st,end_index=en,start_date=as.character(dates[st]),
                                    end_date=as.character(dates[en]),duration_months=en-st+1L,
                                    next_event_start_index=if(is.finite(next_st))next_st else NA_integer_,
                                    climate_event_boundary_complete=events$boundary_complete[k],
                                    response_lag=lag,
                                    matched_all_lags_complete=common$response_window_complete && events$boundary_complete[k],
                                    legacy_all_event_recovery_onset=unname(legacy['onset'])-en),ans)
        }
      }
    }
    if (length(rows_out)) {
      chunk <- data.table::rbindlist(rows_out,fill=TRUE)
      data.table::fwrite(chunk,event_path,append=wrote,col.names=!wrote,na='NA')
      wrote <- TRUE
    }
    cat(sprintf('Block %d/%d: processed %d/%d cells\n',ib,length(row_starts),cells_done,length(selected)))
  }
  audit <- data.table::data.table(date=as.character(dates),n_domain=ntotal,
                                  n_finite_ndvi=nfinite,mean_ndvi_z=sums/pmax(nfinite,1),
                                  minimum_z=ifelse(is.finite(mins),mins,NA_real_),
                                  maximum_z=ifelse(is.finite(maxs),maxs,NA_real_),
                                  missing_share=1-nfinite/ntotal,zero_share=nzero/pmax(nfinite,1),
                                  abs_z_over_20_share=nlarge/pmax(nfinite,1),
                                  known_climate_share=flag_known/ntotal,hd_month_cells=flag_on)
  audit[n_finite_ndvi==0,mean_ndvi_z:=NA_real_]
  data.table::fwrite(audit,file.path(out,'tables','Monthly_input_audit.csv'))
  cell_inventory <- data.table::data.table(cell_id=selected,elev_m=ev[selected],
                                           elev_band=bands[selected],n_events=pixel_event_counts[selected])
  data.table::fwrite(cell_inventory,file.path(out,'tables','Common_grid_cell_inventory.csv'))
  if (!wrote) stop('No HD events found in processed cells; increase max_cells or check input flags. Audit saved in ',out)
  dt <- data.table::fread(event_path,na.strings='NA')
  # Disjoint outcome categories are checked before writing any summaries.
  stopifnot(!anyDuplicated(dt[,.(event_uid,response_lag)]),
            all(dt[recovery_eligible==TRUE,followup_months]>=0),
            all(dt[recovery_observed==1,recovery_confirmation_index]<=dt[recovery_observed==1,end_index]+12L),
            all(dt[recovery_observed==1,recovery_confirmation_index]>dt[recovery_observed==1,minimum_index]))
  if (any(dt$recovery_observed==1 & is.finite(dt$next_event_start_index),na.rm=TRUE))
    stopifnot(all(dt[recovery_observed==1 & is.finite(next_event_start_index),
                    recovery_confirmation_index<next_event_start_index]))
  # No missing values are converted to zero recovery or non-suppression.
  suppression_tables <- list(); recovery_tables <- list(); curves <- list()
  ix <- ir <- ic <- 0L
  reasons <- c('window_12_months','next_HD','dataset_end','missing_NDVI','unknown_climate')
  for (lag in cfg$response_lags) for (band in 0:5) {
    dd <- dt[response_lag==lag]
    if (band>0L) dd <- dd[elev_band==band]
    complete <- dd[response_window_complete==TRUE & climate_event_boundary_complete==TRUE]
    matched <- dd[matched_all_lags_complete==TRUE]
    ix<-ix+1L
    suppression_tables[[ix]] <- data.table::data.table(
      response_lag=lag,elev_band=band,elevation=labels[band+1L],n_events=nrow(dd),
      n_climate_boundary_incomplete=sum(!dd$climate_event_boundary_complete),
      n_complete_response=nrow(complete),n_incomplete_response=sum(!dd$response_window_complete),
      n_observed_suppressed_all_windows=sum(dd$suppressed==TRUE,na.rm=TRUE),
      n_unassessable_suppression=sum(is.na(dd$suppressed)),
      suppression_probability_complete=if(nrow(complete))mean(complete$suppressed) else NA_real_,
      n_matched_across_all_lags=nrow(matched),
      suppression_probability_matched=if(nrow(matched))mean(matched$suppressed) else NA_real_,
      n_new_onset_among_complete=sum(complete$new_onset_suppression==TRUE,na.rm=TRUE),
      n_complete_with_known_pre_month=sum(is.finite(complete$pre_last_z)),
      median_response_minimum_z=r1c1_q(complete$response_min_z,0.5),
      median_suppression_magnitude=r1c1_q(complete[suppressed==TRUE,suppression_magnitude],0.5),
      n_recovery_eligible=sum(dd$recovery_eligible),
      legacy_all_event_observed_only_median_onset=r1c1_q(dd$legacy_all_event_recovery_onset,0.5)
    )
    for (cohort in c('all_suppressed','new_onset_suppressed','matched_all_lags_suppressed')) {
      d <- dd[recovery_eligible==TRUE]
      if (cohort=='new_onset_suppressed') d<-d[new_onset_suppression==TRUE]
      if (cohort=='matched_all_lags_suppressed') d<-d[matched_all_lags_complete==TRUE]
      eligible_n <- nrow(d); recovered_n<-sum(d$recovery_observed==1L)
      ir<-ir+1L
      row <- data.table::data.table(
        cohort=cohort,response_lag=lag,elev_band=band,elevation=labels[band+1L],
        n_eligible=eligible_n,n_recovered=recovered_n,n_censored=eligible_n-recovered_n,
        fraction_observed_recovered=if(eligible_n)recovered_n/eligible_n else NA_real_,
        median_observed_only_onset_from_event_end=r1c1_q(d$recovery_onset_months_from_event_end,0.5),
        km_median_confirmation_from_landmark=NA_real_,
        km_median_not_reached=NA,km_probability_month_12=NA_real_,aj_probability_month_12=NA_real_)
      for (reason in reasons) row[,(paste0('n_censored_',reason)):=sum(d$censor_reason==reason)]
      stopifnot(recovered_n+sum(vapply(reasons,function(r)sum(d$censor_reason==r),integer(1)))==eligible_n)
      if (eligible_n) {
        fit <- r1c1_curves(d,cfg$recovery_horizon,cohort,lag,band,labels[band+1L])
        row[,km_median_confirmation_from_landmark:=fit$median]
        row[,km_median_not_reached:=is.na(fit$median)]
        row[,km_probability_month_12:=fit$curve[month_from_landmark==12,km_recovery_probability]]
        row[,aj_probability_month_12:=fit$curve[month_from_landmark==12,aj_recovery_before_next_HD]]
        ic<-ic+1L; curves[[ic]]<-fit$curve
      }
      recovery_tables[[ir]]<-row
    }
  }
  suppression <- data.table::rbindlist(suppression_tables)
  recovery <- data.table::rbindlist(recovery_tables)
  curve_dt <- data.table::rbindlist(curves,fill=TRUE)
  for (lag in cfg$response_lags) {
    stopifnot(suppression[response_lag==lag & elev_band==0,n_events] ==
                suppression[response_lag==lag & elev_band>0,sum(n_events)])
    for (cohort_name in unique(recovery$cohort)) {
      stopifnot(recovery[response_lag==lag & cohort==cohort_name & elev_band==0,n_eligible] ==
                  recovery[response_lag==lag & cohort==cohort_name & elev_band>0,sum(n_eligible)])
    }
  }
  for (band in 0:5) {
    pp <- suppression[elev_band==band][order(response_lag),suppression_probability_matched]
    stopifnot(all(diff(pp)>=-1e-12,na.rm=TRUE))
  }
  data.table::fwrite(suppression,file.path(out,'tables','Suppression_lag_comparison_by_elevation.csv'))
  data.table::fwrite(recovery,file.path(out,'tables','Recovery_censoring_by_elevation.csv'))
  data.table::fwrite(curve_dt,file.path(out,'tables','Recovery_curves_KM_and_competing_HD.csv'))
  exclusions <- dt[, .(n_events=.N),by=.(response_lag,elevation,recovery_exclusion,response_window_status)]
  data.table::fwrite(exclusions,file.path(out,'tables','Recovery_eligibility_and_response_completeness.csv'))
  # Publication figures use COMPLETE windows and retain censored recoveries.
  plot_lag <- suppression[elev_band>0L]
  plot_lag[,elevation:=factor(elevation,levels=labels[-1])]
  palette <- stats::setNames(c('#0072B2','#E69F00','#009E73','#CC79A7','#D55E00'),labels[-1])
  p1 <- ggplot2::ggplot(plot_lag,ggplot2::aes(response_lag,suppression_probability_matched,
                                            colour=elevation,group=elevation))+
    ggplot2::geom_line()+ggplot2::geom_point(size=2)+
    ggplot2::scale_x_continuous(breaks=0:3)+ggplot2::scale_y_continuous(limits=c(0,1))+
    ggplot2::scale_colour_manual(values=palette,drop=FALSE)+
    ggplot2::labs(x='Additional months after HD event end',y='Fraction with NDVI suppression',
                  colour='Elevation',subtitle='Identical events with complete observations for all four windows')+
    ggplot2::theme_bw(base_size=12)
  ggplot2::ggsave(file.path(out,'figures','Suppression_lag_sensitivity.png'),p1,width=9,height=5,dpi=300)
  ggplot2::ggsave(file.path(out,'figures','Suppression_lag_sensitivity.pdf'),p1,width=9,height=5)
  if (nrow(curve_dt)) {
    cp <- curve_dt[cohort=='all_suppressed' & elev_band>0L]
    cp[,elevation:=factor(elevation,levels=labels[-1])]
    cp[,window:=factor(paste0('Event + ',response_lag,' month(s)'),
                        levels=paste0('Event + ',0:3,' month(s)'))]
    p2 <- ggplot2::ggplot(cp,ggplot2::aes(month_from_landmark,km_recovery_probability,
                                          colour=elevation,group=elevation))+
      ggplot2::geom_step()+ggplot2::facet_wrap(~window,ncol=2)+
      ggplot2::scale_y_continuous(limits=c(0,1))+ggplot2::scale_x_continuous(breaks=c(0,3,6,9,12))+
      ggplot2::scale_colour_manual(values=palette,drop=FALSE)+
      ggplot2::labs(x='Months from later of event end and NDVI minimum',y='Probability of confirmed recovery',
                    colour='Elevation',subtitle='Suppressed events; next HD events censored; see risk counts in curve table')+
      ggplot2::theme_bw(base_size=11)
    ggplot2::ggsave(file.path(out,'figures','Recovery_KM_by_elevation.png'),p2,width=10,height=7,dpi=300)
    ggplot2::ggsave(file.path(out,'figures','Recovery_KM_by_elevation.pdf'),p2,width=10,height=7)
    p3 <- ggplot2::ggplot(cp,ggplot2::aes(month_from_landmark,aj_recovery_before_next_HD,
                                          colour=elevation,group=elevation))+
      ggplot2::geom_step()+ggplot2::facet_wrap(~window,ncol=2)+
      ggplot2::scale_y_continuous(limits=c(0,1))+ggplot2::scale_x_continuous(breaks=c(0,3,6,9,12))+
      ggplot2::scale_colour_manual(values=palette,drop=FALSE)+
      ggplot2::labs(x='Months from later of event end and NDVI minimum',y='Recovery before another HD event',
                    colour='Elevation',subtitle='Competing-event sensitivity; another HD event prevents attribution to the first')+
      ggplot2::theme_bw(base_size=11)
    ggplot2::ggsave(file.path(out,'figures','Recovery_competing_HD_by_elevation.png'),p3,width=10,height=7,dpi=300)
    ggplot2::ggsave(file.path(out,'figures','Recovery_competing_HD_by_elevation.pdf'),p3,width=10,height=7)
  }
  cautions <- character()
  if (any(audit$abs_z_over_20_share>0)) cautions<-c(cautions,'Inspect extreme NDVI anomalies in Monthly_input_audit.csv before interpreting results.')
  if (any(audit$missing_share>0.2)) cautions<-c(cautions,'Some months exceed 20 percent missing NDVI on the common grid; inspect the monthly audit.')
  note <- c('Reviewer 1 Comment 1 analysis',paste('Output',out),
            paste('Processed cells',length(selected),'of',length(all_cells)),
            paste('Climate reference',cfg$climate_threshold_reference),paste('NDVI reference',cfg$ndvi_reference),
            'No thresholds or NDVI standardisation were re-estimated in this run.',
            'Suppression means observed greenness below baseline; it does not establish causal damage.',
            'Primary suppression probabilities use complete response windows and observed climatic event boundaries.',
            'Matched lag comparisons use the same events with complete event-plus-3-month windows.',
            'Primary recovery requires observed suppression, observed climatic boundaries, and an assessable recovery landmark.',
            'New-onset sensitivity additionally requires the immediately preceding NDVI month to be nonnegative.',
            'Primary recovery clock starts at the later of climatic event end and the minimum in the response window.',
            'Recovery is recorded at the SECOND nonnegative month, when the two-month condition is confirmed.',
            'First-month return times relative to event end are supplied separately for comparison with the old metric.',
            'The administrative cutoff remains 12 months after climatic event end, even for lagged responses.',
            'Next HD censoring occurs immediately before the next event; both recovery months must precede it.',
            'Unknown climate or missing NDVI conservatively stops attribution/follow-up; no gaps are bridged.',
            'Suppressed events with lag windows truncated by the next HD or dataset end are retained with censoring.',
            'Incomplete windows without observed suppression are unassessable; missingness before the recovery landmark is reported separately.',
            'KM curves assume noninformative censoring; use competing-HD sensitivity when interpreting interruptions.',
            'AJ curves estimate recovery before the next HD event, treating that next event as a competing endpoint.',
            'Confidence intervals use pixel-clustered errors. They do not fully account for correlation between nearby pixels.',
            'Recovery probabilities beyond supported follow-up are NA. A median not reached is NA, not 12 months.',
            'Increasing lag windows also increases the chance of observing a negative anomaly; lag results are sensitivity evidence, not causal attribution.',
            'Existing interpolation cannot be identified without an observation-quality mask. Results concern the supplied NDVI stack.',
            'The legacy all-event observed-only median is a comparison diagnostic and excludes unrecovered events.',
            'Check the monthly audit before using any output. Do not mix full-period inputs with fixed-baseline manuscript results.',
            'Methods references',
            'https://rspatial.github.io/terra/reference/readwrite.html',
            'https://stat.ethz.ch/R-manual/R-devel/RHOME/library/survival/doc/compete.pdf',
            'https://pmc.ncbi.nlm.nih.gov/articles/PMC4741409/',cautions)
  if (cfg$max_cells>0L) note<-c(note,'THIS IS A SOFTWARE SMOKE TEST. Rerun with max_cells = 0 for research results.')
  writeLines(note,file.path(out,'00_READ_ME_FIRST.txt'))
  writeLines(c('Changes for Reviewer 1 Comment 1',
               'Added event-plus-0/1/2/3-month response windows without changing exposure flags.',
               'Added complete-window and common-event denominators, antecedent-state diagnostics, and new-onset sensitivity.',
               'Restricted primary recovery to observed suppressed events; retained interrupted windows and explicit censoring durations/reasons.',
               'Required two observed nonnegative months and used confirmation timing for survival analysis.',
               'Added pixel-clustered KM curves and competing-HD cumulative-incidence sensitivity.',
               'Added monthly input audit, source checksums, unit self-tests, and outcome reconciliation checks.',
               'Open items: inspect full-run outputs; select response framing; update manuscript and response only after verification.'),
             file.path(out,'CHANGES_v1.txt'))
  writeLines(capture.output(sessionInfo()),file.path(out,'logs','sessionInfo.txt'))
  cat('\nDONE. Outputs:',out,'\n')
  if (length(cautions)) cat(paste(cautions,collapse='\n'),'\n')
  invisible(list(output_dir=out,suppression=suppression,recovery=recovery,curves=curve_dt))
}

# Test-only mode is useful for validating logic without reading project data.
# Example: options(r1c1.test_only=TRUE); source('this_file.R')
if (isTRUE(getOption('r1c1.test_only',FALSE))) {
  r1c1_self_tests()
} else {
  # The override is intended for a small software smoke test, not research.
  cfg <- utils::modifyList(R1C1_SETTINGS,getOption('r1c1.settings',list()))
  R1C1_RESULT <- r1c1_main(cfg)
}
