# Reviewer 1 Comment 2 v2: month-specific reference eligibility.
# Earlier strict-domain outputs are diagnostic; this revision preserves NA.
# Cells are not excluded solely because another calendar month is unassessable.
# Uses the EXISTING monthly datasets; originals are never overwritten.
# Run this ONE file in a fresh R session: source(file.choose())
# No interpolation or missing-value replacement is performed.

R1C2_SETTINGS <- list(
  input_dir = 'E:/proj-china/AdnanB/Dataset/netcdfs',
  ndvi_file = 'ndvi_monthly_1985_2025.nc',
  temperature_file = 'tc_tmx_monthly_1985_2025.nc',
  precipitation_file = 'tc_pr_monthly_1985_2025.nc',
  elevation_file = 'analysis_outputs/elevation_projected_to_ndviGrid.tif',
  output_parent = 'E:/proj-china/AdnanB/Dataset/netcdfs/analysis_outputs',
  rows_per_block = 10L,
  # Every calendar-month reference needs >=80% of its years observed.
  minimum_reference_fraction = 0.80,
  minimum_sd = 1e-6,
  detrend_minimum_years = 20L,
  response_lags = 0:3,
  recovery_horizon = 12L,
  recovery_consecutive_months = 2L,
  antecedent_months = 6L,
  # 0 = full analysis. A positive number is ONLY a software smoke test.
  max_cells = 0L
)

# The following event and survival functions are embedded from the validated
# Comment 1 script, so this file requires no second local script.

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
  # One right-censored endpoint record per event; pixel is the variance cluster.
  stopifnot(!anyDuplicated(d$event_uid))
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
  # Unique one-row event IDs were checked above: each can belong to only
  # one pixel cluster. Some survival validation helpers emit a contrary
  # warning for this valid input. Muffle only that contradicted warning.
  aj <- withCallingHandlers(survival::survfit(survival::Surv(competing_time, endpoint) ~ 1,
                          data=d, id=event_uid,cluster=if(enough_clusters)d$cell_id else NULL,
                          robust=TRUE,se.fit=enough_clusters),warning=function(w) {
    if(identical(conditionMessage(w),'an id value appears on more than one cluster'))
      invokeRestart('muffleWarning')
  })
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
  # The supplied elevation raster explicitly defines the original footprint.
  # Do not require January NDVI anomalies to be estimable at every cell.
  domain <- is.finite(ev)
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
  stem <- paste0('Event_Response_',stamp,if(cfg$max_cells>0L)'_SMOKE' else '')
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
    ggplot2::geom_line(na.rm=TRUE)+ggplot2::geom_point(size=2,na.rm=TRUE)+
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
      ggplot2::geom_step(na.rm=TRUE)+ggplot2::facet_wrap(~window,ncol=2)+
      ggplot2::scale_y_continuous(limits=c(0,1))+ggplot2::scale_x_continuous(breaks=c(0,3,6,9,12))+
      ggplot2::scale_colour_manual(values=palette,drop=FALSE)+
      ggplot2::labs(x='Months from later of event end and NDVI minimum',y='Probability of confirmed recovery',
                    colour='Elevation',subtitle='Suppressed events; next HD events censored; see risk counts in curve table')+
      ggplot2::theme_bw(base_size=11)
    ggplot2::ggsave(file.path(out,'figures','Recovery_KM_by_elevation.png'),p2,width=10,height=7,dpi=300)
    ggplot2::ggsave(file.path(out,'figures','Recovery_KM_by_elevation.pdf'),p2,width=10,height=7)
    p3 <- ggplot2::ggplot(cp,ggplot2::aes(month_from_landmark,aj_recovery_before_next_HD,
                                          colour=elevation,group=elevation))+
      ggplot2::geom_step(na.rm=TRUE)+ggplot2::facet_wrap(~window,ncol=2)+
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
  note <- c('Lag and censored-recovery analysis within the named Comment 2 framework',paste('Output',out),
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



r1c2_references <- list(H14=1985:2014, H04=1985:2004, FULL=1985:2025)

r1c2_frameworks <- function() {
  # Matching references compare whole frameworks. The four extra rows
  # isolate climate versus NDVI choices against the primary framework.
  data.table::data.table(
    framework_id=c('PRIMARY_H14_ORIGINAL','S_H14_DETRENDED',
                   'S_MATCH_H04_ORIGINAL','S_MATCH_H04_DETRENDED',
                   'S_MATCH_FULL_ORIGINAL','S_MATCH_FULL_DETRENDED',
                   'S_CLIMATE_H04','S_CLIMATE_FULL','S_NDVI_H04','S_NDVI_FULL'),
    climate_reference=c('H14','H14','H04','H04','FULL','FULL','H04','FULL','H14','H14'),
    ndvi_reference=c('H14','H14','H04','H04','FULL','FULL','H14','H14','H04','FULL'),
    ndvi_processing=c('original','detrended','original','detrended','original','detrended',
                      'original','original','original','original'),
    role=c('PRIMARY',rep('SENSITIVITY',9)),
    comparison=c('Historical seasonal departure','Remove secular trend',
                 'Shorter matched historical references','Shorter matched references plus detrending',
                 'Full-period matched references','Full-period matched references plus detrending',
                 'Change climate reference only','Change climate reference only',
                 'Change NDVI reference only','Change NDVI reference only')
  )
}

r1c2_mean_sd <- function(x, required, minimum_sd) {
  ok <- is.finite(x); n <- rowSums(ok)
  x[!ok] <- 0
  mu <- rowSums(x)/pmax(n,1)
  delta <- sweep(x,1,mu,'-'); delta[!ok] <- 0
  s <- sqrt(rowSums(delta^2)/pmax(n-1,1))
  valid <- n >= required & is.finite(s) & s > minimum_sd
  mu[!valid] <- s[!valid] <- NA_real_
  list(mean=mu,sd=s,n=n,valid=valid)
}

r1c2_quantile <- function(x, p, required) {
  n <- rowSums(is.finite(x))
  val <- apply(x,1,function(z) {
    z <- z[is.finite(z)]
    if (length(z) < required) NA_real_ else
      as.numeric(stats::quantile(z,p,names=FALSE,type=7))
  })
  list(value=val,n=n,valid=is.finite(val))
}

r1c2_detrend <- function(x, years, required) {
  # Descriptive sensitivity: separate OLS for each pixel/calendar month,
  # fitted over all observed years 1985-2025. Residuals retain original NA.
  tt <- years-2005
  ok <- is.finite(x); n <- rowSums(ok)
  xx <- x; xx[!ok] <- 0
  sx <- rowSums(xx); st <- as.numeric(ok %*% tt)
  stt <- as.numeric(ok %*% (tt^2)); stx <- as.numeric(xx %*% tt)
  den <- stt-st^2/pmax(n,1)
  slope <- (stx-st*sx/pmax(n,1))/den
  intercept <- (sx-slope*st)/pmax(n,1)
  valid <- n >= required & is.finite(slope) & den > 0
  slope[!valid] <- intercept[!valid] <- NA_real_
  residual <- x - (outer(intercept,rep(1,length(tt))) + outer(slope,tt))
  residual[!ok] <- NA_real_
  list(residual=residual,slope=slope,intercept=intercept,n=n,valid=valid)
}

r1c2_flags <- function(tmx, pr, hot, dry) {
  # Unknown status stays NA even when one known component is non-extreme.
  z <- (tmx > hot) & (pr < dry)
  z[!is.finite(tmx) | !is.finite(pr) | !is.finite(hot) | !is.finite(dry)] <- NA
  z*1L
}

r1c2_self_tests <- function() {
  r1c1_self_tests()
  x <- matrix(c(1,2,NA,4, 2,2,2,2),2,byrow=TRUE)
  a <- r1c2_mean_sd(x,3,1e-6)
  stopifnot(a$n[1]==3,a$mean[1]==7/3,is.na(a$sd[2]))
  stopifnot(is.na(r1c2_quantile(x,.95,4)$value[1]))
  yrs <- 1985:2025
  y <- matrix(0.2+0.002*(yrs-2005)+sin(seq_along(yrs))/100,1)
  y[1,10:12] <- NA
  d <- r1c2_detrend(y,yrs,20)
  fit <- stats::lm(as.numeric(y)~I(yrs-2005),na.action=stats::na.exclude)
  stopifnot(all(is.na(d$residual)==is.na(y)),
            max(abs(d$residual-as.numeric(stats::residuals(fit))),na.rm=TRUE)<1e-12)
  f <- r1c2_flags(c(2,2,2,NA,0),c(0,1,NA,0,NA),1,1)
  stopifnot(identical(as.numeric(f),c(1,0,NA,NA,NA)))
  # Strict ties do not qualify; genuine zeros remain observed values.
  stopifnot(r1c2_flags(1,0,1,1)==0,r1c2_flags(2,0,1,0)==0)
  cat('PASS: baseline statistics, detrending, NA preservation and strict threshold tests.\n')
}

r1c2_trend <- function(year,value) {
  good <- is.finite(year)&is.finite(value); yr<-year[good]; y<-value[good]
  n <- length(y)
  if(n<6) return(list(n_years=n,theil_sen_per_year=NA_real_,ols_per_year=NA_real_,
                     hac_lower95=NA_real_,hac_upper95=NA_real_,hac_p=NA_real_))
  X <- cbind(1,yr-mean(yr)); fit <- stats::lm.fit(X,y)
  u <- fit$residuals; xu <- X*u
  meat <- crossprod(xu)
  lag <- min(3L,n-2L)
  for(k in seq_len(lag)) {
    cross <- crossprod(xu[(k+1):n,,drop=FALSE],xu[1:(n-k),,drop=FALSE])
    meat <- meat + (1-k/(lag+1))*(cross+t(cross))
  }
  bread <- solve(crossprod(X))
  variance <- n/(n-2)*(bread%*%meat%*%bread)
  se <- sqrt(max(variance[2,2],0)); slope <- fit$coefficients[2]
  slopes <- unlist(lapply(seq_len(n-1),function(i)(y[(i+1):n]-y[i])/(yr[(i+1):n]-yr[i])))
  list(n_years=n,theil_sen_per_year=median(slopes),ols_per_year=unname(slope),
       hac_lower95=unname(slope-1.96*se),hac_upper95=unname(slope+1.96*se),
       hac_p=if(se>0)2*stats::pnorm(-abs(slope/se)) else NA_real_)
}

r1c2_preprocess <- function(cfg,out,paths,dates) {
  refs <- r1c2_references; keys <- names(refs)
  src <- lapply(paths,terra::rast)
  y <- src$ndvi_file; tmx <- src$temperature_file; pr <- src$precipitation_file
  ev <- as.numeric(terra::values(src$elevation_file))
  base <- is.finite(ev)&is.finite(as.numeric(terra::values(y[[1]])))
  selected <- which(base)
  if(cfg$max_cells>0 && length(selected)>cfg$max_cells)
    selected<-selected[unique(round(seq(1,length(selected),length.out=cfg$max_cells)))]
  keep <- seq_along(base)%in%selected
  months<-as.integer(format(dates,'%m')); years<-as.integer(format(dates,'%Y'))
  nT<-length(dates); nr<-terra::nrow(y); nc<-terra::ncol(y)
  stats_names <- unlist(lapply(1:12,function(m) paste0(
    c('tmx_p95','pr_p10','ndvi_mean','ndvi_sd','residual_mean','residual_sd',
      'n_tmx','n_pr','n_ndvi'),'_m',sprintf('%02d',m))))
  writers<-list(); files<-list()
  open_writer<-function(key,nlayers,names_,time_=NULL) {
    z<-terra::rast(y,nlyrs=nlayers); names(z)<-names_
    if(!is.null(time_))terra::time(z)<-time_
    path<-file.path(out,'rasters',paste0(key,'.tif'))
    terra::writeStart(z,path,overwrite=FALSE,datatype='FLT8S',gdal=c('COMPRESS=DEFLATE','BIGTIFF=YES'))
    writers[[key]]<<-z; files[[key]]<<-path
  }
  for(k in keys) {
    open_writer(paste0('NDVI_',k,'_original'),nT,paste0('z_',format(dates,'%Y_%m')),dates)
    open_writer(paste0('NDVI_',k,'_detrended'),nT,paste0('z_',format(dates,'%Y_%m')),dates)
    open_writer(paste0('HD_',k),nT,paste0('HD_',format(dates,'%Y_%m')),dates)
    open_writer(paste0('Reference_statistics_',k),length(stats_names),stats_names)
  }
  open_writer('NDVI_calendar_month_trend',24,c(paste0('slope_m',1:12),paste0('n_fit_m',1:12)))
  finished<-FALSE
  on.exit({if(!finished)for(z in writers)try(terra::writeStop(z),silent=TRUE)},add=TRUE)
  for(z in src[1:3])terra::readStart(z)
  on.exit({for(z in src[1:3])terra::readStop(z)},add=TRUE)
  common<-rep(FALSE,length(base)); strict_common<-common
  baseline_inventory<-list(); ii<-0L
  audit_count<-audit_sum<-audit_zero<-audit_bad<-matrix(0,3,nT)
  missing_loss<-matrix(0,6,nT,dimnames=list(unlist(lapply(keys,function(k)paste0(k,c('_original','_detrended')))),NULL))
  baseline_unestimable<-missing_loss
  ties<-list(); it<-0L
  rows<-seq(1,nr,by=cfg$rows_per_block)
  for(b in seq_along(rows)) {
    row<-rows[b]; nrows<-min(cfg$rows_per_block,nr-row+1)
    ids<-seq((row-1)*nc+1,(row+nrows-1)*nc)
    raw<-lapply(src[1:3],function(z)terra::readValues(z,row=row,nrows=nrows,mat=TRUE))
    ym<-raw$ndvi_file; tm<-raw$temperature_file; pm<-raw$precipitation_file
    ym[!is.finite(ym)]<-NA_real_; tm[!is.finite(tm)]<-NA_real_; pm[!is.finite(pm)]<-NA_real_
    ia<-which(base[ids]); jj<-which(keep[ids]); count<-length(ids)
    if(length(ia))for(v in 1:3) {
      x<-raw[[v]][ia,,drop=FALSE]; ok<-is.finite(x)
      audit_count[v,]<-audit_count[v,]+colSums(ok)
      audit_sum[v,]<-audit_sum[v,]+colSums(replace(x,!ok,0))
      audit_zero[v,]<-audit_zero[v,]+colSums(ok&x==0)
      if(v==1)audit_bad[v,]<-audit_bad[v,]+colSums(ok&(x < -1 | x > 1))
      if(v==3)audit_bad[v,]<-audit_bad[v,]+colSums(ok&x<0)
    }
    z_out<-list(); flags<-list(); param<-list()
    for(k in keys) {
      z_out[[paste0(k,'_original')]]<-z_out[[paste0(k,'_detrended')]]<-matrix(NA_real_,count,nT)
      flags[[k]]<-matrix(NA_real_,count,nT)
      param[[k]]<-matrix(NA_real_,count,length(stats_names))
    }
    trend<-matrix(NA_real_,count,24)
    valid<-rep(TRUE,length(jj)); reference_valid<-setNames(lapply(keys,function(k)rep(TRUE,length(jj))),keys)
    primary_month_count<-all_framework_month_count<-integer(length(jj))
    if(length(jj))for(m in 1:12) {
      ix<-which(months==m)
      yy<-ym[jj,ix,drop=FALSE]; tt<-tm[jj,ix,drop=FALSE]; pp<-pm[jj,ix,drop=FALSE]
      dt<-r1c2_detrend(yy,years[ix],cfg$detrend_minimum_years)
      month_common<-rep(TRUE,length(jj))
      trend[jj,m]<-dt$slope; trend[jj,m+12]<-dt$n
      for(k in keys) {
        ref<-which(years[ix]%in%refs[[k]]); needed<-ceiling(length(ref)*cfg$minimum_reference_fraction)
        ys<-r1c2_mean_sd(yy[,ref,drop=FALSE],needed,cfg$minimum_sd)
        rs<-r1c2_mean_sd(dt$residual[,ref,drop=FALSE],needed,cfg$minimum_sd)
        hs<-r1c2_quantile(tt[,ref,drop=FALSE],.95,needed)
        ds<-r1c2_quantile(pp[,ref,drop=FALSE],.10,needed)
        vv<-ys$valid&rs$valid&hs$valid&ds$valid&dt$valid
        reference_valid[[k]]<-reference_valid[[k]]&vv; valid<-valid&vv
        month_common<-month_common&vv
        if(k=='H14')primary_month_count<-primary_month_count+as.integer(ys$valid&hs$valid&ds$valid)
        z_out[[paste0(k,'_original')]][jj,ix]<-sweep(sweep(yy,1,ys$mean,'-'),1,ys$sd,'/')
        z_out[[paste0(k,'_detrended')]][jj,ix]<-sweep(sweep(dt$residual,1,rs$mean,'-'),1,rs$sd,'/')
        hot<-matrix(hs$value,nrow=length(jj),ncol=length(ix))
        dry<-matrix(ds$value,nrow=length(jj),ncol=length(ix))
        flags[[k]][jj,ix]<-r1c2_flags(tt,pp,hot,dry)
        param[[k]][jj,(m-1)*9+1:9]<-cbind(hs$value,ds$value,ys$mean,ys$sd,rs$mean,rs$sd,hs$n,ds$n,ys$n)
        # A diagnostic only; inclusive thresholds are never used in frameworks.
        strict<-r1c2_flags(tt,pp,hot,dry)
        inclusive<-(tt>=hot)&(pp<=dry)
        known<-is.finite(tt)&is.finite(pp)&is.finite(hot)&is.finite(dry)
        it<-it+1L; ties[[it]]<-data.table::data.table(reference=k,month=m,
          n_known=sum(known),n_strict=sum(strict==1,na.rm=TRUE),
          n_inclusive=sum(inclusive&known,na.rm=TRUE),
          n_zero_dry_threshold=sum(ds$value==0,na.rm=TRUE))
      }
      all_framework_month_count<-all_framework_month_count+as.integer(month_common)
    }
    strict_common[ids[jj]]<-valid
    common[ids[jj]]<-TRUE
    ii<-ii+1L
    baseline_inventory[[ii]]<-data.table::data.table(cell_id=ids[jj],elevation_m=ev[ids[jj]],
      eligible_H14=reference_valid$H14,eligible_H04=reference_valid$H04,
      eligible_FULL=reference_valid$FULL,strict_all_months_eligible=valid,
      original_footprint=TRUE,n_primary_eligible_calendar_months=primary_month_count,
      n_all_frameworks_eligible_calendar_months=all_framework_month_count)
    not_good<-which(!common[ids])
    for(key in names(z_out)) {
      z<-z_out[[key]]; z[not_good,]<-NA_real_
      if(length(jj)) {
        good<-jj
        missing_loss[key,]<-missing_loss[key,]+colSums(!is.finite(ym[good,,drop=FALSE]) & is.finite(z[good,,drop=FALSE]))
        baseline_unestimable[key,]<-baseline_unestimable[key,]+colSums(is.finite(ym[good,,drop=FALSE]) & !is.finite(z[good,,drop=FALSE]))
      }
      terra::writeValues(writers[[paste0('NDVI_',key)]],z,row,nrows)
    }
    for(k in keys) {
      f<-flags[[k]]; f[not_good,]<-NA_real_
      terra::writeValues(writers[[paste0('HD_',k)]],f,row,nrows)
      terra::writeValues(writers[[paste0('Reference_statistics_',k)]],param[[k]],row,nrows)
    }
    terra::writeValues(writers$NDVI_calendar_month_trend,trend,row,nrows)
    cat(sprintf('Reference block %d/%d; original-footprint cells retained: %d\n',b,length(rows),sum(common)))
  }
  for(z in writers)terra::writeStop(z)
  finished<-TRUE
  if(!any(common))stop('No cells in the original footprint.')
  stopifnot(all(missing_loss==0))
  mask_elev<-src$elevation_file; vv<-ev; vv[!common]<-NA_real_
  terra::values(mask_elev)<-vv
  files$elevation<-file.path(out,'rasters','elevation_common_framework_domain.tif')
  terra::writeRaster(mask_elev,files$elevation,overwrite=FALSE,datatype='FLT8S')
  inv<-data.table::rbindlist(baseline_inventory)
  data.table::fwrite(inv,file.path(out,'tables','Baseline_eligibility_by_cell.csv'),na='NA')
  data.table::fwrite(data.table::data.table(initial_domain=sum(base),selected_for_preprocessing=sum(keep),
    common_domain=sum(common),excluded_reference_statistics=0L,
    n_old_strict_domain=sum(strict_common),
    n_any_primary_reference_month=sum(inv$n_primary_eligible_calendar_months>0),
    n_any_all_frameworks_reference_month=sum(inv$n_all_frameworks_eligible_calendar_months>0),
    eligibility_policy='calendar-month statistics; event-specific assessability',
    smoke_test=cfg$max_cells>0),file.path(out,'tables','Common_domain_inventory.csv'))
  inv[,elev_band:=findInterval(elevation_m,c(500,1500,2500,3500))+1L]
  retention<-inv[,.(n_original_footprint=.N,n_old_strict_domain=sum(strict_all_months_eligible),
    n_any_primary_month=sum(n_primary_eligible_calendar_months>0),
    n_any_all_frameworks_month=sum(n_all_frameworks_eligible_calendar_months>0)),by=elev_band]
  data.table::fwrite(retention,file.path(out,'tables','Month_specific_retention_by_elevation.csv'))
  audit<-data.table::rbindlist(lapply(1:3,function(v)data.table::data.table(
    variable=c('NDVI','temperature','precipitation')[v],date=as.character(dates),
    n_domain=sum(base),n_observed=audit_count[v,],missing_fraction=1-audit_count[v,]/sum(base),
    mean=ifelse(audit_count[v,]>0,audit_sum[v,]/pmax(audit_count[v,],1),NA_real_),
    zero_count=audit_zero[v,],out_of_range_count=audit_bad[v,])))
  data.table::fwrite(audit,file.path(out,'tables','Raw_monthly_input_audit.csv'),na='NA')
  gap<-audit[variable=='NDVI'&date%in%as.character(as.Date(c('1994-10-01','1994-11-01','1994-12-01')))]
  if(nrow(gap)!=3 || any(gap$n_observed!=0))stop('Expected 1994 gap differs from source audit. Investigate inputs before continuing.')
  loss<-data.table::rbindlist(lapply(rownames(missing_loss),function(k)data.table::data.table(
    anomaly=k,date=as.character(dates),n_missing_values_replaced=missing_loss[k,])))
  data.table::fwrite(loss,file.path(out,'tables','NA_preservation_check.csv'))
  unresolved<-data.table::rbindlist(lapply(rownames(baseline_unestimable),function(k)data.table::data.table(
    anomaly=k,date=as.character(dates),n_observed_raw_but_reference_unestimable=baseline_unestimable[k,])))
  data.table::fwrite(unresolved,file.path(out,'tables','Observed_NDVI_with_unestimable_reference.csv'))
  tie<-data.table::rbindlist(ties)[,lapply(.SD,sum),by=.(reference,month)]
  data.table::fwrite(tie,file.path(out,'tables','Threshold_ties_and_zero_rainfall_audit.csv'))
  list(files=files,common=common,elevation=ev,dates=dates)
}

r1c2_exposure <- function(pre,out) {
  labels<-c('National','<500 m','500-1499 m','1500-2499 m','2500-3499 m','>=3500 m')
  ids<-which(pre$common); bands<-findInterval(pre$elevation[ids],c(500,1500,2500,3500))+1L
  area<-as.numeric(terra::values(terra::cellSize(terra::rast(pre$files$elevation),unit='km')))[ids]
  years<-as.integer(format(pre$dates,'%Y')); annual<-list(); ai<-0L
  flag_mats<-list()
  for(k in names(r1c2_references)) {
    # Reading whole compact flag stacks once avoids band-by-band IO.
    f<-terra::values(terra::rast(pre$files[[paste0('HD_',k)]]),mat=TRUE)[ids,,drop=FALSE]
    flag_mats[[k]]<-f
    for(b in 0:5) {
      sel<-if(b==0)seq_along(ids) else which(bands==b)
      w<-area[sel]; g<-f[sel,,drop=FALSE]; total<-sum(w)
      for(yr in sort(unique(years))) {
        ix<-which(years==yr); z<-g[,ix,drop=FALSE]; known<-is.finite(z)
        complete<-rowSums(known)==length(ix); on<-rowSums(z==1,na.rm=TRUE)>0
        known_area_months<-sum(w*rowSums(known)); hd_area_months<-sum(w*rowSums(z==1,na.rm=TRUE))
        area_complete<-sum(w[complete]); ai<-ai+1L
        annual[[ai]]<-data.table::data.table(climate_reference=k,elev_band=b,elevation=labels[b+1],year=yr,
          n_cells=length(sel),domain_area_km2=total,complete_year_area_km2=area_complete,
          complete_year_area_fraction=if(total>0)area_complete/total else NA_real_,
          hd_area_km2_complete=sum(w[complete&on]),
          hd_area_fraction_complete=if(area_complete>0)sum(w[complete&on])/area_complete else NA_real_,
          hd_area_months_km2=hd_area_months,known_climate_area_months_km2=known_area_months,
          hd_area_month_fraction=if(known_area_months>0)hd_area_months/known_area_months else NA_real_)
      }
    }
  }
  ann<-data.table::rbindlist(annual)
  data.table::fwrite(ann,file.path(out,'tables','Annual_climate_exposure_by_reference.csv'),na='NA')
  tr<-data.table::rbindlist(lapply(c('hd_area_fraction_complete','hd_area_month_fraction'),function(metric) {
    ans<-ann[,r1c2_trend(year,get(metric)),by=.(climate_reference,elev_band,elevation)]
    ans[,metric:=metric]; ans
  }))
  tr[,uncertainty_method:='OLS Newey-West HAC lag 3, normal 95% CI; descriptive Theil-Sen slope']
  data.table::fwrite(tr,file.path(out,'tables','Exposure_trend_comparison.csv'),na='NA')
  agree<-data.table::rbindlist(lapply(c('H04','FULL'),function(k) {
    a<-flag_mats$H14; b<-flag_mats[[k]]; known<-is.finite(a)&is.finite(b)
    data.table::data.table(alternative=k,n_joint_known=sum(known),n_changed=sum(known&a!=b),
      fraction_changed=if(sum(known))sum(known&a!=b)/sum(known) else NA_real_)
  }))
  data.table::fwrite(agree,file.path(out,'tables','Climate_flag_agreement_with_primary.csv'),na='NA')
  p<-ggplot2::ggplot(ann[elev_band==0],ggplot2::aes(year,100*hd_area_fraction_complete,colour=climate_reference))+
    ggplot2::geom_line(na.rm=TRUE)+ggplot2::labs(x='Year',y='Area with at least one hot-dry month (%)',
      colour='Climate reference',subtitle='Same spatial domain; denominator has all 12 climate months observed')+
    ggplot2::theme_bw(base_size=11)
  ggplot2::ggsave(file.path(out,'figures','Climate_reference_exposure.png'),p,width=9,height=5,dpi=300)
  invisible(list(annual=ann,trends=tr))
}

r1c2_compare_shared_events <- function(frameworks,run_paths,out) {
  # Event_uid from Comment 1 is a within-framework counter; use actual
  # cell/start/end/lag keys instead when comparing different references.
  fields<-c('cell_id','start_index','end_index','response_lag','event_uid','elev_band',
            'elevation','response_window_complete','climate_event_boundary_complete','suppressed',
            'recovery_eligible','recovery_observed','followup_months','competing_time','competing_status')
  sum_out<-list(); rec_out<-list(); is<-ir<-0L
  for(cr in unique(frameworks$climate_reference)) {
    ff<-frameworks[climate_reference==cr]; if(nrow(ff)<2)next
    records<-lapply(ff$framework_id,function(id) {
      path<-file.path(run_paths[[id]],'tables','HD_event_records_all_lags.csv')
      if(!file.exists(path))return(NULL)
      z<-data.table::fread(path,select=fields,na.strings='NA')
      z[,framework_id:=id];z
    })
    if(any(vapply(records,is.null,logical(1))))next
    d<-data.table::rbindlist(records)
    ids<-d[,.(n_frameworks=.N,
      all_complete=all(response_window_complete&climate_event_boundary_complete),
      all_suppressed_eligible=all(suppressed%in%TRUE&recovery_eligible)),
      by=.(cell_id,start_index,end_index,response_lag)]
    stopifnot(all(ids$n_frameworks==nrow(ff)))
    d<-merge(d,ids,by=c('cell_id','start_index','end_index','response_lag'))
    for(id in ff$framework_id)for(lag in 0:3)for(b in 0:5) {
      z<-d[framework_id==id & response_lag==lag];if(b>0)z<-z[elev_band==b]
      m<-z[all_complete==TRUE]
      is<-is+1L;sum_out[[is]]<-data.table::data.table(framework_id=id,climate_reference=cr,
        response_lag=lag,elev_band=b,n_same_complete_events=nrow(m),
        fraction_suppressed=if(nrow(m))mean(m$suppressed) else NA_real_)
      r<-z[all_suppressed_eligible==TRUE]
      med<-prob<-lo<-hi<-NA_real_
      if(nrow(r)) {
        fit<-r1c1_curves(r,12,'suppressed_in_every_NDVI_framework',lag,b,if(b==0)'National' else unique(r$elevation)[1])
        med<-fit$median; cv<-fit$curve[month_from_landmark==6]
        prob<-cv$km_recovery_probability;lo<-cv$km_lower;hi<-cv$km_upper
      }
      ir<-ir+1L;rec_out[[ir]]<-data.table::data.table(framework_id=id,climate_reference=cr,
        response_lag=lag,elev_band=b,n_same_suppressed_eligible_events=nrow(r),
        km_median_confirmation_from_landmark=med,km_probability_month6=prob,km_lower_month6=lo,km_upper_month6=hi)
    }
    rm(d,records);gc(verbose=FALSE)
  }
  data.table::fwrite(data.table::rbindlist(sum_out),file.path(out,'tables','Same_event_NDVI_suppression_comparison.csv'),na='NA')
  data.table::fwrite(data.table::rbindlist(rec_out),file.path(out,'tables','Same_suppressed_cohort_recovery_comparison.csv'),na='NA')
}

r1c2_recovery_comparison <- function(r,cv,fw,out) {
  # Six months gives a common clock time; intervals still depend on supported
  # follow-up. Never replace unsupported estimates or unreached medians.
  six<-cv[month_from_landmark==6,.(framework_id,cohort,response_lag,elev_band,
    km_probability_month6=km_recovery_probability,km_lower_month6=km_lower,
    km_upper_month6=km_upper,aj_probability_month6=aj_recovery_before_next_HD,
    n_at_risk_month6=n_at_risk)]
  rr<-merge(r,six,by=c('framework_id','cohort','response_lag','elev_band'),all.x=TRUE)
  pr<-rr[framework_id=='PRIMARY_H14_ORIGINAL',.(cohort,response_lag,elev_band,
    primary_eligible=n_eligible,primary_km_probability_month6=km_probability_month6,
    primary_km_median=km_median_confirmation_from_landmark)]
  rr<-merge(rr,pr,by=c('cohort','response_lag','elev_band'),all.x=TRUE)
  rr[,km_month6_difference_percentage_points:=100*(km_probability_month6-primary_km_probability_month6)]
  rr[,eligible_cohort_difference:=n_eligible-primary_eligible]
  data.table::fwrite(merge(rr,fw,by='framework_id'),
    file.path(out,'tables','Recovery_differences_from_primary.csv'),na='NA')
}

r1c2_main <- function(cfg=R1C2_SETTINGS) {
  needed<-c('terra','data.table','ggplot2','survival')
  absent<-needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]
  if(length(absent))stop('Install packages first: install.packages(c(',paste(sprintf('"%s"',absent),collapse=','),'))')
  suppressPackageStartupMessages({library(terra);library(data.table);library(ggplot2);library(survival)})
  r1c2_self_tests()
  stopifnot(cfg$minimum_reference_fraction>0,cfg$minimum_reference_fraction<=1,cfg$rows_per_block>0,
            cfg$max_cells>=0,identical(as.integer(cfg$response_lags),0:3),cfg$recovery_horizon==12,
            cfg$recovery_consecutive_months==2)
  names_in<-c('ndvi_file','temperature_file','precipitation_file','elevation_file')
  paths<-vapply(names_in,function(n) {
    p<-cfg[[n]];if(grepl('^([A-Za-z]:|/)',p))p else file.path(cfg$input_dir,p)
  },character(1))
  if(any(!file.exists(paths)))stop('Missing inputs: ',paste(paths[!file.exists(paths)],collapse='\n'))
  src<-lapply(paths,terra::rast)
  for(z in src[-1])terra::compareGeom(src[[1]][[1]],z[[1]],stopOnError=TRUE)
  dates<-as.Date(seq(as.Date('1985-01-01'),as.Date('2025-12-01'),by='month'))
  for(z in src[1:3]) {
    stopifnot(terra::nlyr(z)==length(dates))
    td<-terra::time(z)
    if(!inherits(td,c('Date','POSIXt')) || anyNA(td) ||
       !identical(as.Date(paste0(format(td,'%Y-%m'),'-01')),dates))stop('Input date axis must be Jan 1985-Dec 2025, in order.')
  }
  stamp<-format(Sys.time(),'%Y%m%d_%H%M%S')
  stem<-paste0('09_R1C2_Monthwise_v2_',stamp,if(cfg$max_cells>0)'_SMOKE' else '')
  out<-file.path(cfg$output_parent,stem); v<-1L
  while(dir.exists(out)){v<-v+1L;out<-file.path(cfg$output_parent,paste0(stem,'_v',v))}
  dir.create(out,recursive=TRUE)
  for(d in c('tables','figures','logs','rasters','frameworks','terra_tmp'))dir.create(file.path(out,d))
  terra::terraOptions(tempdir=file.path(out,'terra_tmp'),memfrac=.5,progress=0)
  log<-file(file.path(out,'logs','console_log.txt'),'wt');sink(log,split=TRUE)
  on.exit({sink();close(log)},add=TRUE)
  cat('Output folder:',out,'\nExisting datasets only. Oct-Dec 1994 remain NA.\n')
  if(cfg$max_cells>0)cat('SMOKE TEST ONLY: these outputs are not research results.\n')
  writeLines(capture.output(dput(cfg)),file.path(out,'logs','settings.R'))
  data.table::fwrite(data.table::data.table(input=names(paths),path=paths,
    bytes=file.info(paths)$size,md5=unname(tools::md5sum(paths))),file.path(out,'tables','Input_inventory.csv'))
  fw<-r1c2_frameworks()
  fw[,climate_years:=vapply(climate_reference,function(k)paste(range(r1c2_references[[k]]),collapse='-'),character(1))]
  fw[,ndvi_years:=vapply(ndvi_reference,function(k)paste(range(r1c2_references[[k]]),collapse='-'),character(1))]
  data.table::fwrite(fw,file.path(out,'tables','Framework_manifest.csv'))
  # Save methods at the start so a failed run is still documented.
  writeLines(c('Reviewer 1 Comment 2 baseline comparison',paste('Output:',out),
    'Primary: fixed 1985-2014 pixel/calendar-month temperature P95, rainfall P10 and original NDVI mean/SD.',
    'Quantiles use R type 7. Hot-dry uses temperature > P95 AND rainfall < P10. Threshold ties do not qualify.',
    'Consecutive qualifying months form events. Unknown climate remains NA, never a known non-event.',
    'NDVI z = (observed NDVI - calendar-month reference mean) / reference sample SD.',
    'Each reference needs at least 80% of its calendar-month years; SD must exceed 1e-6. No SD clamping.',
    'Detrended sensitivity: per-pixel/calendar-month linear fit over observed 1985-2025, at least 20 years.',
    'Its residuals are standardized using residual mean/SD within the named NDVI reference period.',
    'Detrending uses later observations and is descriptive, not a prospective prediction or source harmonization.',
    'All ten frameworks share the original first-raw-NDVI/elevation footprint. Climate exposure is not masked by NDVI completeness.',
    'Reference adequacy is evaluated per calendar month, without requiring all 12 months to pass at each cell.',
    'Source missingness and unestimable monthly reference statistics both yield NA; these are audited separately.',
    'Complete/shared event windows determine vegetation response eligibility. Retained pixels are not all eligible for every event.',
    'Baseline calculations omit missing observations only to estimate statistics; monthly missing observations remain NA.',
    'October-December 1994 are verified fully missing. No zero fill or interpolation; missing follow-up censors recovery.',
    'Every framework repeats the Comment 1 lag windows 0/1/2/3 and two-month confirmed recovery.',
    'Recovery clock starts later of event end and NDVI minimum; administrative end is 12 months after event end.',
    'Non-recovered and interrupted eligible events remain censored. A median not reached stays NA.',
    'Longer response windows and negative NDVI do not establish causal vegetation damage.',
    'Recovery summaries condition on observed suppression; cohorts can change with NDVI processing.',
    'Additional shared-event and shared-suppressed-cohort tables isolate NDVI choices under identical climate events.',
    'Exposure trends use area-weighted annual metrics; HAC lag 3 intervals are approximate. Inspect coverage before trend claims.',
    'This script evaluates the supplied monthly series; NDVI product provenance remains to be documented separately.',
    'Do not mix results from different frameworks. Review sensitivity direction changes before claiming robustness.',
    'Primary framework identification is a methods choice, not a guarantee of significance.',
    'No manuscript or response result wording is automatically changed.',
    'Methods documentation: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html',
    'https://rspatial.github.io/terra/reference/readwrite.html',
    'https://stat.ethz.ch/R-manual/R-devel/RHOME/library/survival/doc/compete.pdf'),file.path(out,'00_READ_ME_FIRST.txt'))
  pre<-r1c2_preprocess(cfg,out,paths,dates)
  r1c2_exposure(pre,out)
  run_paths<-list(); supp<-rec<-curves<-list()
  for(i in seq_len(nrow(fw))) {
    f<-fw[i];cat('\nFRAMEWORK',i,'/',nrow(fw),f$framework_id,'\n')
    rcfg<-list(input_dir=out,
      ndvi_file=pre$files[[paste0('NDVI_',f$ndvi_reference,'_',f$ndvi_processing)]],
      hd_file=pre$files[[paste0('HD_',f$climate_reference)]],elevation_file=pre$files$elevation,
      climate_threshold_reference=paste(f$climate_years,'calendar-month P95/P10; strict >/<'),
      ndvi_reference=paste(f$ndvi_years,f$ndvi_processing,'calendar-month mean and sample SD'),
      output_parent=file.path(out,'frameworks',f$framework_id),framework_id=f$framework_id,
      response_lags=cfg$response_lags,recovery_horizon=cfg$recovery_horizon,
      recovery_consecutive_months=cfg$recovery_consecutive_months,antecedent_months=cfg$antecedent_months,
      rows_per_block=cfg$rows_per_block,max_cells=cfg$max_cells,
      verified_start_date_if_missing=as.character(dates[1]))
    ans<-r1c1_main(rcfg)
    run_paths[[f$framework_id]]<-ans$output_dir
    supp[[i]]<-data.table::copy(ans$suppression)[,framework_id:=f$framework_id]
    rec[[i]]<-data.table::copy(ans$recovery)[,framework_id:=f$framework_id]
    curves[[i]]<-data.table::copy(ans$curves)[,framework_id:=f$framework_id]
    rm(ans);gc(verbose=FALSE)
  }
  s<-data.table::rbindlist(supp,fill=TRUE);r<-data.table::rbindlist(rec,fill=TRUE)
  cv<-data.table::rbindlist(curves,fill=TRUE)
  data.table::fwrite(merge(s,fw,by='framework_id'),file.path(out,'tables','All_frameworks_suppression.csv'),na='NA')
  data.table::fwrite(merge(r,fw,by='framework_id'),file.path(out,'tables','All_frameworks_recovery_censoring.csv'),na='NA')
  data.table::fwrite(cv,file.path(out,'tables','All_frameworks_recovery_curves.csv'),na='NA')
  r1c2_recovery_comparison(r,cv,fw,out)
  data.table::fwrite(data.table::data.table(framework_id=names(run_paths),output_dir=unlist(run_paths)),
    file.path(out,'tables','Framework_output_paths.csv'))
  # Differences from primary are descriptive; event/cohort denominators can differ.
  ps<-s[framework_id=='PRIMARY_H14_ORIGINAL',.(response_lag,elev_band,
    primary_fraction=suppression_probability_matched,primary_events=n_events)]
  diff<-merge(s,ps,by=c('response_lag','elev_band'))
  diff[,suppression_difference_percentage_points:=100*(suppression_probability_matched-primary_fraction)]
  diff[,event_count_difference:=n_events-primary_events]
  data.table::fwrite(diff,file.path(out,'tables','Suppression_differences_from_primary.csv'),na='NA')
  r1c2_compare_shared_events(fw,run_paths,out)
  p<-ggplot2::ggplot(s[elev_band==0],ggplot2::aes(response_lag,100*suppression_probability_matched,
      colour=framework_id,group=framework_id))+
    ggplot2::geom_line(na.rm=TRUE)+ggplot2::geom_point(na.rm=TRUE)+ggplot2::scale_x_continuous(breaks=0:3)+
    ggplot2::labs(x='Additional months after event end',y='Suppressed events (%)',colour='Framework',
      subtitle='Matched across lag windows within each framework; event sets can differ across climate references')+
    ggplot2::theme_bw(base_size=10)
  ggplot2::ggsave(file.path(out,'figures','National_suppression_framework_comparison.png'),p,width=12,height=6,dpi=300)
  writeLines(c('Changes for Reviewer 1 Comment 2',
    'Recomputed climate thresholds and original/detrended NDVI anomalies from existing monthly inputs.',
    'Defined one primary fixed 1985-2014 framework and nine explicitly labelled sensitivity frameworks.',
    'Standardized strict threshold inequalities, quantile type, date checks, spatial mask and missing-data rules.',
    'Preserved NA months and added checks for the original 1994 gap and zero replaced missing observations.',
    'Repeated the validated lag/censored recovery analysis within every framework.',
    'Added climate/NDVI changes separately, shared-event comparisons, baseline eligibility and threshold-tie audits.',
    'Original datasets, scripts and earlier outputs are unchanged.',
    'Changed eligibility from whole-cell exclusion across all months to calendar-month reference adequacy.',
    'Kept the original 80% monthly reference-count rule, SD criterion, detrending rule and event definitions.',
    'Climate exposure uses the fixed original spatial footprint; NDVI response windows remain observation-limited.',
    'Open: inspect full-run results; verify product descriptions; fill response placeholders after numerical checks.'),
    file.path(out,'CHANGES_v1.txt'))
  writeLines(capture.output(sessionInfo()),file.path(out,'logs','sessionInfo.txt'))
  writeLines('All ten frameworks completed; combined tables and shared-event comparisons written.',file.path(out,'logs','COMPLETE.txt'))
  cat('\nDONE. Reviewer 1 Comment 2 outputs:',out,'\n')
  if(cfg$max_cells>0)cat('SMOKE TEST ONLY. Run with max_cells=0 for the full analysis.\n')
  invisible(list(output_dir=out,frameworks=fw))
}

if(isTRUE(getOption('r1c2.test_only',FALSE))) {
  r1c2_self_tests()
} else {
  cfg<-utils::modifyList(R1C2_SETTINGS,getOption('r1c2.settings',list()))
  r1c2_warning_messages<-character()
  R1C2_RESULT<-withCallingHandlers(r1c2_main(cfg),warning=function(w) {
    r1c2_warning_messages<<-c(r1c2_warning_messages,conditionMessage(w))
  })
  writeLines(if(length(r1c2_warning_messages))unique(r1c2_warning_messages) else 'No warnings.',
    file.path(R1C2_RESULT$output_dir,'logs','warnings_at_completion.txt'))
}
