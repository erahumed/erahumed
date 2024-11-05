hbp_ditch_inflow_pct <- function(ditch, area)
{
  res <- stats::aggregate(area ~ ditch, FUN = sum)
  res$area <- res$area / sum(res$area)
  names(res)[names(res) == "area"] <- "inflow_pct"
  res <- res[order(res$ditch), ]
  return(res)
}

hbp_simulate_ditch <- function(
    ideal_height_eod_cm,
    ideal_irrigation,
    ideal_draining,
    petp_cm,
    area_m2,
    capacity_m3_s,
    date,
    cluster_id,
    ideal_flow_rate_cm,
    height_thresh_cm,
    ...
    )
{
  df_list <-
    hbp_make_df_list(
      ideal_height_eod_cm = ideal_height_eod_cm,
      ideal_irrigation = ideal_irrigation,
      ideal_draining = ideal_draining,
      petp_cm = petp_cm,
      area_m2 = area_m2,
      capacity_m3_s = capacity_m3_s,
      date = date,
      cluster_id = cluster_id,
      ...
    ) |>
    lapply(unclass)

  randomize_clusters <- Sys.getenv("erahumed_randomize_clusters", "TRUE") |>
    as.logical()

  for (j in seq_along(df_list)) {
    args <- hbp_extract_daily_inputs(df_list, j)
    args <- c(args,
              list(ideal_flow_rate_cm = ideal_flow_rate_cm,
                   height_thresh_cm = height_thresh_cm,
                   randomize_clusters = randomize_clusters)
    )
    update_j <- do.call(hbp_daily_step, args)

    df_list[[j]][names(df_list[[j]]) %in% names(update_j)] <- NULL
    df_list[[j]] <- c(df_list[[j]], update_j)
  }

  df_list <- lapply(df_list, `class<-`, "data.frame")

  return(df_list)
}

hbp_make_df_list <- function(
    ideal_height_eod_cm,
    ideal_irrigation,
    ideal_draining,
    petp_cm,
    area_m2,
    capacity_m3_s,
    date,
    cluster_id,
    ...
)
{
  data.frame(
    ideal_height_eod_cm,
    ideal_irrigation,
    ideal_draining,
    plan_delay = 0,
    petp_cm,
    height_eod_cm = ideal_height_eod_cm,
    area_m2,
    capacity_m3_s,
    date,
    cluster_id,
    ...
  ) |>
    data.table::setorder(date, cluster_id) |>
    collapse::rsplit(
      by = ~ date,
      flatten = FALSE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    )
}

hbp_extract_daily_inputs <- function(df_list, j) {
  current <- df_list[[j]]
  previous <- if (j > 1) df_list[[j - 1]] else current

  plan_delay_lag <- previous$plan_delay
  ideal_height_eod_cm <- vapply(
    seq_along(plan_delay_lag),
    \(c) { df_list[[j - plan_delay_lag[c]]]$ideal_height_eod_cm[c] },
    numeric(1)
    )
  irrigation <- vapply(
    seq_along(plan_delay_lag),
    \(c) { df_list[[j - plan_delay_lag[c]]]$ideal_irrigation[c] },
    logical(1)
    )

  draining <- vapply(
    seq_along(plan_delay_lag),
    \(c) { df_list[[j - plan_delay_lag[c]]]$ideal_draining[c] },
    logical(1)
  )

  height_sod_cm <- previous$height_eod_cm

  list(
    height_sod_cm = height_sod_cm,
    ideal_height_eod_cm = ideal_height_eod_cm,
    petp_cm = current$petp_cm,
    irrigation = irrigation,
    draining = draining,
    area_m2 = current$area,
    capacity_m3_s = current$capacity_m3_s[[1]],
    date = current$date,
    plan_delay_lag = plan_delay_lag
  )
}

hbp_daily_step <- function(
    height_sod_cm,
    date,
    plan_delay_lag,
    ideal_height_eod_cm,
    petp_cm,
    irrigation,
    draining,
    area_m2,
    capacity_m3_s,
    ideal_flow_rate_cm,
    height_thresh_cm,
    randomize_clusters
)
{
  # Including these two columns here is a relatively clean way to force them in
  # the data.frame output of the calling function.
  l <- list(height_sod_cm = height_sod_cm,
            irrigation = irrigation,
            draining = draining)

  l <- c(l, hbp_ideal_diff_flow_cm(ideal_height_eod_cm = ideal_height_eod_cm,
                                   height_sod_cm = height_sod_cm,
                                   petp_cm = petp_cm))

  l <- c(l, hbp_ideal_flows_cm(ideal_diff_flow_cm = l$ideal_diff_flow_cm,
                               irrigation = irrigation,
                               draining = draining,
                               ideal_flow_rate_cm = ideal_flow_rate_cm))

  l <- c(l, hbp_real_outflow_m3_s(ideal_outflow_cm = l$ideal_outflow_cm,
                                  area_m2 = area_m2,
                                  capacity_m3_s = capacity_m3_s,
                                  randomize_clusters = randomize_clusters))

  l <- c(l, hbp_real_outflow_cm(real_outflow_m3_s = l$real_outflow_m3_s,
                                area_m2 = area_m2))

  l <- c(l, hbp_real_inflow_cm(real_outflow_cm = l$real_outflow_cm,
                               ideal_diff_flow_cm = l$ideal_diff_flow_cm))

  l <- c(l, hbp_real_inflow_m3_s(real_inflow_cm = l$real_inflow_cm,
                                 area_m2 = area_m2))

  l <- c(l, hbp_height_eod_cm(height_sod_cm = height_sod_cm,
                               petp_cm = petp_cm,
                               real_inflow_cm = l$real_inflow_cm,
                               real_outflow_cm = l$real_outflow_cm))

  l <- c(l, hbp_plan_delay(plan_delay_lag = plan_delay_lag,
                           ideal_height_eod_cm = ideal_height_eod_cm,
                           height_eod_cm = l$height_eod_cm,
                           date = date,
                           height_thresh_cm = height_thresh_cm))

  return(l)

}


hbp_ideal_diff_flow_cm <- function(
    ideal_height_eod_cm,
    height_sod_cm,
    petp_cm
    )
{
  res <- ideal_height_eod_cm - pmax2(height_sod_cm + petp_cm, 0)
  return( list(ideal_diff_flow_cm = res) )
}

hbp_ideal_flows_cm <- function(
    ideal_diff_flow_cm,
    irrigation,
    draining,
    ideal_flow_rate_cm
    )
{
  is_flux <- irrigation * draining
  ideal_inflow_cm <-
    is_flux * ideal_flow_rate_cm +
    (1-is_flux) * pmax2(ideal_diff_flow_cm, 0)
  ideal_outflow_cm <- -ideal_diff_flow_cm + ideal_inflow_cm

  ideal_inflow_cm <- ideal_inflow_cm - pmin2(ideal_outflow_cm, 0)
  ideal_outflow_cm <- pmax2(ideal_outflow_cm, 0)

  return( list(ideal_inflow_cm = ideal_inflow_cm,
               ideal_outflow_cm = ideal_outflow_cm) )
}

hbp_real_outflow_m3_s <- function(
    ideal_outflow_cm,
    area_m2,
    capacity_m3_s,
    randomize_clusters = TRUE
    )
{
  n <- length(ideal_outflow_cm)

  ord <- if (randomize_clusters) { sample(n) } else { 1:n }

  ideal_outflow_m3_s <- (ideal_outflow_cm / 100) * area_m2 / s_per_day()
  real_outflow_m3_s <- numeric(n)

  cum_outflows <- pmin2(cumsum(c(0, ideal_outflow_m3_s[ord])), capacity_m3_s)
  real_outflow_m3_s[ord] <- diff(cum_outflows)
  capacity_m3_s <- capacity_m3_s - cum_outflows[length(cum_outflows)]

  real_outflow_m3_s <- real_outflow_m3_s + capacity_m3_s / length(real_outflow_m3_s)

  return( list(real_outflow_m3_s = real_outflow_m3_s) )
}

hbp_real_outflow_cm <- function(real_outflow_m3_s, area_m2) {
  list( real_outflow_cm = m3_s_to_cm_day(real_outflow_m3_s, area_m2) )
}

hbp_real_inflow_cm <- function(real_outflow_cm, ideal_diff_flow_cm) {
  list( real_inflow_cm = pmax2(real_outflow_cm + ideal_diff_flow_cm, 0) )
}

hbp_real_inflow_m3_s <- function(real_inflow_cm, area_m2) {
  list(real_inflow_m3_s = cm_day_to_m3_s(real_inflow_cm, area_m2))
}

hbp_height_eod_cm <- function(
    height_sod_cm,
    petp_cm,
    real_inflow_cm,
    real_outflow_cm
    )
{
  real_diff_flow_cm <- real_inflow_cm - real_outflow_cm
  res <- pmax2(height_sod_cm + petp_cm, 0) + real_diff_flow_cm
  return( list(height_eod_cm = res) )
}

hbp_plan_delay <- function(
    plan_delay_lag,
    ideal_height_eod_cm,
    height_eod_cm,
    date,
    height_thresh_cm,
    mm_dd_start = c(4, 20),
    mm_dd_end = c(10, 15)
    )
{
  date <- as.POSIXlt(date[1])
  reset_delay <-
    (get_mm(date) < mm_dd_start[1]) ||
    (get_mm(date) == mm_dd_start[1] && get_dd(date) < mm_dd_start[2]) ||
    (get_mm(date) > mm_dd_end[1]) ||
    (get_mm(date) == mm_dd_end[1] && get_dd(date) > mm_dd_end[2])

  if (reset_delay)
    return( list(plan_delay = numeric(length(plan_delay_lag))) )

  add_delay <- (ideal_height_eod_cm == 0) & (height_eod_cm > height_thresh_cm)
  return( list(plan_delay = plan_delay_lag + add_delay) )
}

