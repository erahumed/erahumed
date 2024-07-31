simulate_lhb <- function(
    ideal_height_cm,
    irrigation,
    draining,
    petp_cm,
    area_m2,
    capacity_m3_s,
    date,
    cluster_id,
    ideal_flow_rate_cm = 5,
    ...)
{

  df_list <-
    make_lhb_df_list(
      ideal_height_cm = ideal_height_cm,
      irrigation = irrigation,
      draining = draining,
      petp_cm = petp_cm,
      area_m2 = area_m2,
      capacity_m3_s = capacity_m3_s,
      date = date,
      cluster_id,
      ...
      ) |>
    lapply(unclass)

  randomize_clusters <- Sys.getenv("erahumed_randomize_clusters", "TRUE") |>
    as.logical()

  for (j in seq_along(df_list)) {
    args <- extract_lhb_daily_inputs(df_list, j)
    args <- c(args,
              list(ideal_flow_rate_cm = ideal_flow_rate_cm,
                   randomize_clusters = randomize_clusters)
              )
    update_j <- do.call(simulate_lhb_daily_step, args)

    df_list[[j]][names(df_list[[j]]) %in% names(update_j)] <- NULL
    df_list[[j]] <- c(df_list[[j]], update_j)
  }

  df_list <- lapply(df_list, `class<-`, "data.frame")

  return(df_list)
}

extract_lhb_daily_inputs <- function(df_list, j) {
  current <- df_list[[j]]
  previous <- if (j > 1) df_list[[j - 1]] else current

  plan_delay_lag <- previous$plan_delay
  ideal_height_cm <- vapply(
    seq_along(plan_delay_lag),
    \(c) { df_list[[j - plan_delay_lag[c]]]$ideal_height_cm[c] },
    numeric(1)
  )
  real_height_cm_lag <- previous$real_height_cm

  list(
    real_height_cm_lag = real_height_cm_lag,
    ideal_height_cm = ideal_height_cm,
    petp_cm = current$petp_cm,
    irrigation = current$irrigation,
    draining = current$draining,
    area_m2 = current$area,
    capacity_m3_s = current$capacity_m3_s[[1]],
    date = current$date,
    plan_delay_lag = plan_delay_lag
  )
}

make_lhb_df_list <- function(
  ideal_height_cm,
  irrigation,
  draining,
  petp_cm,
  area_m2,
  capacity_m3_s,
  date,
  cluster_id,
  ...
  )
{
  data.frame(
    ideal_height_cm,
    irrigation,
    draining,
    plan_delay = 0,
    petp_cm,
    real_height_cm = ideal_height_cm,
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


simulate_lhb_daily_step <- function(
    real_height_cm_lag,
    date,
    plan_delay_lag,
    ideal_height_cm,
    petp_cm,
    irrigation,
    draining,
    area_m2,
    capacity_m3_s,
    ideal_flow_rate_cm = 5,
    randomize_clusters
    )
{
  . <- list()

  . <- c(., compute_ideal_diff_flow_cm(ideal_height_cm = ideal_height_cm,
                                    real_height_cm_lag = real_height_cm_lag,
                                    petp_cm = petp_cm))

  . <- c(., compute_ideal_flows_cm(ideal_diff_flow_cm = .$ideal_diff_flow_cm,
                                irrigation = irrigation,
                                draining = draining,
                                ideal_flow_rate_cm = ideal_flow_rate_cm))

  . <- c(., compute_real_outflow_m3_s(ideal_outflow_cm = .$ideal_outflow_cm,
                                      area_m2 = area_m2,
                                      capacity_m3_s = capacity_m3_s,
                                      randomize_clusters = randomize_clusters))

  . <- c(., compute_real_outflow_cm(real_outflow_m3_s = .$real_outflow_m3_s,
                                    area_m2 = area_m2))

  . <- c(., compute_real_inflow_cm(real_outflow_cm = .$real_outflow_cm,
                                   ideal_diff_flow_cm = .$ideal_diff_flow_cm))

  . <- c(., compute_real_inflow_m3_s(real_inflow_cm = .$real_inflow_cm,
                                     area_m2 = area_m2))

  . <- c(., compute_real_height_cm(real_height_cm_lag = real_height_cm_lag,
                                   petp_cm = petp_cm,
                                   real_inflow_cm = .$real_inflow_cm,
                                   real_outflow_cm = .$real_outflow_cm))

  . <- c(., compute_plan_delay(plan_delay_lag = plan_delay_lag,
                               ideal_height_cm = ideal_height_cm,
                               real_height_cm = .$real_height_cm,
                               date,
                               thresh = 2.5))

  return(.)

}


compute_ideal_diff_flow_cm <- function(
    ideal_height_cm,
    real_height_cm_lag,
    petp_cm
    )
{
  res <- ideal_height_cm - pmax2(real_height_cm_lag + petp_cm, 0)
  return( list(ideal_diff_flow_cm = res) )
}

compute_ideal_flows_cm <- function(
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
               ideal_outflow_cm = ideal_outflow_cm)
          )
}

compute_real_outflow_m3_s <- function(
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

compute_real_outflow_cm <- function(
    real_outflow_m3_s,
    area_m2
    )
{
  list( real_outflow_cm = m3_s_to_cm_day(real_outflow_m3_s, area_m2) )
}


compute_real_inflow_cm <- function(
    real_outflow_cm,
    ideal_diff_flow_cm
    )
{
  list(real_inflow_cm = pmax2(real_outflow_cm + ideal_diff_flow_cm, 0))
}

compute_real_inflow_m3_s <- function(
    real_inflow_cm,
    area_m2
    )
{
  list(real_inflow_m3_s = cm_day_to_m3_s(real_inflow_cm, area_m2))
}

compute_real_height_cm <- function(
    real_height_cm_lag,
    petp_cm,
    real_inflow_cm,
    real_outflow_cm
    )
{
  res <- pmax2(real_height_cm_lag + petp_cm, 0) +
    real_inflow_cm - real_outflow_cm
  return( list(real_height_cm = res) )
}

compute_plan_delay <- function(
    plan_delay_lag,
    ideal_height_cm,
    real_height_cm,
    date,
    thresh = 2.5,
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

  if (reset_delay) {
    return( list(plan_delay = numeric(length(plan_delay_lag))) )
  }

  add_delay <- (ideal_height_cm == 0) & (real_height_cm > thresh)
  return( list(plan_delay = plan_delay_lag + add_delay) )
}
