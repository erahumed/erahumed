#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. Check the
#' [main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
#' for a detailed description of the simulation workflow.
#'
#' @param date_start `r input_roxy("date_start")`
#' @param date_end `r input_roxy("date_end")`
#' @param cluster_map `r input_roxy("cluster_map")`
#' @param outflows_df `r input_roxy("outflows_df")`
#' @param weather_df `r input_roxy("weather_df")`
#' @param storage_curve_slope_m2 `r input_roxy("storage_curve_slope_m2")`
#' @param storage_curve_intercept_m3 `r input_roxy("storage_curve_intercept_m3")`
#' @param petp_surface_m2 `r input_roxy("petp_surface_m2")`
#' @param ideal_flow_rate_cm `r input_roxy("ideal_flow_rate_cm")`
#' @param height_thresh_cm `r input_roxy("height_thresh_cm")`
#' @param ditch_level_m `r input_roxy("ditch_level_m")`
#' @param drift `r input_roxy("drift")`
#' @param covmax `r input_roxy("covmax")`
#' @param jgrow `r input_roxy("jgrow")`
#' @param dact_m `r input_roxy("dact_m")`
#' @param css_ppm `r input_roxy("css_ppm")`
#' @param foc_ss `r input_roxy("foc_ss")`
#' @param foc_sed `r input_roxy("foc_sed")`
#' @param bd_g_cm3 `r input_roxy("bd_g_cm3")`
#' @param qseep_m_day `r input_roxy("qseep_m_day")`
#' @param porosity `r input_roxy("porosity")`
#' @param seed `r input_roxy("seed")`
#' @param .progress A function used to report simulation progress.
#'   It should accept a single character string as input, representing the
#'   current stage of the simulation (e.g., `"Computing hydrology: lake"`).
#'   This parameter is primarily intended for user feedback mechanisms (e.g.,
#'   console messages). By default, it prints a message to the R console.
#'
#' @return An object of class `erahumed_simulation`.
#'
#' @examples
#' erahumed_simulation()
#'
#' @export
erahumed_simulation <- function(
    date_start = "2020-01-01",
    date_end = "2020-12-31",
    cluster_map = default_cluster_map(seed = seed),
    outflows_df = erahumed::albufera_outflows,
    weather_df = erahumed::albufera_weather,
    storage_curve_slope_m2 = 23.66 * 1e6,
    storage_curve_intercept_m3 = 16.75 * 1e6,
    petp_surface_m2 = 53.9 * 1e6,
    ideal_flow_rate_cm = 5,
    height_thresh_cm = 0.5,
    ditch_level_m = 1,
    drift = 0,
    covmax = 0.5,
    jgrow = 152,
    dact_m = 0.1,
    css_ppm = 50,
    foc_ss = 0.1,
    foc_sed = 0.05,
    bd_g_cm3 = 1.5,
    qseep_m_day = 0,
    porosity = 0.11,
    seed = 840,
    .progress = message
)
{
  tryCatch(
    {
      assert_date(date_start)
      assert_date(date_end)
      if (as.Date(date_start) > as.Date(date_end)) {
        stop("'date_start' must be earlier than or equal to 'date_end'.")
      }

      assert_data.frame(outflows_df, template = erahumed::albufera_outflows)

      assert_data.frame(weather_df, template = erahumed::albufera_weather)

      # Check that consecutive date differences are all equal to one
      if (any(diff(outflows_df$date) != 1)) {
        stop("Invalid 'date' domain in 'outflows_df' (not an interval).")
      }
      if (any(diff(weather_df$date) != 1)) {
        stop("Invalid 'date' domain in 'weather_df' (not an interval)." )
      }

      if (
        date_start < min(c(outflows_df$date, weather_df$date)) ||
        date_end > max(c(outflows_df$date, weather_df$date))
      ) {
        stop(paste(
          "Input data for the specified date interval is incomplete. ",
          "Please check the 'date_start'/'date_end' parameters'",
          "and the 'outflows_df' and 'weather_df' data.frames."
        ))
      }

      if (any(diff(weather_df$date) != 1)) {
        stop("Invalid 'date' domain in 'weather_df' (not an interval)." )
      }

      assert_positive_number(storage_curve_slope_m2)

      assert_positive_number(storage_curve_intercept_m3)

      assert_positive_number(petp_surface_m2)

      assert_positive_number(ditch_level_m)
      assert_positive_number(ideal_flow_rate_cm)

      assert_positive_number(height_thresh_cm)

      assert_fraction(drift)
      assert_fraction(covmax)
      assert_positive_integer(jgrow)
      assert_positive_number(dact_m)
      assert_positive_number(css_ppm)
      assert_fraction(foc_ss)
      assert_fraction(foc_sed)
      assert_positive_number(bd_g_cm3)
      assert_positive_number(qseep_m_day)
      assert_fraction(porosity)
    },
    error = function(e) {
      class(e) <- c("erahumed_input_error", class(e))
      stop(e)
    })


  .progress("Initializing inputs")

  res <- initialize_erahumed_simulation(list(
    date_start = date_start,
    date_end = date_end,
    cluster_map = cluster_map,
    outflows_df = outflows_df,
    weather_df = weather_df,
    storage_curve_slope_m2 = storage_curve_slope_m2,
    storage_curve_intercept_m3 = storage_curve_intercept_m3,
    petp_surface_m2 = petp_surface_m2,
    ideal_flow_rate_cm = ideal_flow_rate_cm,
    height_thresh_cm = height_thresh_cm,
    ditch_level_m = ditch_level_m,
    drift = drift,
    covmax = covmax,
    jgrow = jgrow,
    dact_m = dact_m,
    css_ppm = css_ppm,
    foc_ss = foc_ss,
    foc_sed = foc_sed,
    bd_g_cm3 = bd_g_cm3,
    qseep_m_day = qseep_m_day,
    porosity = porosity,
    seed = seed))

  res$etc$management_df <- get_management_df(cluster_map)
  res$etc$chemical_db <- get_chemical_db(cluster_map)
  res$etc$applications_df <- get_applications_df(cluster_map)

  res <- compute_inp(res)

  .progress("Computing hydrology: lake")
  res <- compute_hbl(res)

  .progress("Computing hydrology: clusters")
  res <- compute_hbc(res)

  .progress("Computing hydrology: ditches")
  res <- compute_hbd(res)


  if (length(res$etc$chemical_db) > 0) {
    .progress("Computing exposure: clusters")
    res <- compute_ctc(res)

    .progress("Computing exposure: ditches")
    res <- compute_ctd(res)

    .progress("Computing exposure: lake")
    res <- compute_ctl(res)

    .progress("Computing risk: clusters")
    res <- compute_rc(res)

    .progress("Computing risk: ditches")
    res <- compute_rd(res)

    .progress("Computing risk: lake")
    res <- compute_rl(res)

  }


  # TODO: msg_callback could print execution time

  return(res)

}

initialize_erahumed_simulation <- function(inputs)
{
  structure(list(inputs = inputs, outputs = list(), etc = list()),
            class = "erahumed_simulation")
}

is_erahumed_simulation <- function(obj) {
  is.list(obj) &&
    inherits(obj, "erahumed_simulation") &&

    utils::hasName(obj, "inputs") &&
    is.list(obj[["inputs"]]) &&

    utils::hasName(obj, "etc") &&
    is.list(obj[["etc"]]) &&

    utils::hasName(obj, "outputs") &&
    is.list(obj[["outputs"]]) &&
    all(sapply(obj[["outputs"]], is.data.frame))
}

#' @export
print.erahumed_simulation <- function(x, ...) {
  cat("<ERAHUMED Simulation>\n")

  date_start <- get_input(x, "date_start")
  date_end <- get_input(x, "date_end")
  cat("  Date range             :", date_start, "to", date_end, "\n")

  n_days <- as.integer(as.Date(date_end) - as.Date(date_start)) + 1
  cat("  Simulation days        :", n_days, "\n")

  cluster_map <- get_input(x, "cluster_map")
  if (inherits(cluster_map, "erahumed_cluster_map")) {
    cat("  Clusters               :", nrow(cluster_map$map_df), "\n")
    cat("  Management systems     :", length(cluster_map$rfms_list), "\n")
  }

  chemicals <- x$etc$chemical_db
  if (length(chemicals) > 0) {
    cat("  Chemicals simulated    :", length(chemicals), "\n")
  } else {
    cat("  Chemicals simulated    : None\n")
  }

  applications <- x$etc$applications_df
  if (is.data.frame(applications)) {
    cat("  Total applications     :", nrow(applications), "\n")
  }

  inform_once(
    "\nNeed help extracting simulation outputs? Check `?get_results`.",
    id = "erahumed_get_results"
  )

  invisible(x)
}


#' @export
summary.erahumed_simulation <- function(object, ...) {
  print(object)
}
