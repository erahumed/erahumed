#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. Check the
#' [main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
#' for a detailed description of the simulation workflow.
#'
#' @param date_start `r input_roxy("date_start")`
#' @param date_end `r input_roxy("date_end")`
#' @param outflows_df `r input_roxy("outflows_df")`
#' @param weather_df `r input_roxy("weather_df")`
#' @param variety_prop `r input_roxy("variety_prop")`
#' @param storage_curve_slope_m2 `r input_roxy("storage_curve_slope_m2")`
#' @param storage_curve_intercept_m3 `r input_roxy("storage_curve_intercept_m3")`
#' @param petp_surface_m2 `r input_roxy("petp_surface_m2")`
#' @param management_df `r input_roxy("management_df")`
#' @param ideal_flow_rate_cm `r input_roxy("ideal_flow_rate_cm")`
#' @param height_thresh_cm `r input_roxy("height_thresh_cm")`
#' @param ditch_level_m `r input_roxy("ditch_level_m")`
#' @param ca_schedules_df `r input_roxy("ca_schedules_df")`
#' @param drift `r input_roxy("drift")`
#' @param covmax `r input_roxy("covmax")`
#' @param jgrow `r input_roxy("jgrow")`
#' @param SNK `r input_roxy("SNK")`
#' @param dact_m `r input_roxy("dact_m")`
#' @param css_ppm `r input_roxy("css_ppm")`
#' @param foc `r input_roxy("foc")`
#' @param bd_g_cm3 `r input_roxy("bd_g_cm3")`
#' @param qseep_m_day `r input_roxy("qseep_m_day")`
#' @param wilting `r input_roxy("wilting")`
#' @param fc `r input_roxy("fc")`
#' @param seed `r input_roxy("seed")`
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
  outflows_df = erahumed::albufera_outflows,
  weather_df = erahumed::albufera_weather,
  variety_prop = c("J.Sendra" = 0.8, "Bomba" = 0.1, "Clearfield" = 0.1),
  storage_curve_slope_m2 = 63.086 * 1e6,
  storage_curve_intercept_m3 = 25.58 * 1e6,
  petp_surface_m2 = 53.9 * 1e6,
  management_df = erahumed::albufera_management,
  ideal_flow_rate_cm = 5,
  height_thresh_cm = 0.5,
  ditch_level_m = 1,
  ca_schedules_df = erahumed::albufera_ca_schedules,
  drift = 0,
  covmax = 0.5,
  jgrow = 152,
  SNK = 0,
  dact_m = 0.1,
  css_ppm = 50,
  foc = 0.17,
  bd_g_cm3 = 1.5,
  qseep_m_day = 0,
  wilting = 0.24,
  fc = 0.35,
  seed = 840
  )
{
  tryCatch(
    {
      assert_date(date_start)
      assert_date(date_end)
      if (as.Date(date_start) > as.Date(date_end)) {
        stop("'date_start' must be earlier than or equal to 'date_end'.")
      }

      outflows_df_template <- erahumed::albufera_outflows
      assert_data.frame(outflows_df, template = outflows_df_template)

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
        msg <- paste(
          "Input data for the specified date interval is incomplete. ",
          "Please check the 'date_start'/'date_end' parameters'",
          "and the 'outflows_df' and 'weather_df' data.frames."
        )
        stop(msg)
      }

      if (any(diff(weather_df$date) != 1)) {
        stop("Invalid 'date' domain in 'weather_df' (not an interval)." )
      }

      assert_positive_vector(variety_prop)
      stopifnot(length(variety_prop) == 3)

      assert_positive_number(storage_curve_slope_m2)

      assert_positive_number(storage_curve_intercept_m3)

      assert_positive_number(petp_surface_m2)

      assert_positive_number(ditch_level_m)

      management_df_temp <- erahumed::albufera_management
      assert_data.frame(management_df, template = management_df_temp)
      dates <- paste(management_df$mm, management_df$dd)
      dates_expected <- paste(management_df_temp$mm, management_df_temp$dd)
      if (!setequal(unique(dates), unique(dates_expected))) {
        msg <- paste(
          "Provided 'management_df' has unexpected 'date' values.",
          "Values should coincide with 'erahumed::albufera_management$date'"
        )
        stop(msg)
      }

      assert_positive_number(ideal_flow_rate_cm)

      assert_positive_number(height_thresh_cm)

    },
    error = function(e) {
      class(e) <- c("erahumed_input_error", class(e))
      stop(e)
    })


  res <- initialize_erahumed_simulation(inputs = as.list(environment()))

  res$etc$cluster_variety_map <-
    withr::with_seed(seed, generate_clusters_variety(variety_prop))

  res |>
    compute_inp() |>
    compute_hbl() |>
    compute_hbc() |>
    compute_hbd() |>
    compute_ca() |>
    compute_ctc() |>
    compute_ctd() |>
    compute_ctl() |>
    compute_rc() |>
    compute_rd() |>
    compute_rl()
}

initialize_erahumed_simulation <- function(inputs)
{
  structure(list(inputs = inputs, outputs = list(), etc = list()),
            class = "erahumed_simulation")
}

is_erahumed_simulation <- function(obj) {
  is.list(obj) &&
    inherits(obj, "erahumed_simulation") &&

    hasName(obj, "inputs") &&
    is.list(obj[["inputs"]]) &&

    hasName(obj, "etc") &&
    is.list(obj[["etc"]]) &&

    hasName(obj, "outputs") &&
    is.list(obj[["outputs"]]) &&
    all(sapply(obj[["outputs"]], is.data.frame))
}

#' @export
print.erahumed_simulation <- function(x, ...) {
  cat("A ERAHUMED simulation.\n\n")
  cat("Start date:", get_input(x, "date_start"), "\n")
  cat("End date:", get_input(x, "date_end"), "\n")
  rlang::inform(

    c("", i = "Need help extracting simulation outputs? Check `?get_results`."),
    .frequency = "once",
    .frequency_id = "erahumed_get_results"
    )

  return(invisible(x))
}

#' @export
summary.erahumed_simulation <- function(object, ...) {
  print(object)
}
