#' Define a water management scheme for rice field clusters
#'
#' Creates a schedule of water levels for rice field clusters in the
#' Albufera Natural Park, including irrigation and draining periods during the
#' sowing season and the *Perelloná* flooding event.
#'
#' @param sowing_yday `[integer(1)]` \cr
#' Day of the year marking the start of the sowing season (1–366, assuming a leap year).
#' @param harvesting_yday `[integer(1)]` \cr
#' Day of the year marking the end of the sowing season (1–366, assuming a leap year).
#' @param emptyings_yday `[integer()]` \cr
#' Days of the year (within the sowing season) when fields are scheduled to be drained.
#' @param flow_height_cm `[numeric(1)]` \cr
#' Target water level (in cm) during the regular days of the sowing season,
#' excluding emptying and transition days.
#' @param perellona_start_yday `[integer(1)]` \cr
#' Day of the year marking the beginning of the *Perelloná* flooding period (after harvest).
#' @param perellona_end_yday `[integer(1)]` \cr
#' Day of the year marking the end of the *Perelloná* flooding period (before sowing).
#' @param perellona_height_cm `[numeric(1)]` \cr
#' Target water level (in cm) during the *Perelloná* flooding period.
#'
#' @return (not exported) A side effect: constructs and validates a water level
#' schedule internally.
#'
#' @details
#' This function generates two internal water level schedules:
#' one for a typical rice field cluster (`height_regular`) and one for a cluster
#' that is also flooded during the *Perelloná* (`height_tancat`).
#' These schedules are smoothed for use in simulations of irrigation and drainage operations.
#'
#' @noRd
water_management_scheme <- function(
    sowing_yday = 111,
    harvesting_yday = 251,
    emptyings_yday = c(116:117, 129:130, 163:168),
    flow_height_cm = 10,
    perellona_end_yday = 16,
    perellona_start_yday = 305,
    perellona_height_cm = 20
    )
{
  tryCatch(
    {
      assert_positive_integer(sowing_yday)
      stopifnot(sowing_day < 366)
      assert_positive_integer(harvesting_yday)
      stopifnot(sowing_day < 366)
      stopifnot(sowing_yday < harvesting_yday)

      assert_positive_vector(emptyings_yday)
      if (any(emptyings_yday < sowing_yday) | any(emptyings_yday > harvesting_yday)) {
        stop("Emptyings must lie between 'sowing_yday' and 'harvesting_yday'.")
      }

      assert_positive_number(flow_height_cm)

      assert_positive_integer(perellona_start_yday)
      assert_positive_integer(perellona_end_yday)
      stopifnot(perellona_end_yday < sowing_yday)
      stopifnot(perellona_start_yday > harvesting_yday)

      assert_positive_number(perellona_height_cm)
    },
    error = function(e) {
      class(e) <- c("erahumed_wms_error", class(e))
      stop(e)
    })

  n_days <- 366
  flow_days <- sowing_yday:harvesting_yday
  perellona_days <- c(1:perellona_end_yday, perellona_start_yday:n_days)

  height_regular <- numeric(n_days)
  height_regular[flow_days] <- flow_height_cm
  height_regular[c(sowing_yday, emptyings_yday, harvesting_yday)] <- 0

  height_tancat <- height_regular
  height_tancat[perellona_days] <- perellona_height_cm

  height_regular <- smoother_stepwise(height_regular)
  height_tancat <- smoother_stepwise(height_tancat)

  days_dummy <- seq(from = as.Date("2000-01-01"), to = "2000-12-31", by = "day")

  df_regular <- data.frame(
    mm = get_mm(days_dummy),
    dd = get_dd(days_dummy),
    yday = seq_along(days_dummy),
    sowing = yday == sowing_yday,
    harvesting = yday == harvesting_yday,
    seed_day = seq_along(days_dummy) - sowing_yday,
    ideal_height_eod_cm = height_regular,
    ideal_irrigation = ideal_irrigation(height_regular),
    ideal_draining = ideal_draining(height_regular),
    tancat = FALSE
  )

  df_tancat <- data.frame(
    mm = get_mm(days_dummy),
    dd = get_dd(days_dummy),
    yday = seq_along(days_dummy),
    sowing = yday == sowing_yday,
    harvesting = yday == harvesting_yday,
    seed_day = seq_along(days_dummy) - sowing_yday,
    ideal_height_eod_cm = height_tancat,
    ideal_irrigation = ideal_irrigation(height_tancat),
    ideal_draining = ideal_draining(height_tancat),
    tancat = TRUE
  )
}

smoother_stepwise <- function(x) {
  n <- length(x)

  diffs <- c(x[1] - x[n], diff(x))

  for (i in which(diffs != 0)) {
    j <- if (x[i] != 0) { i } else if (i > 1) { i - 1 } else { n }
    x[j] <- x[j] / 2
  }

  return(x)
}

ideal_irrigation <- function(x) {
  x > 0 & (c(x[1] - x[length(x)], diff(x)) >= 0)
}

ideal_draining <- function(x) {
  diffs <- c(x[1] - x[length(x)], diff(x))
  diffs < 0 | diffs == 0 & x > 0
}
