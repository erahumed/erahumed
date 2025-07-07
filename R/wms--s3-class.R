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
wms <- function(
    sowing_yday = 111,
    harvesting_yday = 251,
    emptyings_yday = c(116:117, 129:130, 163:168),
    flow_height_cm = 10,
    perellona_end_yday = 15,
    perellona_start_yday = 306,
    perellona_height_cm = 20
    )
{
  tryCatch(
    {
      assert_positive_integer(sowing_yday)
      stopifnot(sowing_yday < 366)
      assert_positive_integer(harvesting_yday)
      stopifnot(sowing_yday < 366)
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

  res <- wms_df(sowing_yday,
                harvesting_yday,
                emptyings_yday,
                flow_height_cm,
                perellona_end_yday,
                perellona_start_yday,
                perellona_height_cm
                )

  class(res) <- c("erahumed_wms", class(res))

  return(res)
}

is_wms <- function(x) {
  inherits(x, "erahumed_wms")
}

