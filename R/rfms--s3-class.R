#' Rice Field Management Systems
#'
#' Functions to define and initialize rice field management systems.
#'
#' @param sowing_yday `[integer(1)]` \cr
#' Day of the year marking the start of the sowing season (1–366, assuming a leap year).
#' @param harvesting_yday `[integer(1)]` \cr
#' Day of the year marking the end of the sowing season (1–366, assuming a leap year).
#' @param perellona_start_yday `[integer(1)]` \cr
#' Day of the year marking the beginning of the *Perelloná* flooding period (after harvest).
#' @param perellona_end_yday `[integer(1)]` \cr
#' Day of the year marking the end of the *Perelloná* flooding period (before sowing).
#' @param flow_height_cm `[numeric(1)]` \cr
#' Target water level (in cm) during the regular days of the sowing season,
#' excluding emptying and transition days.
#' @param perellona_height_cm `[numeric(1)]` \cr
#' Target water level (in cm) during the *Perelloná* flooding period.
#'
#' @return An object of class `erahumed_rfms`.
#'
#' @details These functions are used to define and initialize rice field
#' management systems. Specifically:
#'
#' * `new_rfms()` creates an empty management system, with no scheduled
#'   chemical applications.
#' * `jsendra_rfms()`, `bomba_rfms()`, and `clearfield_rfms()` provide
#'   predefined systems inspired by the *J. Sendra*, *Bomba*, and *Clearfield*
#'   rice varieties, respectively.
#'
#' Additional chemical applications can be scheduled using the helper function
#' [add_application()]. The default values of the arguments of `new_rfms()`
#' coincide with those used internally by `jsendra_rfms()`, `bomba_rfms()`, and
#' `clearfield_rfms()`.
#'
#' @seealso [add_application]
#'
#' @name rfms
#'
#' @export
new_rfms <- function(
    sowing_yday = 111,
    harvesting_yday = 251,
    perellona_end_yday = 15,
    perellona_start_yday = 306,
    flow_height_cm = 10,
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
      assert_positive_integer(perellona_start_yday)
      assert_positive_integer(perellona_end_yday)
      stopifnot(perellona_end_yday < sowing_yday)
      stopifnot(perellona_start_yday > harvesting_yday)
      assert_positive_number(flow_height_cm)
      assert_positive_number(perellona_height_cm)
    },
    error = function(e) {
      class(e) <- c("erahumed_rfms_error", class(e))
      stop(e)
    })

  res <- list(sowing_yday = sowing_yday,
              harvesting_yday = harvesting_yday,
              perellona_end_yday = perellona_end_yday,
              perellona_start_yday = perellona_start_yday,
              flow_height_cm = flow_height_cm,
              perellona_height_cm = perellona_height_cm,
              applications = list())
  class(res) <- "erahumed_rfms"
  return(res)
}

is_rfms <- function(x) {
  inherits(x, "erahumed_rfms") &&
    is.list(x) &&
    hasName(x, "applications")
}

#' Add a Chemical Application to a Rice Field Management System
#'
#' Schedules a chemical application within an existing rice field management
#' system (see [rfms]).
#'
#' @param rfms `[`[erahumed_rfms][rfms]`]` \cr
#'   A rice field management system object created with one of the `*_rfms()`
#'   functions documented in [rfms].
#' @param chemical `[`[erahumed_chemical][chemical]`]` \cr
#'   Chemical to be applied.
#' @param amount_kg_ha `[numeric(1)]` \cr
#'   Application rate in kilograms per hectare.
#' @param seed_day `[integer(1)]` \cr
#'   Day after seeding when the application occurs.
#' @param type `[character(1)]` \cr
#'   Application type, either `"ground"` or `"aerial"`.
#' @param emptying_days `[numeric(1)]` \cr
#'   Duration (in days) that the field remains empty after a `"ground"`
#'   application. Ignored if `type` is `"aerial"`.
#'
#' @return An object of class [erahumed_rfms][rfms].
#'
#' @seealso [rfms]
#'
#' @export
add_application <- function(
    rfms,
    chemical,
    amount_kg_ha,
    seed_day,
    type = c("ground", "aerial"),
    emptying_days = 1
    )
{
  tryCatch(
    {
      assert_rfms(rfms)

      assert_positive_integer(seed_day)
      assert_length_one(seed_day)
      if (seed_day > (rfms$harvesting_yday - rfms$sowing_yday))
        stop("Specified 'seed_day' is outside of the sowing window.")

      assert_positive_integer(emptying_days)
      assert_length_one(emptying_days)


      application <- chemical_application(chemical = chemical,
                                          amount_kg_ha = amount_kg_ha,
                                          seed_day = seed_day,
                                          type = type,
                                          emptying_days = emptying_days)
    },
    error = function(e) {
      class(e) <- c("erahumed_application_error", class(e))
      stop(e)
    })

  .add_application(rfms, application)
}

.add_application <- function(rfms, application) {
  rfms[["applications"]] <- c(rfms[["applications"]], list(application))
  return(rfms)
}

chemical_application <- function(chemical,
                                 amount_kg_ha,
                                 seed_day,
                                 type = c("ground", "aerial"),
                                 emptying_days = emptying_days
)
{
  tryCatch(
    {
      assert_erahumed_chemical(chemical)
      assert_positive_number(amount_kg_ha)
      assert_positive_integer(seed_day)
      assert_length_one(seed_day)
      if (missing(type))
        stop("Please specify application type")
      type <- match.arg(type)

    },
    error = function(e) {
      class(e) <- c("erahumed_chemical_application_error", class(e))
      stop(e)
    })

  res <- list(chemical = chemical,
              amount_kg_ha = amount_kg_ha,
              seed_day = seed_day,
              type = type,
              emptying_days = emptying_days)
  class(res) <- "erahumed_chemical_application"

  return(res)
}

#' @export
print.erahumed_rfms <- function(x, ...) {
  cat("<Rice Field Management System>\n")
  cat("  Sowing period     : Day", x$sowing_yday, "to", x$harvesting_yday, "\n")
  cat("  Perelloná period  : Day", x$perellona_start_yday, "to", x$perellona_end_yday, "\n")
  cat("  Flow height       :", x$flow_height_cm, "cm during sowing season\n")
  cat("  Perelloná height  :", x$perellona_height_cm, "cm during Perelloná\n")
  cat("  Applications      :", length(x$applications), "chemical application(s)\n")
  invisible(x)
}

#' @export
summary.erahumed_rfms <- function(object, ...) {
  n_app <- length(object$applications)

  cat("<Rice Field Management System Summary>\n")
  cat("  Sowing period      : Day", object$sowing_yday, "to", object$harvesting_yday, "\n")
  cat("  Perelloná period   : Day", object$perellona_start_yday, "to", object$perellona_end_yday, "\n")
  cat("  Flow height        :", object$flow_height_cm, "cm\n")
  cat("  Perelloná height   :", object$perellona_height_cm, "cm\n")
  cat("  Applications       :", n_app, "scheduled\n\n")

  if (n_app == 0) {
    cat("  No chemical applications defined.\n")
  } else {
    df <- do.call(rbind, lapply(object$applications, function(app) {
      data.frame(
        chemical = app$chemical$display_name,
        type = app$type,
        seed_day = app$seed_day,
        amount_kg_ha = app$amount_kg_ha,
        emptying_days = ifelse(app$type == "ground", app$emptying_days, NA_real_),
        stringsAsFactors = FALSE
      )
    }))

    print(df, row.names = FALSE)
  }

  invisible(object)
}
