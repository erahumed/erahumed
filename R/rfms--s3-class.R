#' @param sowing_yday `r get_param_roxy("sowing_yday", fun = "rfms")`
#' @param harvesting_yday `r get_param_roxy("harvesting_yday", fun = "rfms")`
#' @param perellona_start_yday `r get_param_roxy("perellona_start_yday", fun = "rfms")`
#' @param perellona_end_yday `r get_param_roxy("perellona_end_yday", fun = "rfms")`
#' @param flow_height_cm `r get_param_roxy("flow_height_cm", fun = "rfms")`
#' @param perellona_height_cm `r get_param_roxy("perellona_height_cm", fun = "rfms")`
#' @param display_name `r get_param_roxy("display_name", fun = "rfms")`
#'
#' @rdname rfms
#' @export
new_rfms <- function(
    sowing_yday = 111,
    harvesting_yday = 251,
    perellona_end_yday = 15,
    perellona_start_yday = 306,
    flow_height_cm = 10,
    perellona_height_cm = 20,
    display_name = "New Management System"
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
      assert_string(display_name)
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
              display_name = display_name,
              applications = list())
  class(res) <- "erahumed_rfms"
  return(res)
}

is_rfms <- function(x) {
  inherits(x, "erahumed_rfms") &&
    is.list(x) &&
    utils::hasName(x, "applications")
}

#' Schedule chemical application
#'
#' Schedules a chemical application within an existing rice field management
#' system (see [rfms]).
#'
#' @param system `[`[erahumed_rfms][rfms]`]` \cr
#'   A rice field management system object.
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
schedule_application <- function(
    system,
    chemical,
    amount_kg_ha,
    seed_day,
    type = c("ground", "aerial"),
    emptying_days = 1
)
{
  tryCatch(
    {
      assert_rfms(system)

      assert_positive_integer(seed_day)
      assert_length_one(seed_day)
      if (seed_day == 0)
        stop("'seed_day' must be greater than 0.")
      if (seed_day > (system$harvesting_yday - system$sowing_yday))
        stop("Specified 'seed_day' is outside of the sowing window.")

      assert_positive_integer(emptying_days)
      assert_length_one(emptying_days)
      if (emptying_days == 0)
        stop("'emptying_days' must be greater than 0.")
      if (emptying_days > seed_day)
        stop("'emptying_days' cannot be greater than 'seed_day'.")

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

  .add_application(system, application)
}

.add_application <- function(system, application) {
  system[["applications"]] <- c(system[["applications"]], list(application))
  return(system)
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
  cat("  Name               : ", x$display_name)
  cat("  Sowing period     : Day", x$sowing_yday, "to", x$harvesting_yday, "\n")
  cat("  Perellon\u{00E1} period  : Day", x$perellona_start_yday, "to", x$perellona_end_yday, "\n")
  cat("  Flow height       :", x$flow_height_cm, "cm during sowing season\n")
  cat("  Perellon\u{00E1} height  :", x$perellona_height_cm, "cm during Perellon\u{00E1}\n")
  cat("  Applications      :", length(x$applications), "chemical application(s)\n")
  invisible(x)
}

#' @export
summary.erahumed_rfms <- function(object, ...) {
  n_app <- length(object$applications)

  cat("<Rice Field Management System Summary>\n")
  cat("  Name               : ", object$display_name)
  cat("  Sowing period      : Day", object$sowing_yday, "to", object$harvesting_yday, "\n")
  cat("  Perellon\u{00E1} period   : Day", object$perellona_start_yday, "to", object$perellona_end_yday, "\n")
  cat("  Flow height        :", object$flow_height_cm, "cm\n")
  cat("  Perellon\u{00E1} height   :", object$perellona_height_cm, "cm\n")
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
