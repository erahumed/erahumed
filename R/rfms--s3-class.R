#' Rice Field Management System
#'
#' Functions to define and initialize rice field management systems.
#'
#' - `new_rfms()` creates an empty management system, without any scheduled
#' chemical applications.
#' - `jsendra_rfms()`, `bomba_rfms()`, and `clearfield_rfms()` provide
#' predefined systems inspired by the *J. Sendra*, *Bomba*, and *Clearfield*
#' rice varieties, respectively.
#'
#' Additional chemical applications can be scheduled using the helper function
#' [add_application()].
#'
#' @return An object of class `erahumed_rfms`.
#'
#' @seealso [add_application]
#'
#' @name rfms
#'
#' @export
new_rfms <- function()
{
  res <- list(applications = list())
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
#'
#' @return An object of class [erahumed_rfms][rfms].
#'
#' @seealso [rfms]
#'
#' @examples
#' rfms <- new_rfms()
#' rfms <- add_application(rfms, chemical = "penoxsulam", amount_kg_ha = 0.025, seed_day = 10, type = "ground")
#'
#' @export
add_application <- function(
    rfms,
    chemical,
    amount_kg_ha,
    seed_day,
    type = c("ground", "aerial")
    )
{
  tryCatch(
    {
      assert_rfms(rfms)
      application <- chemical_application(chemical = chemical,
                                          amount_kg_ha = amount_kg_ha,
                                          seed_day = seed_day,
                                          type = type)
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
                                 type = c("ground", "aerial")
)
{
  tryCatch(
    {
      assert_erahumed_chemical(chemical)
      assert_positive_number(amount_kg_ha)
      assert_positive_integer(seed_day)
      assert_length_one(seed_day)

      # TODO: some logic here to check that 'seed_day' is valid, e.g. it is
      # not "yday 400".

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
              type = type)
  class(res) <- "erahumed_chemical_application"
}
