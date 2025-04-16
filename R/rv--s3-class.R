rice_variety <- function(water_management_scheme = water_management_scheme(),
                         # TODO el ... no sirve pa na si no exporto tmb un constructor de "chemical_application()",
                         # pero la idea era solo exportar el "add_application" sin hacer explicita la clase...
                         ...)
{
  tryCatch(
    {
      assert_water_management_scheme(water_management_scheme)
      for (x in list(...)) {
        assert_chemical_application(x)
      }
    },
    error = function(e) {
      class(e) <- c("erahumed_rv_error", class(e))
      stop(e)
    })

  res <- list(water_management_scheme = water_management_scheme,
              applications = list()
              )
  class(res) <- "erahumed_rice_variety"

  for (application in list(...)) {
    res <- add_application0(res, application)
  }



  return(res)
}

add_application <- function(
    rice_variety,
    chemical,
    amount_kg_ha,
    seed_day,
    type = c("ground", "aerial")
    )
{
  assert_rice_variety(rice_variety)

  application <- chemical_application(chemical = chemical,
                                      amount_kg_ha = amount_kg_ha,
                                      seed_day = seed_day,
                                      type = type
                                      )

  add_application0(rice_variety, application)

  return(rice_variety)
}

add_application0 <- function(rice_variety, application) {
  assert_rice_variety(rice_variety)
  assert_chemical_application(application)
  check_compatible_application(rice_variety, application)
  rice_variety$applications <- c(rice_variety$applications, application)
  return(rice_variety)
}

chemical_application <- function(chemical,
                                 amount_kg_ha,
                                 seed_day,
                                 type = c("ground", "aerial")
                                 )
{
  tryCatch(
    {
      assert_chemical(chemical)

      assert_positive_number(amount_kg_ha)

      assert_numeric_vector(seed_day)
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
              type = type)
  class(res) <- "erahumed_chemical_application"
}
