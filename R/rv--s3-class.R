chemical_application_scheme <- function(...) {
  # Check that every

  res <- list(...)

  class(res) <- "erahumed_ca_scheme"

  return(res)
}

add_application <- function(
    chemical_application_scheme,
    chemical,
    amount_kg_ha,
    day,
    type = c("ground", "aerial"),
    variety = c("J.Sendra", "Bomba", "Clearfield")
    )
{
  tryCatch(
    {
      # TODO: assert chemical_application_scheme
      # TODO: assert chemical

      assert_positive_number(amount_kg_ha)

      assert_numeric_vector(day)
      assert_length_one(day)

      if (missing(type))
        stop("Please specify application type")
      type <- match.arg(type)

      if (missing(variety))
        stop("Please specify application variety")
      variety <- match.arg(variety)

      # TODO: check availability of day for specified application type and variety
      # Requires an argument with the management plan?
      # Or maybe the chemical application scheme could be joined together with the rice variety?
      },
    error = function(e) {
      class(e) <- c("erahumed_chemical_application_error", class(e))
      stop(e)
    })

}
