#' @rdname rfms
#' @export
jsendra <- function() {
  new_rfms(display_name = "J.Sendra") |>
    schedule_application(chemical = acetamiprid(),
                         seed_day = 52,
                         amount_kg_ha = 0.03,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = bentazone(),
                         seed_day = 52,
                         amount_kg_ha = 1,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = mcpa(),
                         seed_day = 52,
                         amount_kg_ha = 0.50,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = penoxsulam(),
                         seed_day = 18,
                         amount_kg_ha = 0.04,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = cyhalofop_butyl(),
                         seed_day = 5,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = cyhalofop_butyl(),
                         seed_day = 18,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 76,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 90,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 76,
                         amount_kg_ha = 0.13,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 90,
                         amount_kg_ha = 0.13,
                         type = "aerial")
}

#' @rdname rfms
#' @export
bomba <- function() {
  new_rfms(display_name = "Bomba") |>
    schedule_application(chemical = acetamiprid(),
                         seed_day = 52,
                         amount_kg_ha = 0.03,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = bentazone(),
                         seed_day = 52,
                         amount_kg_ha = 1,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = mcpa(),
                         seed_day = 52,
                         amount_kg_ha = 0.50,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = penoxsulam(),
                         seed_day = 18,
                         amount_kg_ha = 0.04,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = cyhalofop_butyl(),
                         seed_day = 5,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = cyhalofop_butyl(),
                         seed_day = 18,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 76,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 90,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 76,
                         amount_kg_ha = 0.13,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 90,
                         amount_kg_ha = 0.13,
                         type = "aerial") |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 104,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 104,
                         amount_kg_ha = 0.13,
                         type = "aerial")
}

#' @rdname rfms
#' @export
clearfield <- function() {
  new_rfms(display_name = "Clearfield") |>
    schedule_application(chemical = acetamiprid(),
                         seed_day = 52,
                         amount_kg_ha = 0.03,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = cycloxydim(),
                         seed_day = 18,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 2) |>
    schedule_application(chemical = cycloxydim(),
                         seed_day = 52,
                         amount_kg_ha = 0.30,
                         type = "ground",
                         emptying_days = 6) |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 76,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = azoxystrobin(),
                         seed_day = 90,
                         amount_kg_ha = 0.20,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 76,
                         amount_kg_ha = 0.13,
                         type = "aerial") |>
    schedule_application(chemical = difenoconazole(),
                         seed_day = 90,
                         amount_kg_ha = 0.13,
                         type = "aerial")
}
